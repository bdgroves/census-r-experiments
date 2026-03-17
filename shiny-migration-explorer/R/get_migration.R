# =============================================================================
# DATA FETCHING + CACHING
# =============================================================================
# Wraps get_flows() with a file-based cache so repeated lookups
# are instant instead of hitting the API every time.
#
# Cache files are stored in data/cache/ relative to the app folder.
# When Shiny runs via runApp("shiny-migration-explorer"), the working
# directory is automatically set to the app folder, so file.path()
# resolves correctly without needing here().

get_migration_cached <- function(state, county, fips, year = 2020) {
  
  # ------------------------------------------------------------------
  # FIX 1: Use file.path() instead of here()
  # here() resolves from the PROJECT root, not the app folder.
  # file.path() resolves from wherever Shiny set the working directory,
  # which is the app folder — exactly where data/cache/ lives.
  # ------------------------------------------------------------------
  
  # Clean county name for safe filenames
  county_clean <- county |>
    tolower() |>
    str_replace_all(" ", "_") |>
    str_replace_all("[^a-z0-9_]", "")
  
  cache_file <- file.path(
    "data", "cache",
    paste0(tolower(state), "_", county_clean, "_", year, ".rds")
  )
  
  # ------------------------------------------------------------------
  # FIX 2: Ensure cache directory exists before trying to write to it.
  # The first time the app runs on any machine, this folder may not
  # exist yet. dir.create() with recursive = TRUE handles that safely.
  # ------------------------------------------------------------------
  cache_dir <- file.path("data", "cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    message("Created cache directory: ", normalizePath(cache_dir))
  }
  
  # Return cached version if it exists — instant load
  if (file.exists(cache_file)) {
    message("Loading from cache: ", basename(cache_file))
    return(readRDS(cache_file))
  }
  
  message("Pulling from Census API: ", county, " ", state, " ", year)
  
  # Pull from Census API
  raw <- get_flows(
    geography = "county",
    state     = state,
    county    = county,
    year      = year,
    geometry  = TRUE
  )
  
  # Extract coordinates before dropping geometry.
  # st_coordinates() must be called while centroid1/centroid2 are still
  # active sf columns — they lose their spatial context after as.data.frame()
  flows_clean <- raw |>
    mutate(
      tuol_lon  = st_coordinates(centroid1)[, 1],
      tuol_lat  = st_coordinates(centroid1)[, 2],
      other_lon = st_coordinates(centroid2)[, 1],
      other_lat = st_coordinates(centroid2)[, 2]
    ) |>
    as.data.frame() |>
    select(-any_of(c("centroid1", "centroid2", "geometry"))) |>
    filter(!is.na(GEOID2))   |>  # drop international rows (Asia, Europe etc)
    filter(!is.na(other_lon)) |>  # drop rows with no destination coords
    filter(!is.na(other_lat)) |>
    filter(!is.na(estimate))  |>  # drop suppressed estimates
    filter(estimate > 0)      |>  # drop zero-migration rows
    filter(GEOID2 != fips)        # drop self-to-self row
  
  # Split into inflow and outflow
  inflow_df <- flows_clean |>
    filter(variable == "MOVEDIN") |>
    arrange(desc(estimate))
  
  outflow_df <- flows_clean |>
    filter(variable == "MOVEDOUT") |>
    arrange(desc(estimate))
  
  # County centroid coordinates (same for every row — just grab first)
  county_lon <- flows_clean$tuol_lon[1]
  county_lat <- flows_clean$tuol_lat[1]
  
  # County display name — strip state suffix
  county_name <- raw |>
    pull(FULL1_NAME) |>
    first() |>
    str_remove(",.*$")
  
  # Build great circle arc lines for mapping
  inflow_lines <- build_arcs(
    inflow_df,
    lon1 = "other_lon", lat1 = "other_lat",
    lon2 = "tuol_lon",  lat2 = "tuol_lat"
  )
  
  outflow_lines <- build_arcs(
    outflow_df,
    lon1 = "tuol_lon",  lat1 = "tuol_lat",
    lon2 = "other_lon", lat2 = "other_lat"
  )
  
  # Full state name for string matching
  target_state_name <- state_name_from_abbr(state)
  
  # Percent of inflow from within same state
  pct_in_state <- inflow_df |>
    mutate(in_state = str_detect(FULL2_NAME, target_state_name)) |>
    summarise(
      pct = sum(estimate[in_state], na.rm = TRUE) /
        sum(estimate,           na.rm = TRUE) * 100
    ) |>
    pull(pct)
  
  # ------------------------------------------------------------------
  # FIX 3: Remove the unused `target_state` variable that was
  # computing nrow() and assigning it to nothing useful.
  # The state name is already captured in target_state_name above.
  # ------------------------------------------------------------------
  
  # Same-state counties used as the neighbor scorecard pool.
  # Simple proxy: any county in the same state that appears in the data.
  neighbor_fips <- flows_clean |>
    filter(str_detect(FULL2_NAME, target_state_name)) |>
    pull(GEOID2) |>
    unique()
  
  # Assemble the result list — everything the app needs
  result <- list(
    county_name   = county_name,
    county_lon    = county_lon,
    county_lat    = county_lat,
    fips          = fips,
    state         = state,
    year          = year,
    total_in      = sum(inflow_df$estimate,  na.rm = TRUE),
    total_out     = sum(outflow_df$estimate, na.rm = TRUE),
    pct_in_state  = pct_in_state,
    inflow        = inflow_df,
    outflow       = outflow_df,
    inflow_lines  = inflow_lines,
    outflow_lines = outflow_lines,
    flows_clean   = flows_clean,
    neighbor_fips = neighbor_fips
  )
  
  # Write to cache — next load will be instant
  saveRDS(result, cache_file)
  message("Cached: ", basename(cache_file))
  
  result
}


# =============================================================================
# BUILD GREAT CIRCLE ARC LINES
# =============================================================================
# Converts a dataframe of origin/destination coordinates into sf linestrings
# using gcIntermediate() from geosphere for proper curved great circle paths.
#
# Arguments:
#   df         dataframe containing coordinate columns
#   lon1/lat1  column names for the origin point
#   lon2/lat2  column names for the destination point
#   n          number of intermediate points (higher = smoother arc)

build_arcs <- function(df, lon1, lat1, lon2, lat2, n = 80) {
  
  # Guard against empty input
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df |>
    mutate(
      geometry = pmap(
        list(
          .data[[lon1]], .data[[lat1]],
          .data[[lon2]], .data[[lat2]]
        ),
        function(x1, y1, x2, y2) {
          tryCatch({
            arc <- gcIntermediate(
              c(x1, y1), c(x2, y2),
              n               = n,
              addStartEnd     = TRUE,
              breakAtDateLine = FALSE
            )
            # gcIntermediate returns a list when arc crosses the date line
            if (is.list(arc)) arc <- arc[[1]]
            st_linestring(arc)
          }, error = function(e) NULL)  # return NULL silently on failure
        }
      )
    ) |>
    filter(!map_lgl(geometry, is.null)) |>  # drop any failed arcs
    st_as_sf(crs = 4326)
}


# =============================================================================
# HELPER: STATE ABBREVIATION TO FULL NAME
# =============================================================================
# Used for string matching against FULL2_NAME which contains the full
# state name (e.g. "Los Angeles County, California")

state_name_from_abbr <- function(abbr) {
  tidycensus::fips_codes |>
    filter(state == abbr) |>
    pull(state_name) |>
    first()
}