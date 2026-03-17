# =============================================================================
# DATA FETCHING + CACHING
# =============================================================================
# Wraps get_flows() with a file-based cache so repeated lookups
# are instant instead of hitting the API every time.

get_migration_cached <- function(state, county, fips, year = 2020) {
  
  # Build a cache filename from the inputs
  cache_file <- here("data", "cache",
                     paste0(tolower(state), "_",
                            tolower(str_replace_all(county, " ", "_")),
                            "_", year, ".rds"))
  
  # Return cached version if it exists
  if (file.exists(cache_file)) {
    cat("Loading from cache:", basename(cache_file), "\n")
    return(readRDS(cache_file))
  }
  
  cat("Pulling from Census API:", county, state, year, "\n")
  
  # Pull from API
  raw <- get_flows(
    geography = "county",
    state     = state,
    county    = county,
    year      = year,
    geometry  = TRUE
  )
  
  # Extract coordinates before dropping geometry
  flows_clean <- raw |>
    mutate(
      tuol_lon  = st_coordinates(centroid1)[, 1],
      tuol_lat  = st_coordinates(centroid1)[, 2],
      other_lon = st_coordinates(centroid2)[, 1],
      other_lat = st_coordinates(centroid2)[, 2]
    ) |>
    as.data.frame() |>
    select(-any_of(c("centroid1", "centroid2", "geometry"))) |>
    filter(!is.na(GEOID2)) |>
    filter(!is.na(other_lon)) |>
    filter(!is.na(other_lat)) |>
    filter(!is.na(estimate)) |>
    filter(estimate > 0) |>
    filter(GEOID2 != fips)
  
  # Split
  inflow_df <- flows_clean |>
    filter(variable == "MOVEDIN") |>
    arrange(desc(estimate))
  
  outflow_df <- flows_clean |>
    filter(variable == "MOVEDOUT") |>
    arrange(desc(estimate))
  
  # County info
  county_lon <- flows_clean$tuol_lon[1]
  county_lat <- flows_clean$tuol_lat[1]
  
  county_name <- raw |>
    pull(FULL1_NAME) |>
    first() |>
    str_remove(",.*$")
  
  # Build arc lines
  inflow_lines  <- build_arcs(inflow_df,
                              lon1 = "other_lon", lat1 = "other_lat",
                              lon2 = "tuol_lon",  lat2 = "tuol_lat")
  outflow_lines <- build_arcs(outflow_df,
                              lon1 = "tuol_lon",  lat1 = "tuol_lat",
                              lon2 = "other_lon", lat2 = "other_lat")
  
  # State name from county info
  target_state <- flows_clean |>
    filter(str_detect(FULL2_NAME,
                      paste0(state_name_from_abbr(state), "$"))) |>
    nrow()
  
  pct_in_state <- inflow_df |>
    mutate(in_state = str_detect(
      FULL2_NAME,
      state_name_from_abbr(state)
    )) |>
    summarise(
      pct = sum(estimate[in_state], na.rm = TRUE) /
        sum(estimate, na.rm = TRUE) * 100
    ) |>
    pull(pct)
  
  # Get neighbor FIPS (counties touching this one)
  # Simple approach: filter for counties in same state
  neighbor_fips <- flows_clean |>
    filter(str_detect(FULL2_NAME, state_name_from_abbr(state))) |>
    pull(GEOID2) |>
    unique()
  
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
  
  # Cache it
  saveRDS(result, cache_file)
  cat("Cached to:", basename(cache_file), "\n")
  
  result
}

# Build great circle arc sf linestrings from a dataframe
build_arcs <- function(df, lon1, lat1, lon2, lat2, n = 80) {
  if (nrow(df) == 0) return(NULL)
  
  df |>
    mutate(
      geometry = pmap(
        list(.data[[lon1]], .data[[lat1]],
             .data[[lon2]], .data[[lat2]]),
        function(x1, y1, x2, y2) {
          tryCatch({
            arc <- gcIntermediate(
              c(x1, y1), c(x2, y2),
              n = n, addStartEnd = TRUE,
              breakAtDateLine = FALSE
            )
            if (is.list(arc)) arc <- arc[[1]]
            st_linestring(arc)
          }, error = function(e) NULL)
        }
      )
    ) |>
    filter(!map_lgl(geometry, is.null)) |>
    st_as_sf(crs = 4326)
}

# Helper: convert state abbreviation to full name for string matching
state_name_from_abbr <- function(abbr) {
  tidycensus::fips_codes |>
    filter(state == abbr) |>
    pull(state_name) |>
    first()
}