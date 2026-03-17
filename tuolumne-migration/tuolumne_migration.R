# =============================================================================
# TUOLUMNE COUNTY MIGRATION FLOW MAP
# =============================================================================
# Uses ACS 5-year estimates (2016-2020) from the U.S. Census Bureau to map
# where Tuolumne County, CA residents move to and where new residents come from.
#
# Outputs (written to tuolumne-migration/outputs/):
#   - tuolumne_inflow_map.png   (static map: origins of new residents)
#   - tuolumne_outflow_map.png  (static map: destinations of departing residents)
#   - tuolumne_migration.html   (interactive leaflet map, both flows together)
#
# Requirements:
#   - Free Census API key from https://api.census.gov/data/key_signup.html
#   - See SETUP section below to enter your key
#
# Repo structure assumed:
#   census-r-experiments/
#   └── tuolumne-migration/
#       ├── tuolumne_migration.R   <- this file
#       └── outputs/               <- all outputs written here
# =============================================================================


# =============================================================================
# SECTION 1: PACKAGES
# =============================================================================

# Install any packages not already on your machine
packages_needed <- c(
  "tidycensus",  # Census API access + get_flows()
  "tidyverse",   # data wrangling (dplyr, purrr, etc.)
  "sf",          # spatial data handling
  "ggplot2",     # static maps
  "leaflet",     # interactive maps
  "geosphere",   # great circle arc calculations
  "scales",      # number formatting in legends
  "viridis",     # color palettes
  "htmlwidgets", # save leaflet map as HTML file
  "here"         # safe file paths relative to project root
)

packages_to_install <- packages_needed[
  !packages_needed %in% installed.packages()[, "Package"]
]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}

# Load all packages
library(tidycensus)
library(tidyverse)
library(sf)
library(ggplot2)
library(leaflet)
library(geosphere)
library(scales)
library(viridis)
library(htmlwidgets)
library(here)


# =============================================================================
# SECTION 2: SETUP
# =============================================================================

# --- Census API Key ---
# Get a free key at: https://api.census.gov/data/key_signup.html
# install = TRUE saves it permanently to .Renviron so you never need to set
# it again. After running this line for the first time, restart R.

# census_api_key("YOUR_KEY_HERE", install = TRUE)  # <-- run once, then comment out

# --- County of interest ---
# Change these three lines to run this script for a different county
TARGET_STATE  <- "CA"
TARGET_COUNTY <- "Tuolumne"
TARGET_FIPS   <- "06109"   # state FIPS (06) + county FIPS (109)

# --- Output folder ---
# here() always starts from the project root (where .Rproj or .git lives),
# so this path works correctly regardless of your working directory.
# The folder is created automatically if it doesn't exist yet.
OUTPUT_DIR <- here("tuolumne-migration", "outputs")

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat("Created output folder:", OUTPUT_DIR, "\n")
} else {
  cat("Output folder:", OUTPUT_DIR, "\n")
}


# =============================================================================
# SECTION 3: PULL MIGRATION DATA FROM CENSUS API
# =============================================================================

# get_flows() returns ACS county-to-county migration estimates.
# geometry = TRUE attaches two point columns:
#   centroid1 = the target county's centroid (Tuolumne)
#   centroid2 = the other county's centroid
# variable options in the result:
#   MOVEDIN  = people who moved TO Tuolumne from that county
#   MOVEDOUT = people who moved FROM Tuolumne to that county
#   MOVEDNET = net difference (MOVEDIN minus MOVEDOUT)

cat("Pulling migration data from Census API...\n")

tuolumne_flows <- get_flows(
  geography = "county",
  state     = TARGET_STATE,
  county    = TARGET_COUNTY,
  year      = 2020,       # 2020 = ACS 2016-2020 5-year estimates
  geometry  = TRUE
)

cat("Done. Rows returned:", nrow(tuolumne_flows), "\n")


# =============================================================================
# SECTION 4: EXPLORE THE RAW DATA (optional, safe to skip after first run)
# =============================================================================

# Peek at structure
glimpse(tuolumne_flows)

# Top 10 counties sending people TO Tuolumne
cat("\nTop 10 inflow origins:\n")
tuolumne_flows |>
  filter(variable == "MOVEDIN", !is.na(estimate)) |>
  arrange(desc(estimate)) |>
  select(FULL2_NAME, estimate, moe) |>
  head(10) |>
  print()

# Top 10 counties receiving people FROM Tuolumne
cat("\nTop 10 outflow destinations:\n")
tuolumne_flows |>
  filter(variable == "MOVEDOUT", !is.na(estimate)) |>
  arrange(desc(estimate)) |>
  select(FULL2_NAME, estimate, moe) |>
  head(10) |>
  print()

# Net migration by county (positive = more came in than left)
cat("\nTop 10 net migration counties:\n")
tuolumne_flows |>
  filter(variable == "MOVEDNET", !is.na(estimate)) |>
  arrange(desc(estimate)) |>
  select(FULL2_NAME, estimate) |>
  head(10) |>
  print()


# =============================================================================
# SECTION 5: CLEAN AND EXTRACT COORDINATES
# =============================================================================
# The raw data includes international/regional rows (e.g. "Asia", "Europe")
# which have no county FIPS or centroid coordinates — we drop those.
#
# IMPORTANT: st_coordinates() must be called BEFORE as.data.frame() because
# the centroid columns lose their sf context once converted to a plain dataframe.

flows_clean <- tuolumne_flows |>
  
  # Extract lon/lat from both centroid columns while still a full sf object
  mutate(
    tuol_lon  = st_coordinates(centroid1)[, 1],  # Tuolumne longitude
    tuol_lat  = st_coordinates(centroid1)[, 2],  # Tuolumne latitude
    other_lon = st_coordinates(centroid2)[, 1],  # other county longitude
    other_lat = st_coordinates(centroid2)[, 2]   # other county latitude
  ) |>
  
  # Convert to plain dataframe (removes all sf geometry structure)
  as.data.frame() |>
  
  # Drop the now-redundant geometry list columns
  select(-any_of(c("centroid1", "centroid2", "geometry"))) |>
  
  # Keep only valid US county rows (drop international rows and self-flows)
  filter(!is.na(GEOID2)) |>        # removes Asia, Europe, etc.
  filter(!is.na(other_lon)) |>     # removes rows with no destination coords
  filter(!is.na(other_lat)) |>
  filter(!is.na(estimate)) |>      # removes suppressed estimates
  filter(estimate > 0) |>          # removes zero-migration rows
  filter(GEOID2 != TARGET_FIPS)    # removes Tuolumne-to-Tuolumne row

cat("Clean rows:", nrow(flows_clean), "\n")
cat("Missing other_lon:", sum(is.na(flows_clean$other_lon)), "\n")
cat("Missing other_lat:", sum(is.na(flows_clean$other_lat)), "\n")

# Split into separate inflow and outflow tables
inflow_df <- flows_clean |>
  filter(variable == "MOVEDIN") |>
  arrange(desc(estimate))

outflow_df <- flows_clean |>
  filter(variable == "MOVEDOUT") |>
  arrange(desc(estimate))

cat("\nCounties sending people TO Tuolumne:", nrow(inflow_df), "\n")
cat("Counties receiving people FROM Tuolumne:", nrow(outflow_df), "\n")


# =============================================================================
# SECTION 6: BUILD GREAT CIRCLE ARC LINES
# =============================================================================
# gcIntermediate() from the geosphere package calculates the curved path
# (great circle arc) between two lon/lat points on the globe.
# We wrap it in a function and apply it to every row using purrr::pmap().
# The result is converted to an sf linestring layer for mapping.

make_arc <- function(lon1, lat1, lon2, lat2, n = 100) {
  # n = number of intermediate points along the arc (higher = smoother curve)
  tryCatch({
    arc <- gcIntermediate(
      p1              = c(lon1, lat1),
      p2              = c(lon2, lat2),
      n               = n,
      addStartEnd     = TRUE,   # include the exact start and end points
      breakAtDateLine = FALSE   # don't split arcs at the international date line
    )
    # gcIntermediate returns a list if the arc crosses the date line
    if (is.list(arc)) arc <- arc[[1]]
    st_linestring(arc)
  }, error = function(e) NULL)  # return NULL silently if arc fails
}

# Build inflow arcs: other county --> Tuolumne
cat("Building inflow arcs...\n")
inflow_lines <- inflow_df |>
  mutate(
    geometry = pmap(
      list(other_lon, other_lat, tuol_lon, tuol_lat),
      make_arc
    )
  ) |>
  filter(!map_lgl(geometry, is.null)) |>  # drop any failed arcs
  st_as_sf(crs = 4326)

# Build outflow arcs: Tuolumne --> other county
cat("Building outflow arcs...\n")
outflow_lines <- outflow_df |>
  mutate(
    geometry = pmap(
      list(tuol_lon, tuol_lat, other_lon, other_lat),
      make_arc
    )
  ) |>
  filter(!map_lgl(geometry, is.null)) |>
  st_as_sf(crs = 4326)

cat("Inflow arcs built:", nrow(inflow_lines), "\n")
cat("Outflow arcs built:", nrow(outflow_lines), "\n")

# Quick visual check — you should see curved lines fanning across the US
plot(st_geometry(inflow_lines), main = "Inflow Arcs — Sanity Check")


# =============================================================================
# SECTION 7: BACKGROUND MAP LAYER
# =============================================================================
# Pull US state boundaries for the map background.
# We filter out Alaska, Hawaii, and Puerto Rico to keep the
# continental US view clean.

cat("Pulling US state boundaries...\n")

us_states <- get_acs(
  geography = "state",
  variables = "B01001_001",  # total population — we only need the geometry
  year      = 2020,
  geometry  = TRUE
) |>
  st_transform(4326) |>
  filter(!NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))

# Pull Tuolumne's coordinates from the arc data for map annotations
tuol_lon <- inflow_lines$tuol_lon[1]
tuol_lat <- inflow_lines$tuol_lat[1]


# =============================================================================
# SECTION 8: SHARED THEME
# =============================================================================
# Define the dark map theme once and reuse it on both static maps
# rather than copying the same theme() block twice.

dark_flow_theme <- theme_void() +
  theme(
    plot.background  = element_rect(fill = "#0d0d1a", color = NA),
    panel.background = element_rect(fill = "#0d0d1a", color = NA),
    plot.title    = element_text(color = "white",   size = 16,
                                 face = "bold",    hjust = 0.5,
                                 margin = margin(t = 15)),
    plot.subtitle = element_text(color = "#aaaaaa", size = 11,
                                 hjust = 0.5,
                                 margin = margin(t = 5, b = 10)),
    plot.caption  = element_text(color = "#666666", size = 8,
                                 hjust = 0.5,
                                 margin = margin(b = 10)),
    legend.position = "bottom",
    legend.title    = element_text(color = "white",   size = 9),
    legend.text     = element_text(color = "#aaaaaa", size = 8),
    plot.margin     = margin(10, 10, 10, 10)
  )


# =============================================================================
# SECTION 9: STATIC MAP — INFLOW (where new residents come from)
# =============================================================================
# Line color and thickness both encode the number of migrants.
# Plasma palette: dark purple (few) --> bright yellow (many)

inflow_map <- ggplot() +
  
  # Dark background states
  geom_sf(data = us_states, fill = "#1a1a2e",
          color = "#2d2d44", linewidth = 0.3) +
  
  # Arc lines — color, thickness, and opacity all scale with migrant count
  geom_sf(data = inflow_lines,
          aes(color = estimate, linewidth = estimate, alpha = estimate)) +
  
  # Yellow dot marking Tuolumne County
  annotate("point", x = tuol_lon, y = tuol_lat,
           color = "yellow", size = 3) +
  
  # Label for Tuolumne dot
  annotate("text", x = tuol_lon + 1.2, y = tuol_lat + 0.8,
           label = "Tuolumne\nCounty, CA",
           color = "yellow", size = 2.5, hjust = 0) +
  
  scale_color_viridis_c(option = "plasma", labels = scales::comma) +
  scale_linewidth_continuous(range = c(0.3, 3.5), guide = "none") +
  scale_alpha_continuous(range    = c(0.4, 1.0),  guide = "none") +
  
  coord_sf(xlim = c(-125, -65), ylim = c(24, 50)) +
  
  labs(
    title    = "Where Tuolumne County's New Residents Come From",
    subtitle = "ACS 5-Year Estimates, 2016–2020",
    caption  = "Source: U.S. Census Bureau",
    color    = "People Moving In"
  ) +
  
  dark_flow_theme

inflow_map

# Save to outputs folder
ggsave(
  filename = here(OUTPUT_DIR, "tuolumne_inflow_map.png"),
  plot     = inflow_map,
  width    = 12,
  height   = 7,
  dpi      = 300,
  bg       = "#0d0d1a"
)
cat("Saved:", here(OUTPUT_DIR, "tuolumne_inflow_map.png"), "\n")


# =============================================================================
# SECTION 10: STATIC MAP — OUTFLOW (where departing residents go)
# =============================================================================
# Same structure as inflow map but uses the inferno palette (warm tones)
# to visually distinguish outflow from inflow at a glance.

outflow_map <- ggplot() +
  
  geom_sf(data = us_states, fill = "#1a1a2e",
          color = "#2d2d44", linewidth = 0.3) +
  
  geom_sf(data = outflow_lines,
          aes(color = estimate, linewidth = estimate, alpha = estimate)) +
  
  annotate("point", x = tuol_lon, y = tuol_lat,
           color = "yellow", size = 3) +
  
  annotate("text", x = tuol_lon + 1.2, y = tuol_lat + 0.8,
           label = "Tuolumne\nCounty, CA",
           color = "yellow", size = 2.5, hjust = 0) +
  
  scale_color_viridis_c(option = "inferno", labels = scales::comma) +
  scale_linewidth_continuous(range = c(0.3, 3.5), guide = "none") +
  scale_alpha_continuous(range    = c(0.4, 1.0),  guide = "none") +
  
  coord_sf(xlim = c(-125, -65), ylim = c(24, 50)) +
  
  labs(
    title    = "Where Tuolumne County Residents Move To",
    subtitle = "ACS 5-Year Estimates, 2016–2020",
    caption  = "Source: U.S. Census Bureau",
    color    = "People Moving Out"
  ) +
  
  dark_flow_theme

outflow_map

# Save to outputs folder
ggsave(
  filename = here(OUTPUT_DIR, "tuolumne_outflow_map.png"),
  plot     = outflow_map,
  width    = 12,
  height   = 7,
  dpi      = 300,
  bg       = "#0d0d1a"
)
cat("Saved:", here(OUTPUT_DIR, "tuolumne_outflow_map.png"), "\n")


# =============================================================================
# SECTION 11: INTERACTIVE LEAFLET MAP
# =============================================================================
# Combines inflow and outflow on one interactive map.
# Users can toggle each layer on/off and hover/click lines for details.
# Saved as a self-contained HTML file — share it or open in any browser.

inflow_pal  <- colorNumeric("Blues", domain = inflow_lines$estimate)
outflow_pal <- colorNumeric("Reds",  domain = outflow_lines$estimate)

interactive_map <- leaflet() |>
  
  # Dark basemap
  addProviderTiles(providers$CartoDB.DarkMatter) |>
  
  # Center on Tuolumne County at a zoom showing most of the western US
  setView(lng = tuol_lon, lat = tuol_lat, zoom = 5) |>
  
  # Inflow lines (blue tones)
  addPolylines(
    data    = inflow_lines,
    color   = ~inflow_pal(estimate),
    weight  = ~scales::rescale(estimate, to = c(0.5, 5)),
    opacity = ~scales::rescale(estimate, to = c(0.4, 0.9)),
    popup   = ~paste0(
      "<b>From:</b> ", FULL2_NAME, "<br>",
      "<b>Moving to Tuolumne:</b> ", format(estimate, big.mark = ",")
    ),
    label   = ~paste0(FULL2_NAME, ": ", format(estimate, big.mark = ",")),
    group   = "Moving IN (blue)"
  ) |>
  
  # Outflow lines (red tones)
  addPolylines(
    data    = outflow_lines,
    color   = ~outflow_pal(estimate),
    weight  = ~scales::rescale(estimate, to = c(0.5, 5)),
    opacity = ~scales::rescale(estimate, to = c(0.4, 0.9)),
    popup   = ~paste0(
      "<b>To:</b> ", FULL2_NAME, "<br>",
      "<b>Leaving Tuolumne:</b> ", format(estimate, big.mark = ",")
    ),
    label   = ~paste0(FULL2_NAME, ": ", format(estimate, big.mark = ",")),
    group   = "Moving OUT (red)"
  ) |>
  
  # Tuolumne County marker — pass plain numeric coords, NOT an sf object
  addCircleMarkers(
    lng         = tuol_lon,
    lat         = tuol_lat,
    radius      = 8,
    color       = "white",
    weight      = 2,
    fillColor   = "yellow",
    fillOpacity = 0.9,
    popup       = "<b>Tuolumne County, CA</b>"
  ) |>
  
  # Toggle buttons to show/hide each flow layer
  addLayersControl(
    overlayGroups = c("Moving IN (blue)", "Moving OUT (red)"),
    options       = layersControlOptions(collapsed = FALSE)
  ) |>
  
  addLegend("bottomleft",
            pal = inflow_pal, values = inflow_lines$estimate,
            title = "Moving IN", opacity = 0.8) |>
  
  addLegend("bottomright",
            pal = outflow_pal, values = outflow_lines$estimate,
            title = "Moving OUT", opacity = 0.8)

# Display in RStudio viewer pane
interactive_map

# Save as standalone HTML (no internet connection needed to open it)
# Note: saveWidget() requires an absolute path — here() provides that
htmlwidgets::saveWidget(
  interactive_map,
  file          = here(OUTPUT_DIR, "tuolumne_migration.html"),
  selfcontained = TRUE
)
cat("Saved:", here(OUTPUT_DIR, "tuolumne_migration.html"), "\n")


# =============================================================================
# SECTION 12: SUMMARY STATISTICS
# =============================================================================

total_in  <- sum(inflow_df$estimate,  na.rm = TRUE)
total_out <- sum(outflow_df$estimate, na.rm = TRUE)
net       <- total_in - total_out

cat("\n========== TUOLUMNE COUNTY MIGRATION SUMMARY (2016-2020) ==========\n")
cat("Total moving IN: ", format(total_in,  big.mark = ","), "\n")
cat("Total moving OUT:", format(total_out, big.mark = ","), "\n")
cat("Net migration:   ", format(net,       big.mark = ","),
    ifelse(net > 0, "(net gain)", "(net loss)"), "\n")

cat("\nTop 5 origins (people moving TO Tuolumne):\n")
inflow_df |>
  select(County = FULL2_NAME, `Moving In` = estimate) |>
  head(5) |>
  mutate(`Moving In` = format(`Moving In`, big.mark = ",")) |>
  print()

cat("\nTop 5 destinations (people leaving Tuolumne):\n")
outflow_df |>
  select(County = FULL2_NAME, `Moving Out` = estimate) |>
  head(5) |>
  mutate(`Moving Out` = format(`Moving Out`, big.mark = ",")) |>
  print()

cat("\nAll outputs written to:", OUTPUT_DIR, "\n")

# =============================================================================
# END OF SCRIPT
# =============================================================================