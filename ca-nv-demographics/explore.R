# =============================================================================
# CALIFORNIA & NEVADA DEMOGRAPHICS — EXPLORATION
# =============================================================================
# Pulls ACS tract-level demographic data for CA and NV.
# Explores distributions and builds prototype maps using mapgl.
#
# Run this FIRST before launching the Shiny app —
# it saves cached RDS files that the app loads at startup.
#
# Run order:
#   1. This script  (explore.R)
#   2. shiny::runApp("ca-nv-demographics/shiny-demographics")
#
# Requirements:
#   - Census API key already set via census_api_key(install = TRUE)
#   - install.packages("mapgl")  version 0.4.4+
# =============================================================================

library(tidycensus)
library(tidyverse)
library(sf)
library(mapgl)
library(here)
library(scales)

# Output and cache directories
OUTPUT_DIR <- here("ca-nv-demographics", "outputs")
CACHE_DIR  <- here("ca-nv-demographics", "shiny-demographics",
                   "data", "cache")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(CACHE_DIR,  recursive = TRUE, showWarnings = FALSE)

cat("Output dir:", OUTPUT_DIR, "\n")
cat("Cache dir: ", CACHE_DIR,  "\n")

# =============================================================================
# VARIABLES TO PULL
# =============================================================================
# All from ACS 5-year Data Profiles (DP tables) — tract level.
# Named vector: names become column names after pivot_wider.

vars <- c(
  bach_plus      = "DP02_0068P",  # % bachelor's degree or higher
  median_income  = "DP03_0062",   # median household income ($)
  poverty_rate   = "DP03_0119P",  # % below poverty line
  unemployment   = "DP03_0009P",  # % unemployed (civilian labor force)
  median_rent    = "DP04_0134",   # median gross rent ($)
  owner_occupied = "DP04_0046P",  # % owner-occupied units
  vacancy_rate   = "DP04_0003P",  # % vacant housing units
  median_age     = "DP05_0018",   # median age
  foreign_born   = "DP02_0093P",  # % foreign born
  diff_house     = "DP02_0087P"   # % in different house 1 year ago
)

# =============================================================================
# HELPER: ADD DERIVED VARIABLES
# =============================================================================
# Calculates rent burden and income tier from base variables.
# Also extracts county and tract name from the NAME column.
#
# ACS tract NAME format: "Census Tract X; County Name; State"
# NOTE: uses semicolons NOT commas — earlier version used comma
# regex which returned NA for all county values. Fixed here.

add_derived_vars <- function(df) {
  df |>
    dplyr::mutate(
      
      # FIX: extract county using semicolon delimiter
      # "Census Tract 51.27; San Joaquin County; California"
      #  → "San Joaquin County"
      county = stringr::str_extract(NAME, "(?<=; ).*(?=; )"),
      
      # Extract clean tract label (everything before first semicolon)
      # "Census Tract 51.27; ..." → "Census Tract 51.27"
      tract_name = stringr::str_trim(
        stringr::str_extract(NAME, "^[^;]+")
      ),
      
      # Rent burden: annual rent as % of annual income
      # Standard thresholds: >30% = cost burdened, >50% = severely burdened
      rent_burden = dplyr::case_when(
        !is.na(estimate_median_rent) &
          !is.na(estimate_median_income) &
          estimate_median_income > 0 ~
          round(
            (estimate_median_rent * 12 / estimate_median_income) * 100,
            1
          ),
        TRUE ~ NA_real_
      ),
      
      # Income tier — useful for categorical mapping
      income_tier = dplyr::case_when(
        is.na(estimate_median_income)   ~ NA_character_,
        estimate_median_income < 40000  ~ "Under $40k",
        estimate_median_income < 60000  ~ "$40k\u2013$60k",
        estimate_median_income < 80000  ~ "$60k\u2013$80k",
        estimate_median_income < 100000 ~ "$80k\u2013$100k",
        estimate_median_income < 150000 ~ "$100k\u2013$150k",
        TRUE                            ~ "$150k+"
      ),
      income_tier = factor(income_tier, levels = c(
        "Under $40k",
        "$40k\u2013$60k",
        "$60k\u2013$80k",
        "$80k\u2013$100k",
        "$100k\u2013$150k",
        "$150k+"
      ))
    )
}


# =============================================================================
# SECTION 1: PULL DATA — CALIFORNIA
# =============================================================================

cat("\nPulling California tract data (may take 30-60 seconds)...\n")

ca_long <- get_acs(
  geography = "tract",
  variables = vars,
  state     = "CA",
  year      = 2022,
  geometry  = TRUE
)

# Pivot to wide — one row per tract, one column per variable
ca_tracts <- ca_long |>
  dplyr::select(GEOID, NAME, variable, estimate, moe, geometry) |>
  tidyr::pivot_wider(
    names_from  = variable,
    values_from = c(estimate, moe)
  ) |>
  dplyr::mutate(state = "California") |>
  add_derived_vars()

cat("California tracts pulled:", nrow(ca_tracts), "\n")

# Verify county extraction worked
cat("Sample county values:\n")
ca_tracts |>
  sf::st_drop_geometry() |>
  dplyr::select(NAME, county, tract_name) |>
  head(3) |>
  print()


# =============================================================================
# SECTION 2: PULL DATA — NEVADA
# =============================================================================

cat("\nPulling Nevada tract data...\n")

nv_long <- get_acs(
  geography = "tract",
  variables = vars,
  state     = "NV",
  year      = 2022,
  geometry  = TRUE
)

nv_tracts <- nv_long |>
  dplyr::select(GEOID, NAME, variable, estimate, moe, geometry) |>
  tidyr::pivot_wider(
    names_from  = variable,
    values_from = c(estimate, moe)
  ) |>
  dplyr::mutate(state = "Nevada") |>
  add_derived_vars()

cat("Nevada tracts pulled:", nrow(nv_tracts), "\n")

# Combined dataset
ca_nv_tracts <- dplyr::bind_rows(ca_tracts, nv_tracts)
cat("Total tracts (CA + NV):", nrow(ca_nv_tracts), "\n")


# =============================================================================
# SECTION 3: VALIDATE THE DATA
# =============================================================================

cat("\n=== DATA VALIDATION ===\n")

# Check county extraction
cat("\nCounty NA rate:\n")
cat("  CA:", sum(is.na(ca_tracts$county)), "NA of", nrow(ca_tracts), "\n")
cat("  NV:", sum(is.na(nv_tracts$county)), "NA of", nrow(nv_tracts), "\n")

# Check key variables
cat("\nCA key variable summary:\n")
ca_tracts |>
  sf::st_drop_geometry() |>
  dplyr::summarise(
    bach_median    = median(estimate_bach_plus,    na.rm = TRUE),
    income_median  = median(estimate_median_income, na.rm = TRUE),
    rent_median    = median(estimate_median_rent,   na.rm = TRUE),
    burden_median  = median(rent_burden,            na.rm = TRUE),
    burden_na      = sum(is.na(rent_burden))
  ) |>
  print()

cat("\nNV key variable summary:\n")
nv_tracts |>
  sf::st_drop_geometry() |>
  dplyr::summarise(
    bach_median    = median(estimate_bach_plus,    na.rm = TRUE),
    income_median  = median(estimate_median_income, na.rm = TRUE),
    rent_median    = median(estimate_median_rent,   na.rm = TRUE),
    burden_median  = median(rent_burden,            na.rm = TRUE),
    burden_na      = sum(is.na(rent_burden))
  ) |>
  print()


# =============================================================================
# SECTION 4: CA vs NV COMPARISON
# =============================================================================

cat("\n=== CA vs NV MEDIAN COMPARISON ===\n")

bind_rows(
  ca_tracts |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      across(
        c(starts_with("estimate_"), rent_burden),
        ~median(., na.rm = TRUE)
      )
    ) |>
    dplyr::mutate(state = "California"),
  nv_tracts |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      across(
        c(starts_with("estimate_"), rent_burden),
        ~median(., na.rm = TRUE)
      )
    ) |>
    dplyr::mutate(state = "Nevada")
) |>
  tidyr::pivot_longer(-state) |>
  tidyr::pivot_wider(names_from = state, values_from = value) |>
  dplyr::mutate(
    name       = stringr::str_remove(name, "estimate_"),
    Difference = round(Nevada - California, 1),
    Pct_Diff   = round((Nevada - California) / abs(California) * 100, 1)
  ) |>
  dplyr::arrange(desc(abs(Difference))) |>
  dplyr::rename(Variable = name) |>
  dplyr::mutate(
    California = round(California, 1),
    Nevada     = round(Nevada, 1)
  ) |>
  print(n = 15)


# =============================================================================
# SECTION 5: SAVE CACHE FILES
# =============================================================================

cat("\nSaving cache files...\n")

saveRDS(ca_tracts,    file.path(CACHE_DIR, "ca_tracts_2022.rds"))
saveRDS(nv_tracts,    file.path(CACHE_DIR, "nv_tracts_2022.rds"))
saveRDS(ca_nv_tracts, file.path(CACHE_DIR, "ca_nv_tracts_2022.rds"))

cat("Saved:\n")
cat("  ca_tracts_2022.rds    —", nrow(ca_tracts), "rows\n")
cat("  nv_tracts_2022.rds    —", nrow(nv_tracts), "rows\n")
cat("  ca_nv_tracts_2022.rds —", nrow(ca_nv_tracts), "rows\n")


# =============================================================================
# SECTION 6: PROTOTYPE MAPS
# =============================================================================
# Run these interactively to preview before launching the Shiny app.
# Each map has an interactive legend — drag the handles to filter by range.

cat("\nBuilding prototype maps...\n")

# Helper to build a quick prototype map
quick_map <- function(data, col, label,
                      palette = viridisLite::viridis,
                      cap_at  = NULL) {
  
  # Build tooltip
  d <- data |>
    dplyr::mutate(
      .val = .data[[col]],
      .val = if (!is.null(cap_at)) pmin(.val, cap_at) else .val,
      tooltip_text = dplyr::case_when(
        is.na(.val) ~ "No data",
        TRUE ~ paste0(
          "<strong>", tract_name, "</strong><br>",
          county, ", ", state, "<br>",
          label, ": <strong>",
          round(.val, 1),
          "</strong>"
        )
      )
    )
  
  scale <- interpolate_palette(
    data    = d |> dplyr::filter(!is.na(.val)),
    column  = ".val",
    method  = "quantile",
    n       = 5,
    palette = palette
  )
  
  maplibre(bounds = d, style = carto_style("dark-matter")) |>
    add_fill_layer(
      id                 = "layer",
      source             = d,
      fill_color         = scale$expression,
      fill_opacity       = 0.75,
      fill_outline_color = "rgba(0,0,0,0.05)",
      tooltip            = "tooltip_text"
    ) |>
    add_legend(
      label,
      values        = scale$breaks,
      colors        = scale$colors,
      type          = "continuous",
      layer_id      = "layer",
      interactive   = TRUE,
      filter_column = ".val",
      position      = "bottom-left"
    ) |>
    add_screenshot_control(position = "top-right", image_scale = 2)
}

# --- Map 1: Education ---
cat("Map 1: Education...\n")
map_education <- quick_map(
  ca_nv_tracts,
  col     = "estimate_bach_plus",
  label   = "Bachelor's Degree or Higher (%)",
  palette = viridisLite::viridis
)
map_education

# --- Map 2: Median income ---
cat("Map 2: Median income...\n")
map_income <- quick_map(
  ca_nv_tracts,
  col     = "estimate_median_income",
  label   = "Median Household Income ($)",
  palette = viridisLite::viridis
)
map_income

# --- Map 3: Rent burden ---
cat("Map 3: Rent burden...\n")
map_burden <- quick_map(
  ca_nv_tracts,
  col     = "rent_burden",
  label   = "Rent Burden (Annual Rent as % of Income)",
  palette = viridisLite::inferno,
  cap_at  = 150    # cap extreme outliers for display
)
map_burden

# --- Map 4: Recent movers ---
cat("Map 4: Recent movers...\n")
map_movers <- quick_map(
  ca_nv_tracts,
  col     = "estimate_diff_house",
  label   = "Residents in Different House 1 Year Ago (%)",
  palette = viridisLite::plasma
)
map_movers

# --- Map 5: Poverty rate ---
cat("Map 5: Poverty rate...\n")
map_poverty <- quick_map(
  ca_nv_tracts,
  col     = "estimate_poverty_rate",
  label   = "Poverty Rate (%)",
  palette = viridisLite::inferno
)
map_poverty


# =============================================================================
# SECTION 7: QUICK STATS STORIES
# =============================================================================

cat("\n=== INTERESTING FINDINGS ===\n")

# Rent burden distribution
cat("\nRent burden categories — CA vs NV:\n")
ca_nv_tracts |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(rent_burden), rent_burden <= 120) |>
  dplyr::mutate(
    burden_cat = dplyr::case_when(
      rent_burden <  30 ~ "Not burdened (<30%)",
      rent_burden <  50 ~ "Cost burdened (30-50%)",
      TRUE               ~ "Severely burdened (>50%)"
    )
  ) |>
  dplyr::count(state, burden_cat) |>
  dplyr::group_by(state) |>
  dplyr::mutate(pct = scales::percent(n / sum(n), accuracy = 0.1)) |>
  dplyr::select(state, burden_cat, n, pct) |>
  print()

# Counties with highest/lowest education in CA
cat("\nTop 5 CA counties by median tract education:\n")
ca_tracts |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(county), !is.na(estimate_bach_plus)) |>
  dplyr::group_by(county) |>
  dplyr::summarise(
    median_bach = round(median(estimate_bach_plus, na.rm = TRUE), 1),
    tracts      = dplyr::n(),
    .groups     = "drop"
  ) |>
  dplyr::arrange(desc(median_bach)) |>
  head(5) |>
  print()

cat("\nBottom 5 CA counties by median tract education:\n")
ca_tracts |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(county), !is.na(estimate_bach_plus)) |>
  dplyr::group_by(county) |>
  dplyr::summarise(
    median_bach = round(median(estimate_bach_plus, na.rm = TRUE), 1),
    tracts      = dplyr::n(),
    .groups     = "drop"
  ) |>
  dplyr::arrange(median_bach) |>
  head(5) |>
  print()

# Nevada county breakdown
cat("\nNevada counties by median income:\n")
nv_tracts |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(county), !is.na(estimate_median_income)) |>
  dplyr::group_by(county) |>
  dplyr::summarise(
    median_income = round(median(estimate_median_income, na.rm = TRUE)),
    tracts        = dplyr::n(),
    .groups       = "drop"
  ) |>
  dplyr::arrange(desc(median_income)) |>
  dplyr::mutate(
    median_income = paste0("$", format(median_income, big.mark = ","))
  ) |>
  print()


# =============================================================================
# DONE
# =============================================================================

cat("\n=== EXPLORATION COMPLETE ===\n")
cat("Cache files saved to:", CACHE_DIR, "\n")
cat("\nNext step — launch the Shiny app:\n")
cat("  shiny::runApp('ca-nv-demographics/shiny-demographics')\n")

# =============================================================================
# END OF SCRIPT
# =============================================================================