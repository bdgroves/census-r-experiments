# =============================================================================
# CHART BUILDERS
# =============================================================================
# Three chart/table functions used by the Shiny app:
#   make_comparison_chart() — diverging bar: top origins vs destinations
#   make_distance_chart()   — bar chart of migrants by distance band
#   make_neighbor_table()   — in/out/net table for same-state counties
#
# All functions include null/empty guards so the app degrades gracefully
# if data is missing rather than throwing errors.

# Pre-build FIPS lookup once at source() time so make_neighbor_table()
# doesn't rebuild it on every call
.fips_lookup <- tidycensus::fips_codes |>
  mutate(full_fips = paste0(state_code, county_code)) |>
  select(full_fips, county_name = county)


# =============================================================================
# COMPARISON CHART
# =============================================================================
# Diverging horizontal bar chart showing top N counties by total exchange.
# Purple bars = people moving IN, orange bars = people moving OUT.

make_comparison_chart <- function(inflow, outflow, n = 15) {
  
  # Guard: return placeholder if no data
  if (is.null(inflow)  || nrow(inflow)  == 0 ||
      is.null(outflow) || nrow(outflow) == 0) {
    return(
      ggplot() +
        labs(title = "No flow data available") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#13132a", color = NA),
          title           = element_text(color = "#aaaaaa")
        )
    )
  }
  
  # Find top N counties by combined in + out volume
  top_fips <- bind_rows(
    inflow  |> select(GEOID2, estimate),
    outflow |> select(GEOID2, estimate)
  ) |>
    group_by(GEOID2) |>
    summarise(total = sum(estimate), .groups = "drop") |>
    arrange(desc(total)) |>
    head(n) |>
    pull(GEOID2)
  
  plot_data <- bind_rows(
    inflow  |> mutate(direction = "Moving In"),
    outflow |> mutate(direction = "Moving Out")
  ) |>
    filter(GEOID2 %in% top_fips) |>
    mutate(
      short_name = str_remove(FULL2_NAME, ",.*$") |> str_trunc(22),
      # Outflow goes left (negative), inflow goes right (positive)
      value      = ifelse(direction == "Moving Out", -estimate, estimate),
      text_label = paste0(direction, ": ",
                          format(abs(estimate), big.mark = ","))
    )
  
  ggplot(plot_data,
         aes(x    = value,
             y    = reorder(short_name, abs(value)),
             fill = direction,
             text = text_label)) +
    
    geom_col(alpha = 0.85) +
    geom_vline(xintercept = 0, color = "white", linewidth = 0.4) +
    
    scale_fill_manual(values = c(
      "Moving In"  = "#7b2d8b",
      "Moving Out" = "#cc4c02"
    )) +
    
    scale_x_continuous(
      labels = function(x) format(abs(x), big.mark = ",")
    ) +
    
    labs(
      x    = "\u2190 Moving Out     Moving In \u2192",
      y    = NULL,
      fill = NULL
    ) +
    
    theme_minimal() +
    theme(
      plot.background    = element_rect(fill = "#13132a", color = NA),
      panel.background   = element_rect(fill = "#13132a", color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#2a2a3a"),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(color = "#cccccc", size = 8),
      axis.title.x       = element_text(color = "#888888", size = 9),
      legend.position    = "bottom",
      legend.text        = element_text(color = "white",   size = 8),
      legend.background  = element_rect(fill = "#13132a", color = NA)
    )
}


# =============================================================================
# DISTANCE CHART
# =============================================================================
# Bar chart showing how far people travel to reach the selected county.
# Always shows all 5 distance bands even if some have zero migrants.

make_distance_chart <- function(inflow, county_lon, county_lat) {
  
  # Guard: missing inputs
  if (is.null(inflow) || nrow(inflow) == 0 ||
      is.null(county_lon) || is.null(county_lat) ||
      is.na(county_lon)  || is.na(county_lat)) {
    return(
      ggplot() +
        labs(title = "No distance data available") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#13132a", color = NA),
          title           = element_text(color = "#aaaaaa")
        )
    )
  }
  
  # Guard: coordinate columns must exist
  if (!all(c("other_lon", "other_lat") %in% names(inflow))) {
    return(
      ggplot() +
        labs(title = "Distance data unavailable") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#13132a", color = NA),
          title           = element_text(color = "#aaaaaa")
        )
    )
  }
  
  # All 5 bands defined upfront — guarantees complete x-axis
  band_levels <- c(
    "Local\n<100 mi",
    "Regional\n100-300 mi",
    "Mid-range\n300-600 mi",
    "Long dist.\n600-1500 mi",
    "Cross-country\n1500+ mi"
  )
  
  # Calculate haversine distance for each origin county
  with_distances <- inflow |>
    mutate(
      distance_mi = distHaversine(
        cbind(county_lon, county_lat),
        cbind(other_lon,  other_lat)
      ) / 1000 * 0.621371,
      band = case_when(
        distance_mi <  100 ~ "Local\n<100 mi",
        distance_mi <  300 ~ "Regional\n100-300 mi",
        distance_mi <  600 ~ "Mid-range\n300-600 mi",
        distance_mi < 1500 ~ "Long dist.\n600-1500 mi",
        TRUE               ~ "Cross-country\n1500+ mi"
      )
    )
  
  # Summarise migrants per band
  band_totals <- with_distances |>
    group_by(band) |>
    summarise(migrants = sum(estimate, na.rm = TRUE), .groups = "drop")
  
  # FIX: left join against complete skeleton so all 5 bars always show
  # group_by(.drop=FALSE) is unreliable across tidyverse versions
  dist_data <- tibble(band = band_levels) |>
    left_join(band_totals, by = "band") |>
    mutate(
      migrants   = replace_na(migrants, 0),
      band       = factor(band, levels = band_levels),
      pct        = ifelse(sum(migrants) > 0,
                          migrants / sum(migrants), 0),
      text_label = paste0(
        format(migrants, big.mark = ","),
        " (", percent(pct, accuracy = 0.1), ")"
      )
    )
  
  ggplot(dist_data,
         aes(x = band, y = migrants, fill = band, text = text_label)) +
    
    geom_col(alpha = 0.85, show.legend = FALSE) +
    
    scale_fill_viridis_d(option = "plasma") +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0, 0.12))
    ) +
    
    labs(x = NULL, y = "Migrants") +
    
    theme_minimal() +
    theme(
      plot.background    = element_rect(fill = "#13132a", color = NA),
      panel.background   = element_rect(fill = "#13132a", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#2a2a3a"),
      panel.grid.minor   = element_blank(),
      axis.text.x  = element_text(color = "#cccccc", size = 7,
                                  lineheight = 1.1, hjust = 0.5),
      axis.text.y  = element_text(color = "#cccccc", size = 8),
      axis.title.y = element_text(color = "#888888", size = 9),
      # Extra bottom margin so wrapped x labels aren't clipped
      plot.margin  = margin(t = 10, r = 10, b = 20, l = 10)
    )
}


# =============================================================================
# NEIGHBOR TABLE
# =============================================================================
# Returns a dataframe of in/out/net/total for same-state counties.
# Used by renderDT() in the Neighbor County Scorecard panel.

make_neighbor_table <- function(inflow, outflow, neighbor_fips) {
  
  # Guard: no neighbor FIPS provided
  if (is.null(neighbor_fips) || length(neighbor_fips) == 0) {
    return(tibble(
      County = character(),
      In     = integer(),
      Out    = integer(),
      Net    = integer(),
      Total  = integer()
    ))
  }
  
  # Guard: replace NULL/empty with empty tibbles
  if (is.null(inflow)  || nrow(inflow)  == 0) {
    inflow <- tibble(GEOID2 = character(),
                     FULL2_NAME = character(),
                     estimate   = numeric())
  }
  if (is.null(outflow) || nrow(outflow) == 0) {
    outflow <- tibble(GEOID2   = character(),
                      estimate = numeric())
  }
  
  # Filter to same-state counties in each direction
  in_data <- inflow |>
    filter(GEOID2 %in% neighbor_fips) |>
    select(GEOID2, County = FULL2_NAME, moved_in = estimate)
  
  out_data <- outflow |>
    filter(GEOID2 %in% neighbor_fips) |>
    select(GEOID2, moved_out = estimate)
  
  # Guard: no matching counties found at all
  if (nrow(in_data) == 0 && nrow(out_data) == 0) {
    return(tibble(
      County = "No same-state flows found",
      In     = NA_integer_,
      Out    = NA_integer_,
      Net    = NA_integer_,
      Total  = NA_integer_
    ))
  }
  
  # Full join so counties appearing in only one direction are included
  full_join(in_data, out_data, by = "GEOID2") |>
    mutate(
      # County name is NA for rows that only appear in outflow
      # Recover from pre-built FIPS lookup table
      County = case_when(
        !is.na(County) ~ str_remove(County, ",.*$"),
        TRUE ~ {
          name <- .fips_lookup$county_name[
            match(GEOID2, .fips_lookup$full_fips)
          ]
          ifelse(is.na(name), GEOID2, name)
        }
      ),
      In    = replace_na(as.integer(moved_in),  0L),
      Out   = replace_na(as.integer(moved_out), 0L),
      Net   = In - Out,
      Total = In + Out
    ) |>
    select(County, In, Out, Net, Total) |>
    arrange(desc(Total))
}