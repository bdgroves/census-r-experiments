# =============================================================================
# CHART BUILDERS
# =============================================================================

make_comparison_chart <- function(inflow, outflow, n = 15) {
  
  # Get top counties by total exchange
  top_fips <- bind_rows(
    inflow  |> select(GEOID2, estimate),
    outflow |> select(GEOID2, estimate)
  ) |>
    group_by(GEOID2) |>
    summarise(total = sum(estimate)) |>
    arrange(desc(total)) |>
    head(n) |>
    pull(GEOID2)
  
  plot_data <- bind_rows(
    inflow  |> mutate(direction = "Moving In"),
    outflow |> mutate(direction = "Moving Out")
  ) |>
    filter(GEOID2 %in% top_fips) |>
    mutate(
      short_name = str_remove(FULL2_NAME, ",.*$") |>
        str_trunc(22),
      value      = ifelse(direction == "Moving Out",
                          -estimate, estimate),
      text_label = paste0(direction, ": ",
                          format(abs(estimate), big.mark = ","))
    )
  
  ggplot(plot_data,
         aes(x    = value,
             y    = reorder(short_name, abs(value)),
             fill = direction,
             text = text_label)) +
    
    geom_col(alpha = 0.85) +
    geom_vline(xintercept = 0, color = "white",
               linewidth = 0.4) +
    
    scale_fill_manual(values = c("Moving In"  = "#7b2d8b",
                                 "Moving Out" = "#cc4c02")) +
    scale_x_continuous(
      labels = function(x) format(abs(x), big.mark = ",")
    ) +
    
    labs(x = "← Moving Out     Moving In →",
         y = NULL, fill = NULL) +
    
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
      legend.text        = element_text(color = "white", size = 8),
      legend.background  = element_rect(fill = "#13132a", color = NA)
    )
}

make_distance_chart <- function(inflow, county_lon, county_lat) {
  
  dist_data <- inflow |>
    mutate(
      distance_mi = distHaversine(
        cbind(county_lon, county_lat),
        cbind(other_lon, other_lat)
      ) / 1000 * 0.621371,
      band = case_when(
        distance_mi <  100 ~ "Local\n(<100 mi)",
        distance_mi <  300 ~ "Regional\n(100-300 mi)",
        distance_mi <  600 ~ "Mid-range\n(300-600 mi)",
        distance_mi < 1500 ~ "Long distance\n(600-1500 mi)",
        TRUE               ~ "Cross-country\n(1500+ mi)"
      ),
      band = factor(band, levels = c(
        "Local\n(<100 mi)",
        "Regional\n(100-300 mi)",
        "Mid-range\n(300-600 mi)",
        "Long distance\n(600-1500 mi)",
        "Cross-country\n(1500+ mi)"
      ))
    ) |>
    group_by(band) |>
    summarise(
      migrants = sum(estimate, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    mutate(
      pct        = migrants / sum(migrants),
      text_label = paste0(format(migrants, big.mark = ","),
                          " (", percent(pct, accuracy = 0.1), ")")
    )
  
  ggplot(dist_data,
         aes(x    = band,
             y    = migrants,
             fill = band,
             text = text_label)) +
    
    geom_col(alpha = 0.85, show.legend = FALSE) +
    
    scale_fill_viridis_d(option = "plasma") +
    scale_y_continuous(
      labels  = comma,
      expand  = expansion(mult = c(0, 0.1))
    ) +
    
    labs(x = NULL, y = "Migrants") +
    
    theme_minimal() +
    theme(
      plot.background    = element_rect(fill = "#13132a", color = NA),
      panel.background   = element_rect(fill = "#13132a", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#2a2a3a"),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(color = "#cccccc", size = 8),
      axis.title.y       = element_text(color = "#888888", size = 9)
    )
}

make_neighbor_table <- function(inflow, outflow, neighbor_fips) {
  
  bind_rows(
    inflow  |> mutate(dir = "in"),
    outflow |> mutate(dir = "out")
  ) |>
    filter(GEOID2 %in% neighbor_fips) |>
    mutate(short = str_remove(FULL2_NAME, ",.*$")) |>
    select(short, dir, estimate) |>
    pivot_wider(names_from = dir, values_from = estimate,
                values_fill = 0) |>
    rename(County = short) |>
    mutate(
      In    = replace_na(get("in",  envir = as.environment(.)),  0),
      Out   = replace_na(get("out", envir = as.environment(.)), 0),
      Net   = In - Out,
      Total = In + Out
    ) |>
    select(County, In, Out, Net, Total) |>
    arrange(desc(Total))
}