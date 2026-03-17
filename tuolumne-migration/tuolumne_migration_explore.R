# =============================================================================
# TUOLUMNE COUNTY MIGRATION — DATA EXPLORATION
# =============================================================================
# Run tuolumne_migration.R first to load required objects.
# =============================================================================

# Safety check
required_objects <- c("inflow_df", "outflow_df", "flows_clean", "tuolumne_flows")
missing_objects  <- required_objects[!sapply(required_objects, exists)]
if (length(missing_objects) > 0) {
  stop("Run tuolumne_migration.R first. Missing: ",
       paste(missing_objects, collapse = ", "))
}

library(tidyverse)
library(scales)


# =============================================================================
# SECTION 1: THE BIG NUMBERS
# =============================================================================

total_in   <- sum(inflow_df$estimate,  na.rm = TRUE)
total_out  <- sum(outflow_df$estimate, na.rm = TRUE)
net        <- total_in - total_out
n_in       <- nrow(inflow_df)
n_out      <- nrow(outflow_df)

cat("
╔══════════════════════════════════════════════════════╗
║     TUOLUMNE COUNTY MIGRATION SUMMARY 2016-2020      ║
╠══════════════════════════════════════════════════════╣
║                                                      ║
║  People who moved IN:  ", formatC(total_in,  width = 6, big.mark=","), "                    ║
║  People who moved OUT: ", formatC(total_out, width = 6, big.mark=","), "                    ║
║  Net migration:        ", formatC(net,       width = 6, big.mark=","), "                    ║
║                                                      ║
║  Counties sending people here:    ", formatC(n_in,  width = 3), "             ║
║  Counties receiving people from here:", formatC(n_out, width = 3), "          ║
║                                                      ║
╚══════════════════════════════════════════════════════╝
")


# =============================================================================
# SECTION 2: WHERE DO PEOPLE GO AND COME FROM?
# =============================================================================

cat("\n--- TOP 15 ORIGINS (moving TO Tuolumne) ---\n")
inflow_df |>
  select(County = FULL2_NAME, `Moving In` = estimate, MOE = moe) |>
  head(15) |>
  mutate(
    `Moving In` = format(`Moving In`, big.mark = ","),
    `% of Total` = paste0(
      round(as.numeric(gsub(",","",`Moving In`)) / total_in * 100, 1), "%"
    )
  ) |>
  print()

cat("\n--- TOP 15 DESTINATIONS (leaving Tuolumne) ---\n")
outflow_df |>
  select(County = FULL2_NAME, `Moving Out` = estimate, MOE = moe) |>
  head(15) |>
  mutate(
    `Moving Out` = format(`Moving Out`, big.mark = ","),
    `% of Total` = paste0(
      round(as.numeric(gsub(",","",`Moving Out`)) / total_out * 100, 1), "%"
    )
  ) |>
  print()


# =============================================================================
# SECTION 3: THE CALIFORNIA STORY
# =============================================================================
# Tuolumne is in California — how much of the migration is within the state?

ca_inflow <- inflow_df |>
  mutate(is_ca = str_detect(FULL2_NAME, "California"))

ca_outflow <- outflow_df |>
  mutate(is_ca = str_detect(FULL2_NAME, "California"))

ca_in_total  <- sum(ca_inflow$estimate[ca_inflow$is_ca],   na.rm = TRUE)
ca_out_total <- sum(ca_outflow$estimate[ca_outflow$is_ca], na.rm = TRUE)
oo_in_total  <- sum(ca_inflow$estimate[!ca_inflow$is_ca],  na.rm = TRUE)
oo_out_total <- sum(ca_outflow$estimate[!ca_outflow$is_ca],na.rm = TRUE)

cat("\n--- THE CALIFORNIA STORY ---\n")
cat("Of people moving IN:\n")
cat("  From within California:", format(ca_in_total, big.mark=","),
    paste0("(", round(ca_in_total/total_in*100,1), "%)"), "\n")
cat("  From outside California:", format(oo_in_total, big.mark=","),
    paste0("(", round(oo_in_total/total_in*100,1), "%)"), "\n")

cat("\nOf people moving OUT:\n")
cat("  To within California:", format(ca_out_total, big.mark=","),
    paste0("(", round(ca_out_total/total_out*100,1), "%)"), "\n")
cat("  To outside California:", format(oo_out_total, big.mark=","),
    paste0("(", round(oo_out_total/total_out*100,1), "%)"), "\n")

cat("\nTop 10 California counties sending people TO Tuolumne:\n")
ca_inflow |>
  filter(is_ca) |>
  mutate(short = str_remove(FULL2_NAME, ", California")) |>
  select(County = short, `Moving In` = estimate) |>
  head(10) |>
  print()

cat("\nTop 10 California counties receiving people FROM Tuolumne:\n")
ca_outflow |>
  filter(is_ca) |>
  mutate(short = str_remove(FULL2_NAME, ", California")) |>
  select(County = short, `Moving Out` = estimate) |>
  head(10) |>
  print()


# =============================================================================
# SECTION 4: TWO-WAY RELATIONSHIPS
# =============================================================================
# Which counties have the strongest MUTUAL migration with Tuolumne?
# i.e., people moving both ways — true exchange relationships

two_way <- inflow_df |>
  select(GEOID2, FULL2_NAME, moved_in = estimate) |>
  inner_join(
    outflow_df |> select(GEOID2, moved_out = estimate),
    by = "GEOID2"
  ) |>
  mutate(
    total_exchange = moved_in + moved_out,
    net            = moved_in - moved_out,
    net_direction  = ifelse(net > 0, "Net gain from", "Net loss to"),
    ratio          = round(moved_in / moved_out, 2),
    short_name     = str_remove(FULL2_NAME, ",.*$")
  ) |>
  arrange(desc(total_exchange))

cat("\n--- STRONGEST TWO-WAY MIGRATION RELATIONSHIPS ---\n")
cat("(Counties where people move both TO and FROM Tuolumne)\n\n")
two_way |>
  select(
    County        = short_name,
    `Moved In`    = moved_in,
    `Moved Out`   = moved_out,
    `Total Exchange` = total_exchange,
    `Net`         = net,
    Direction     = net_direction
  ) |>
  head(15) |>
  print()


# =============================================================================
# SECTION 5: NET MIGRATION — WHO'S WINNING AND LOSING
# =============================================================================

net_flows <- flows_clean |>
  filter(variable %in% c("MOVEDIN", "MOVEDOUT")) |>
  select(GEOID2, FULL2_NAME, variable, estimate) |>
  pivot_wider(names_from = variable, values_from = estimate) |>
  rename(moved_in = MOVEDIN, moved_out = MOVEDOUT) |>
  mutate(
    moved_in  = replace_na(moved_in,  0),
    moved_out = replace_na(moved_out, 0),
    net       = moved_in - moved_out,
    short_name = str_remove(FULL2_NAME, ",.*$")
  ) |>
  filter(moved_in > 0 | moved_out > 0)

cat("\n--- TOP 10 NET GAINERS (Tuolumne gains most FROM these counties) ---\n")
net_flows |>
  filter(net > 0) |>
  arrange(desc(net)) |>
  select(County = short_name, `Moved In` = moved_in,
         `Moved Out` = moved_out, `Net Gain` = net) |>
  head(10) |>
  print()

cat("\n--- TOP 10 NET LOSERS (Tuolumne loses most TO these counties) ---\n")
net_flows |>
  filter(net < 0) |>
  arrange(net) |>
  select(County = short_name, `Moved In` = moved_in,
         `Moved Out` = moved_out, `Net Loss` = net) |>
  head(10) |>
  print()


# =============================================================================
# SECTION 6: DISTANCE ANALYSIS
# =============================================================================
# Are people mostly moving locally or making big moves?
# Use the arc endpoint coordinates already in our data

library(geosphere)

distance_df <- flows_clean |>
  filter(variable == "MOVEDIN") |>
  mutate(
    distance_km = distHaversine(
      cbind(tuol_lon, tuol_lat),
      cbind(other_lon, other_lat)
    ) / 1000,
    distance_mi   = distance_km * 0.621371,
    distance_band = case_when(
      distance_mi <  100 ~ "Local (< 100 mi)",
      distance_mi <  300 ~ "Regional (100-300 mi)",
      distance_mi <  600 ~ "Mid-range (300-600 mi)",
      distance_mi < 1500 ~ "Long distance (600-1500 mi)",
      TRUE               ~ "Cross-country (1500+ mi)"
    ),
    distance_band = factor(distance_band, levels = c(
      "Local (< 100 mi)",
      "Regional (100-300 mi)",
      "Mid-range (300-600 mi)",
      "Long distance (600-1500 mi)",
      "Cross-country (1500+ mi)"
    ))
  )

cat("\n--- HOW FAR DO PEOPLE TRAVEL TO GET TO TUOLUMNE? ---\n")
distance_df |>
  group_by(distance_band) |>
  summarise(
    counties  = n(),
    migrants  = sum(estimate, na.rm = TRUE),
    avg_per_county = round(mean(estimate, na.rm = TRUE))
  ) |>
  mutate(pct = paste0(round(migrants / sum(migrants) * 100, 1), "%")) |>
  print()

cat("\n--- FARTHEST COUNTIES SENDING PEOPLE TO TUOLUMNE ---\n")
distance_df |>
  arrange(desc(distance_mi)) |>
  filter(estimate > 0) |>
  mutate(short = str_remove(FULL2_NAME, ",.*$")) |>
  select(County = short, `Miles Away` = distance_mi,
         `People Moving In` = estimate) |>
  mutate(`Miles Away` = round(`Miles Away`)) |>
  head(10) |>
  print()

cat("\n--- MEDIAN DISTANCE PEOPLE TRAVEL TO TUOLUMNE ---\n")
median_dist <- weighted.mean(distance_df$distance_mi,
                             distance_df$estimate, na.rm = TRUE)
cat("Weighted median distance:", round(median_dist), "miles\n")


# =============================================================================
# SECTION 7: SURPRISE COUNTIES
# =============================================================================
# Small or unexpected places with notable migration flows

cat("\n--- SURPRISE FINDS: UNEXPECTED ORIGINS ---\n")
cat("(Counties far away or surprising that still sent people)\n\n")

distance_df |>
  filter(distance_mi > 1000, estimate >= 5) |>
  mutate(short = str_remove(FULL2_NAME, ",.*$")) |>
  arrange(desc(estimate)) |>
  select(County = short, State = FULL2_NAME,
         `Miles Away` = distance_mi,
         `People Moving In` = estimate) |>
  mutate(`Miles Away` = round(`Miles Away`)) |>
  head(15) |>
  print()


# =============================================================================
# SECTION 8: QUICK VISUALIZATIONS
# =============================================================================

# --- Plot 1: Top 20 inflow vs outflow side by side ---
top20_counties <- two_way |>
  head(20) |>
  pull(GEOID2)

comparison_plot <- flows_clean |>
  filter(GEOID2 %in% top20_counties) |>
  filter(variable %in% c("MOVEDIN", "MOVEDOUT")) |>
  mutate(
    short_name = str_remove(FULL2_NAME, ",.*$"),
    short_name = str_trunc(short_name, 25),
    estimate   = ifelse(variable == "MOVEDOUT", -estimate, estimate),
    direction  = ifelse(variable == "MOVEDIN", "Moving In", "Moving Out")
  ) |>
  ggplot(aes(x = estimate,
             y = reorder(short_name, abs(estimate)),
             fill = direction)) +
  
  geom_col(alpha = 0.85) +
  geom_vline(xintercept = 0, color = "white", linewidth = 0.5) +
  
  scale_fill_manual(values = c("Moving In"  = "#7b2d8b",
                               "Moving Out" = "#cc4c02")) +
  scale_x_continuous(
    labels = function(x) format(abs(x), big.mark = ","),
    breaks = pretty_breaks(6)
  ) +
  
  labs(
    title    = "Tuolumne County: Migration In vs Out by County",
    subtitle = "Top 20 counties by total exchange volume | ACS 2016-2020",
    x        = "← Moving Out          Moving In →",
    y        = NULL,
    fill     = NULL,
    caption  = "Source: U.S. Census Bureau ACS 2016-2020"
  ) +
  
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#0d0d1a", color = NA),
    panel.background = element_rect(fill = "#0d0d1a", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#2a2a3a"),
    panel.grid.minor   = element_blank(),
    plot.title    = element_text(color = "white", face = "bold",
                                 size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "#aaaaaa", size = 10,
                                 hjust = 0.5),
    plot.caption  = element_text(color = "#555555", size = 8),
    axis.text     = element_text(color = "#cccccc", size = 8),
    axis.title.x  = element_text(color = "#888888", size = 9),
    legend.position = "bottom",
    legend.text   = element_text(color = "white")
  )

comparison_plot

ggsave(
  here(OUTPUT_DIR, "tuolumne_comparison_chart.png"),
  plot   = comparison_plot,
  width  = 10,
  height = 8,
  dpi    = 300,
  bg     = "#0d0d1a"
)

# --- Plot 2: Distance distribution ---
distance_plot <- distance_df |>
  group_by(distance_band) |>
  summarise(migrants = sum(estimate, na.rm = TRUE)) |>
  mutate(pct = migrants / sum(migrants)) |>
  
  ggplot(aes(x = distance_band, y = migrants, fill = distance_band)) +
  
  geom_col(alpha = 0.85, show.legend = FALSE) +
  
  geom_text(aes(label = paste0(format(migrants, big.mark=","),
                               "\n(", percent(pct, accuracy=0.1), ")")),
            vjust = -0.4, color = "white", size = 3.5) +
  
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title    = "How Far Do People Travel to Move to Tuolumne County?",
    subtitle = "Inflow migration by distance band | ACS 2016-2020",
    x        = NULL,
    y        = "Number of migrants",
    caption  = "Source: U.S. Census Bureau ACS 2016-2020"
  ) +
  
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#0d0d1a", color = NA),
    panel.background = element_rect(fill = "#0d0d1a", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#2a2a3a"),
    panel.grid.minor   = element_blank(),
    plot.title    = element_text(color = "white", face = "bold",
                                 size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "#aaaaaa", size = 10,
                                 hjust = 0.5),
    plot.caption  = element_text(color = "#555555", size = 8),
    axis.text.x   = element_text(color = "#cccccc", size = 8,
                                 angle = 15, hjust = 1),
    axis.text.y   = element_text(color = "#cccccc", size = 8)
  )

distance_plot

ggsave(
  here(OUTPUT_DIR, "tuolumne_distance_chart.png"),
  plot   = distance_plot,
  width  = 10,
  height = 6,
  dpi    = 300,
  bg     = "#0d0d1a"
)

cat("\nExploration complete. Charts saved to:", OUTPUT_DIR, "\n")

# =============================================================================
# END OF SCRIPT
# =============================================================================