# =============================================================================
# TUOLUMNE COUNTY MIGRATION — DEEP DIVE ANALYSIS
# =============================================================================
# Investigates the most interesting stories found in the exploration script:
#   1. The Stanislaus Shuffle (neighbor county churn)
#   2. Bay Area cash-out ratios
#   3. One-way pipelines (Salt Lake, Guadalupe TX)
#   4. Surprise long-distance flows (Maui, Franklin FL)
#   5. MOE reliability check on key findings
#   6. Comparison to neighboring counties
#   7. How does Tuolumne rank among similar Sierra counties?
#
# DEPENDS ON: tuolumne_migration.R
# Run tuolumne_migration.R first, or uncomment source() below.
# =============================================================================

# source(here::here("tuolumne-migration", "tuolumne_migration.R"))

# Safety check
required_objects <- c("inflow_df", "outflow_df", "flows_clean",
                      "tuolumne_flows", "OUTPUT_DIR")
missing_objects  <- required_objects[!sapply(required_objects, exists)]
if (length(missing_objects) > 0) {
  stop("Run tuolumne_migration.R first. Missing: ",
       paste(missing_objects, collapse = ", "))
}

library(tidyverse)
library(scales)
library(geosphere)
library(tidycensus)
library(sf)
library(here)

cat("Starting deep dive analysis...\n")


# =============================================================================
# SECTION 1: MOE RELIABILITY CHECK
# =============================================================================
# The margin of error (MOE) on ACS estimates can be huge for small counties.
# Before treating any finding as real, check if the estimate is reliable.
# Rule of thumb: if MOE > 50% of estimate, treat with caution.
# Census calls this the "Coefficient of Variation" (CV).

cat("\n=== SECTION 1: MOE RELIABILITY CHECK ===\n")

reliability <- bind_rows(
  inflow_df  |> mutate(direction = "Inflow"),
  outflow_df |> mutate(direction = "Outflow")
) |>
  mutate(
    cv          = moe / estimate,           # coefficient of variation
    reliability = case_when(
      cv <= 0.15 ~ "High reliability",      # MOE < 15% of estimate
      cv <= 0.30 ~ "Medium reliability",    # MOE 15-30%
      cv <= 0.50 ~ "Use with caution",      # MOE 30-50%
      TRUE       ~ "Unreliable"             # MOE > 50% — treat as noise
    ),
    short_name  = str_remove(FULL2_NAME, ",.*$")
  )

cat("\nReliability summary across all flows:\n")
reliability |>
  count(reliability) |>
  mutate(pct = percent(n / sum(n), accuracy = 0.1)) |>
  print()

cat("\nKey findings — reliability check:\n")

# Check our most interesting specific findings
key_findings <- c(
  "Clark County",          # Las Vegas
  "Salt Lake County",      # SLC one-way
  "Maui County",           # Hawaii surprise
  "Franklin County",       # Florida surprise
  "Guadalupe County",      # Texas one-way
  "Monongalia County",     # WV college town
  "Stanislaus County",     # biggest relationship
  "Alameda County"         # Bay Area cashout
)

reliability |>
  filter(str_detect(FULL2_NAME,
                    paste(key_findings, collapse = "|"))) |>
  select(County     = short_name,
         Direction  = direction,
         Estimate   = estimate,
         MOE        = moe,
         CV         = cv,
         Reliability = reliability) |>
  mutate(CV = percent(CV, accuracy = 0.1)) |>
  arrange(County, Direction) |>
  print()


# =============================================================================
# SECTION 2: THE STANISLAUS SHUFFLE — DEEP DIVE
# =============================================================================
# 900 total exchanges between two neighboring counties.
# Who are these people and what does the churn look like?

cat("\n=== SECTION 2: THE STANISLAUS SHUFFLE ===\n")

# The Stanislaus relationship vs all other neighbors
neighbors <- c(
  "06109",  # Tuolumne (self — excluded)
  "06109",  # placeholder
  "06005",  # Amador
  "06009",  # Calaveras
  "06019",  # Fresno
  "06039",  # Madera
  "06051",  # Mono
  "06077",  # San Joaquin
  "06099"   # Stanislaus
)

neighbor_names <- c(
  "Amador", "Calaveras", "Fresno",
  "Madera", "Mono", "San Joaquin", "Stanislaus"
)

cat("\nAll contiguous neighbor county flows:\n")
bind_rows(
  inflow_df  |> mutate(direction = "In"),
  outflow_df |> mutate(direction = "Out")
) |>
  filter(GEOID2 %in% neighbors) |>
  mutate(short = str_remove(FULL2_NAME, ",.*$")) |>
  select(County = short, Direction = direction, Estimate = estimate) |>
  pivot_wider(names_from = Direction, values_from = Estimate) |>
  mutate(
    Net       = replace_na(In, 0) - replace_na(Out, 0),
    Total     = replace_na(In, 0) + replace_na(Out, 0),
    Ratio     = round(replace_na(In, 0) / pmax(replace_na(Out, 0), 1), 2)
  ) |>
  arrange(desc(Total)) |>
  print()

cat("\n--- STANISLAUS vs ALL OTHER COUNTIES ---\n")
stanislaus_share_in  <- inflow_df$estimate[
  str_detect(inflow_df$FULL2_NAME, "Stanislaus")
] / sum(inflow_df$estimate) * 100

stanislaus_share_out <- outflow_df$estimate[
  str_detect(outflow_df$FULL2_NAME, "Stanislaus")
] / sum(outflow_df$estimate) * 100

cat("Stanislaus share of ALL inflow: ",
    round(stanislaus_share_in, 1), "%\n")
cat("Stanislaus share of ALL outflow:",
    round(stanislaus_share_out, 1), "%\n")
cat("Total Stanislaus exchange (900) as % of all exchanges:",
    round(900 / (sum(inflow_df$estimate) +
                   sum(outflow_df$estimate)) * 100, 1), "%\n")


# =============================================================================
# SECTION 3: BAY AREA CASH-OUT RATIOS
# =============================================================================
# Rank all Bay Area counties by how strongly they favor Tuolumne

cat("\n=== SECTION 3: BAY AREA CASH-OUT ANALYSIS ===\n")

bay_area_fips <- c(
  "06001",  # Alameda
  "06013",  # Contra Costa
  "06041",  # Marin
  "06055",  # Napa
  "06075",  # San Francisco
  "06081",  # San Mateo
  "06085",  # Santa Clara
  "06095",  # Solano
  "06097"   # Sonoma
)

bay_flows <- bind_rows(
  inflow_df  |> mutate(direction = "moved_in"),
  outflow_df |> mutate(direction = "moved_out")
) |>
  filter(GEOID2 %in% bay_area_fips) |>
  mutate(short = str_remove(FULL2_NAME, ", California")) |>
  select(County = short, GEOID2, direction, estimate) |>
  pivot_wider(names_from = direction, values_from = estimate,
              values_fill = 0) |>
  mutate(
    net       = moved_in - moved_out,
    ratio     = round(moved_in / pmax(moved_out, 1), 1),
    net_label = ifelse(net > 0,
                       paste0("+", format(net, big.mark=",")),
                       format(net, big.mark=","))
  ) |>
  arrange(desc(moved_in))

cat("\nBay Area county flows to/from Tuolumne:\n")
bay_flows |>
  select(County,
         `Moved In`  = moved_in,
         `Moved Out` = moved_out,
         `Net`       = net_label,
         `In:Out Ratio` = ratio) |>
  print()

total_bay_in  <- sum(bay_flows$moved_in)
total_bay_out <- sum(bay_flows$moved_out)
cat("\nTotal Bay Area inflow to Tuolumne:", total_bay_in, "\n")
cat("Total Bay Area outflow from Tuolumne:", total_bay_out, "\n")
cat("Net Bay Area contribution:", total_bay_in - total_bay_out, "\n")
cat("Bay Area as % of all inflow:",
    round(total_bay_in / sum(inflow_df$estimate) * 100, 1), "%\n")


# =============================================================================
# SECTION 4: ONE-WAY PIPELINE ANALYSIS
# =============================================================================
# Find ALL counties where migration is essentially one-directional

cat("\n=== SECTION 4: ONE-WAY PIPELINES ===\n")

all_flows <- flows_clean |>
  filter(variable %in% c("MOVEDIN", "MOVEDOUT")) |>
  select(GEOID2, FULL2_NAME, variable, estimate) |>
  pivot_wider(names_from = variable, values_from = estimate,
              values_fill = 0) |>
  rename(moved_in = MOVEDIN, moved_out = MOVEDOUT) |>
  mutate(
    net       = moved_in - moved_out,
    total     = moved_in + moved_out,
    ratio     = case_when(
      moved_out == 0 ~ Inf,
      moved_in  == 0 ~ 0,
      TRUE           ~ moved_in / moved_out
    ),
    short_name = str_remove(FULL2_NAME, ",.*$"),
    state      = str_extract(FULL2_NAME, "[^,]+$") |> str_trim()
  )

# Pure one-way IN (nobody leaves Tuolumne for these places)
cat("\nPURE ONE-WAY IN — people come from here, nobody goes back:\n")
all_flows |>
  filter(moved_out == 0, moved_in >= 10) |>
  arrange(desc(moved_in)) |>
  select(County    = short_name,
         State     = state,
         `Moved In` = moved_in) |>
  print()

# Pure one-way OUT (nobody comes from these places to Tuolumne)
cat("\nPURE ONE-WAY OUT — people leave to here, nobody comes back:\n")
all_flows |>
  filter(moved_in == 0, moved_out >= 10) |>
  arrange(desc(moved_out)) |>
  select(County     = short_name,
         State      = state,
         `Moved Out` = moved_out) |>
  print()

# Strong one-way (ratio > 5:1 or < 1:5)
cat("\nSTRONG ONE-WAY TOWARD TUOLUMNE (5:1+ ratio, min 20 people):\n")
all_flows |>
  filter(ratio >= 5, moved_in >= 20) |>
  arrange(desc(ratio)) |>
  select(County     = short_name,
         State      = state,
         `Moved In`  = moved_in,
         `Moved Out` = moved_out,
         Ratio      = ratio) |>
  mutate(Ratio = ifelse(is.infinite(Ratio), "∞", as.character(round(Ratio, 1)))) |>
  print()

cat("\nSTRONG ONE-WAY AWAY FROM TUOLUMNE (1:5+ ratio, min 20 people):\n")
# The issue is that after select(), the column is named "Ratio" not "ratio"
# Fix: rename AFTER the mutate, or reference the new name

all_flows |>
  filter(ratio <= 0.2, moved_out >= 20) |>
  arrange(ratio) |>
  mutate(
    Ratio = ifelse(ratio == 0, "0 (one-way out)",
                   as.character(round(ratio, 2)))
  ) |>
  select(County     = short_name,
         State      = state,
         `Moved In`  = moved_in,
         `Moved Out` = moved_out,
         Ratio) |>
  print()


# =============================================================================
# SECTION 5: GEOGRAPHIC PULL ANALYSIS
# =============================================================================
# Which states are sending / receiving the most people?

cat("\n=== SECTION 5: STATE-LEVEL PATTERNS ===\n")

state_inflow <- inflow_df |>
  mutate(state = str_extract(FULL2_NAME, "[^,]+$") |> str_trim()) |>
  group_by(state) |>
  summarise(
    counties  = n(),
    moved_in  = sum(estimate, na.rm = TRUE)
  ) |>
  arrange(desc(moved_in))

state_outflow <- outflow_df |>
  mutate(state = str_extract(FULL2_NAME, "[^,]+$") |> str_trim()) |>
  group_by(state) |>
  summarise(
    counties  = n(),
    moved_out = sum(estimate, na.rm = TRUE)
  ) |>
  arrange(desc(moved_out))

cat("\nTop states sending people TO Tuolumne:\n")
state_inflow |>
  mutate(
    pct = percent(moved_in / sum(moved_in), accuracy = 0.1)
  ) |>
  head(10) |>
  print()

cat("\nTop states receiving people FROM Tuolumne:\n")
state_outflow |>
  mutate(
    pct = percent(moved_out / sum(moved_out), accuracy = 0.1)
  ) |>
  head(10) |>
  print()

# State-level net
cat("\nState-level net migration with Tuolumne:\n")
state_inflow |>
  full_join(state_outflow, by = "state") |>
  mutate(
    moved_in  = replace_na(moved_in,  0),
    moved_out = replace_na(moved_out, 0),
    net       = moved_in - moved_out
  ) |>
  filter(state != "California") |>   # CA dominates, look at out-of-state
  arrange(desc(abs(net))) |>
  select(State     = state,
         `Moved In`  = moved_in,
         `Moved Out` = moved_out,
         Net        = net) |>
  head(15) |>
  print()


# =============================================================================
# SECTION 6: COMPARE TO NEIGHBORING SIERRA COUNTIES
# =============================================================================
# How does Tuolumne's migration story compare to Calaveras, Amador, Mariposa?
# Pull their flows too and compare key metrics side by side.

cat("\n=== SECTION 6: SIERRA COUNTY COMPARISON ===\n")
cat("Pulling migration data for neighboring Sierra counties...\n")
cat("(This will make a few API calls — may take 30-60 seconds)\n\n")

sierra_counties <- list(
  list(state = "CA", county = "Amador",    fips = "06005"),
  list(state = "CA", county = "Calaveras", fips = "06009"),
  list(state = "CA", county = "Mariposa",  fips = "06043"),
  list(state = "CA", county = "El Dorado", fips = "06017")
)

pull_county_summary <- function(state, county, fips) {
  tryCatch({
    flows <- get_flows(
      geography = "county",
      state     = state,
      county    = county,
      year      = 2020,
      geometry  = FALSE
    )
    
    inflow  <- flows |>
      filter(variable == "MOVEDIN",  !is.na(estimate),
             estimate > 0, GEOID2 != fips)
    outflow <- flows |>
      filter(variable == "MOVEDOUT", !is.na(estimate),
             estimate > 0, GEOID2 != fips)
    
    tibble(
      county         = county,
      total_in       = sum(inflow$estimate,  na.rm = TRUE),
      total_out      = sum(outflow$estimate, na.rm = TRUE),
      net            = total_in - total_out,
      n_origins      = nrow(inflow),
      n_destinations = nrow(outflow),
      top_origin     = inflow |>
        arrange(desc(estimate)) |>
        slice(1) |>
        pull(FULL2_NAME) |>
        str_remove(",.*$"),
      top_origin_n   = inflow |>
        arrange(desc(estimate)) |>
        slice(1) |>
        pull(estimate),
      top_dest       = outflow |>
        arrange(desc(estimate)) |>
        slice(1) |>
        pull(FULL2_NAME) |>
        str_remove(",.*$"),
      top_dest_n     = outflow |>
        arrange(desc(estimate)) |>
        slice(1) |>
        pull(estimate)
    )
  }, error = function(e) {
    cat("  Warning: Could not pull data for", county, "\n")
    NULL
  })
}

# Pull all four comparison counties
sierra_comparison <- map_dfr(sierra_counties, function(x) {
  cat("  Pulling", x$county, "...\n")
  pull_county_summary(x$state, x$county, x$fips)
})

# Add Tuolumne for comparison
tuolumne_row <- tibble(
  county         = "Tuolumne",
  total_in       = sum(inflow_df$estimate),
  total_out      = sum(outflow_df$estimate),
  net            = sum(inflow_df$estimate) - sum(outflow_df$estimate),
  n_origins      = nrow(inflow_df),
  n_destinations = nrow(outflow_df),
  top_origin     = inflow_df |>
    slice(1) |> pull(FULL2_NAME) |> str_remove(",.*$"),
  top_origin_n   = inflow_df |> slice(1) |> pull(estimate),
  top_dest       = outflow_df |>
    slice(1) |> pull(FULL2_NAME) |> str_remove(",.*$"),
  top_dest_n     = outflow_df |> slice(1) |> pull(estimate)
)

all_sierra <- bind_rows(tuolumne_row, sierra_comparison) |>
  mutate(
    net_label      = ifelse(net > 0,
                            paste0("+", format(net, big.mark=",")),
                            format(net, big.mark=",")),
    exchange_rate  = round((total_in + total_out) /
                             ((total_in + total_out) / 2), 1)
  )

cat("\n--- SIERRA FOOTHILLS MIGRATION COMPARISON ---\n")
all_sierra |>
  select(
    County         = county,
    `Moved In`     = total_in,
    `Moved Out`    = total_out,
    `Net`          = net_label,
    `# Origins`    = n_origins,
    `# Dests`      = n_destinations,
    `Top Origin`   = top_origin,
    `Top Dest`     = top_dest
  ) |>
  print()


# =============================================================================
# SECTION 7: THE SURPRISE FLOWS — DEEPER LOOK
# =============================================================================

cat("\n=== SECTION 7: SURPRISE FLOWS — DEEPER LOOK ===\n")

# Franklin County FL — check MOE and context
cat("\n--- FRANKLIN COUNTY, FL (53 people, 2083 miles) ---\n")
inflow_df |>
  filter(str_detect(FULL2_NAME, "Franklin County, Florida")) |>
  select(County = FULL2_NAME, Estimate = estimate, MOE = moe) |>
  mutate(
    CV          = percent(MOE / Estimate, accuracy = 0.1),
    Reliable    = MOE / Estimate <= 0.5,
    `Note`      = paste0("Population ~12,000. MOE = ", MOE,
                         ". CV = ", percent(MOE/Estimate, accuracy=0.1))
  ) |>
  print()

# Maui County HI
cat("\n--- MAUI COUNTY, HI (50 people, 2476 miles) ---\n")
inflow_df |>
  filter(str_detect(FULL2_NAME, "Maui")) |>
  select(County = FULL2_NAME, Estimate = estimate, MOE = moe) |>
  mutate(
    CV       = percent(MOE / Estimate, accuracy = 0.1),
    Reliable = MOE / Estimate <= 0.5
  ) |>
  print()

# All Hawaii flows
cat("\nAll Hawaii flows:\n")
bind_rows(
  inflow_df  |> mutate(dir = "In"),
  outflow_df |> mutate(dir = "Out")
) |>
  filter(str_detect(FULL2_NAME, "Hawaii")) |>
  select(County = FULL2_NAME, Direction = dir,
         Estimate = estimate, MOE = moe) |>
  print()

# Guadalupe County TX — military connection check
cat("\n--- GUADALUPE COUNTY, TX (111 out, 0 in) ---\n")
cat("Note: Contains Randolph AFB / Seguin area (suburban San Antonio)\n")
outflow_df |>
  filter(str_detect(FULL2_NAME, "Guadalupe")) |>
  select(County = FULL2_NAME, Estimate = estimate, MOE = moe) |>
  mutate(
    CV       = percent(MOE / Estimate, accuracy = 0.1),
    Reliable = MOE / Estimate <= 0.5
  ) |>
  print()

# All Texas flows combined
cat("\nAll Texas county flows:\n")
bind_rows(
  inflow_df  |> mutate(dir = "In"),
  outflow_df |> mutate(dir = "Out")
) |>
  filter(str_detect(FULL2_NAME, "Texas")) |>
  mutate(short = str_remove(FULL2_NAME, ", Texas")) |>
  select(County    = short,
         Direction = dir,
         Estimate  = estimate,
         MOE       = moe) |>
  arrange(County, Direction) |>
  print()

# Salt Lake City one-way check
cat("\n--- SALT LAKE COUNTY, UT (130 out, 0 in) ---\n")
outflow_df |>
  filter(str_detect(FULL2_NAME, "Salt Lake")) |>
  select(County = FULL2_NAME, Estimate = estimate, MOE = moe) |>
  mutate(
    CV       = percent(MOE / Estimate, accuracy = 0.1),
    Reliable = MOE / Estimate <= 0.5,
    Note     = "Large LDS population in both counties may drive this flow"
  ) |>
  print()


# =============================================================================
# SECTION 8: DEEP DIVE VISUALIZATIONS
# =============================================================================

cat("\n=== SECTION 8: BUILDING DEEP DIVE CHARTS ===\n")

# --- Chart 1: Bay Area ratio chart ---
cat("Building Bay Area ratio chart...\n")

bay_ratio_plot <- bay_flows |>
  filter(moved_in + moved_out > 0) |>
  mutate(
    County    = fct_reorder(County, moved_in),
    ratio_lab = ifelse(is.infinite(ratio),
                       "No outflow",
                       paste0(ratio, ":1"))
  ) |>
  ggplot(aes(y = County)) +
  
  # Outflow bar (left, negative direction visually)
  geom_col(aes(x = -moved_out), fill = "#cc4c02",
           alpha = 0.8, width = 0.6) +
  
  # Inflow bar (right)
  geom_col(aes(x = moved_in), fill = "#7b2d8b",
           alpha = 0.8, width = 0.6) +
  
  # Center line
  geom_vline(xintercept = 0, color = "white",
             linewidth = 0.5) +
  
  # Ratio label
  geom_text(aes(x = moved_in + 5,
                label = paste0(ratio, ":1 →")),
            hjust = 0, color = "#aaaaaa", size = 3) +
  
  scale_x_continuous(
    labels   = function(x) format(abs(x), big.mark = ","),
    breaks   = pretty_breaks(6),
    expand   = expansion(mult = c(0.05, 0.25))
  ) +
  
  labs(
    title    = "Bay Area Counties: Migration Exchange with Tuolumne",
    subtitle = "Purple = moving TO Tuolumne  |  Orange = leaving Tuolumne  |  Ratio = In:Out",
    x        = "← Leaving Tuolumne          Moving to Tuolumne →",
    y        = NULL,
    caption  = "Source: U.S. Census Bureau ACS 2016-2020"
  ) +
  
  theme_minimal() +
  theme(
    plot.background    = element_rect(fill = "#0d0d1a", color = NA),
    panel.background   = element_rect(fill = "#0d0d1a", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#2a2a3a"),
    panel.grid.minor   = element_blank(),
    plot.title    = element_text(color = "white",   face = "bold",
                                 size = 13, hjust = 0.5),
    plot.subtitle = element_text(color = "#aaaaaa", size = 9,
                                 hjust = 0.5),
    plot.caption  = element_text(color = "#555555", size = 8),
    axis.text     = element_text(color = "#cccccc", size = 9),
    axis.title.x  = element_text(color = "#888888", size = 9)
  )

bay_ratio_plot

ggsave(
  here(OUTPUT_DIR, "tuolumne_bayarea_deep.png"),
  plot = bay_ratio_plot, width = 10, height = 5,
  dpi = 300, bg = "#0d0d1a"
)
cat("Saved: tuolumne_bayarea_deep.png\n")


# --- Chart 2: One-way pipeline visual ---
cat("Building one-way pipeline chart...\n")

pipeline_data <- all_flows |>
  filter(total >= 20) |>
  mutate(
    flow_type = case_when(
      moved_out == 0               ~ "Pure inflow\n(no return)",
      moved_in  == 0               ~ "Pure outflow\n(no return)",
      ratio >= 3                   ~ "Strong inflow\n(3:1+)",
      ratio <= 0.33                ~ "Strong outflow\n(3:1+)",
      TRUE                         ~ "Balanced\nexchange"
    ),
    flow_type = factor(flow_type, levels = c(
      "Pure inflow\n(no return)",
      "Strong inflow\n(3:1+)",
      "Balanced\nexchange",
      "Strong outflow\n(3:1+)",
      "Pure outflow\n(no return)"
    )),
    short_name = str_trunc(short_name, 20)
  )

pipeline_plot <- pipeline_data |>
  ggplot(aes(x = moved_in, y = moved_out,
             color = flow_type, size = total)) +
  
  # Reference line (1:1 ratio)
  geom_abline(slope = 1, intercept = 0,
              color = "#444444", linetype = "dashed",
              linewidth = 0.5) +
  
  geom_point(alpha = 0.75) +
  
  # Label the most interesting points
  ggrepel::geom_text_repel(
    data = pipeline_data |>
      filter(total >= 100 | moved_out == 0 & moved_in >= 30 |
               moved_in == 0 & moved_out >= 30),
    aes(label = short_name),
    color    = "white",
    size     = 2.8,
    max.overlaps = 15,
    segment.color = "#555555"
  ) +
  
  scale_color_manual(values = c(
    "Pure inflow\n(no return)"   = "#7b2d8b",
    "Strong inflow\n(3:1+)"      = "#b05cc7",
    "Balanced\nexchange"          = "#888888",
    "Strong outflow\n(3:1+)"     = "#e07840",
    "Pure outflow\n(no return)"  = "#cc4c02"
  )) +
  
  scale_size_continuous(range = c(2, 10), guide = "none") +
  
  labs(
    title    = "Tuolumne County: Migration Flow Patterns by County",
    subtitle = "Each dot = one county  |  Dot size = total exchange volume  |  Dashed line = equal in/out",
    x        = "People Moving TO Tuolumne",
    y        = "People Leaving Tuolumne",
    color    = "Flow Type",
    caption  = "Source: U.S. Census Bureau ACS 2016-2020"
  ) +
  
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#0d0d1a", color = NA),
    panel.background = element_rect(fill = "#0d0d1a", color = NA),
    panel.grid       = element_line(color = "#2a2a3a"),
    panel.grid.minor = element_blank(),
    plot.title    = element_text(color = "white",   face = "bold",
                                 size = 13, hjust = 0.5),
    plot.subtitle = element_text(color = "#aaaaaa", size = 9,
                                 hjust = 0.5),
    plot.caption  = element_text(color = "#555555", size = 8),
    axis.text     = element_text(color = "#cccccc", size = 9),
    axis.title    = element_text(color = "#888888", size = 10),
    legend.background = element_rect(fill = "#0d0d1a", color = NA),
    legend.text   = element_text(color = "white",   size = 8),
    legend.title  = element_text(color = "#aaaaaa", size = 9)
  )

# Need ggrepel for labels
if (!"ggrepel" %in% installed.packages()[,"Package"]) {
  install.packages("ggrepel")
}
library(ggrepel)

# Rebuild with ggrepel loaded
pipeline_plot <- pipeline_data |>
  ggplot(aes(x = moved_in, y = moved_out,
             color = flow_type, size = total)) +
  
  geom_abline(slope = 1, intercept = 0,
              color = "#444444", linetype = "dashed",
              linewidth = 0.5) +
  
  geom_point(alpha = 0.75) +
  
  geom_text_repel(
    data = pipeline_data |>
      filter(total >= 100 |
               (moved_out == 0 & moved_in  >= 30) |
               (moved_in  == 0 & moved_out >= 30)),
    aes(label = short_name),
    color         = "white",
    size          = 2.8,
    max.overlaps  = 20,
    segment.color = "#555555"
  ) +
  
  scale_color_manual(values = c(
    "Pure inflow\n(no return)"  = "#7b2d8b",
    "Strong inflow\n(3:1+)"     = "#b05cc7",
    "Balanced\nexchange"         = "#888888",
    "Strong outflow\n(3:1+)"    = "#e07840",
    "Pure outflow\n(no return)" = "#cc4c02"
  )) +
  
  scale_size_continuous(range = c(2, 10), guide = "none") +
  
  labs(
    title    = "Tuolumne County: Migration Flow Patterns by County",
    subtitle = "Each dot = one county  |  Size = total exchange  |  Dashed = equal in/out",
    x        = "People Moving TO Tuolumne",
    y        = "People Leaving Tuolumne",
    color    = "Flow Type",
    caption  = "Source: U.S. Census Bureau ACS 2016-2020"
  ) +
  
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#0d0d1a", color = NA),
    panel.background = element_rect(fill = "#0d0d1a", color = NA),
    panel.grid       = element_line(color = "#2a2a3a"),
    panel.grid.minor = element_blank(),
    plot.title    = element_text(color = "white",   face = "bold",
                                 size = 13, hjust = 0.5),
    plot.subtitle = element_text(color = "#aaaaaa", size = 9,
                                 hjust = 0.5),
    plot.caption  = element_text(color = "#555555", size = 8),
    axis.text     = element_text(color = "#cccccc", size = 9),
    axis.title    = element_text(color = "#888888", size = 10),
    legend.background = element_rect(fill = "#0d0d1a", color = NA),
    legend.text   = element_text(color = "white",   size = 8),
    legend.title  = element_text(color = "#aaaaaa", size = 9)
  )

pipeline_plot

ggsave(
  here(OUTPUT_DIR, "tuolumne_flow_patterns.png"),
  plot = pipeline_plot, width = 10, height = 8,
  dpi = 300, bg = "#0d0d1a"
)
cat("Saved: tuolumne_flow_patterns.png\n")


# --- Chart 3: Sierra county comparison ---
cat("Building Sierra comparison chart...\n")

sierra_plot <- all_sierra |>
  select(county, total_in, total_out, net) |>
  pivot_longer(cols = c(total_in, total_out),
               names_to = "direction",
               values_to = "people") |>
  mutate(
    people    = ifelse(direction == "total_out", -people, people),
    direction = ifelse(direction == "total_in", "Moving In", "Moving Out"),
    county    = fct_reorder(county, abs(people))
  ) |>
  
  ggplot(aes(x = people, y = county, fill = direction)) +
  
  geom_col(alpha = 0.85, width = 0.6) +
  geom_vline(xintercept = 0, color = "white", linewidth = 0.5) +
  
  # Net label
  geom_text(
    data = all_sierra,
    aes(x = 0, y = county,
        label = ifelse(net > 0,
                       paste0("net +", format(net, big.mark=",")),
                       paste0("net ", format(net, big.mark=","))),
        color = net > 0),
    inherit.aes = FALSE,
    vjust       = -0.8,
    size        = 3,
    fontface    = "bold"
  ) +
  
  scale_fill_manual(values = c("Moving In"  = "#7b2d8b",
                               "Moving Out" = "#cc4c02")) +
  scale_color_manual(values = c("TRUE" = "#44cc88",
                                "FALSE" = "#ff6666"),
                     guide = "none") +
  scale_x_continuous(
    labels = function(x) format(abs(x), big.mark = ","),
    breaks = pretty_breaks(6)
  ) +
  
  labs(
    title    = "Sierra Foothills Counties: Migration Comparison",
    subtitle = "ACS 5-Year Estimates 2016-2020  |  All five counties are net receivers",
    x        = "← Moving Out          Moving In →",
    y        = NULL,
    fill     = NULL,
    caption  = "Source: U.S. Census Bureau ACS 2016-2020"
  ) +
  
  theme_minimal() +
  theme(
    plot.background    = element_rect(fill = "#0d0d1a", color = NA),
    panel.background   = element_rect(fill = "#0d0d1a", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#2a2a3a"),
    panel.grid.minor   = element_blank(),
    plot.title    = element_text(color = "white",   face = "bold",
                                 size = 13, hjust = 0.5),
    plot.subtitle = element_text(color = "#aaaaaa", size = 9,
                                 hjust = 0.5),
    plot.caption  = element_text(color = "#555555", size = 8),
    axis.text     = element_text(color = "#cccccc", size = 10),
    axis.title.x  = element_text(color = "#888888", size = 9),
    legend.position = "bottom",
    legend.text   = element_text(color = "white")
  )

sierra_plot

ggsave(
  here(OUTPUT_DIR, "tuolumne_sierra_comparison.png"),
  plot = sierra_plot, width = 10, height = 6,
  dpi = 300, bg = "#0d0d1a"
)
cat("Saved: tuolumne_sierra_comparison.png\n")

cat("\n=== DEEP DIVE COMPLETE ===\n")
cat("Outputs saved to:", OUTPUT_DIR, "\n")
cat("  tuolumne_bayarea_deep.png\n")
cat("  tuolumne_flow_patterns.png\n")
cat("  tuolumne_sierra_comparison.png\n")

# =============================================================================
# END OF SCRIPT
# =============================================================================