# =============================================================================
# TUOLUMNE COUNTY MIGRATION — TWITTER/SOCIAL IMAGES
# =============================================================================
# Produces three Twitter/X optimized images (16:9, 300dpi) combining the
# flow maps with a ranked data panel showing top counties.
#
# DEPENDS ON: tuolumne_migration.R
# Run tuolumne_migration.R first, OR uncomment the source() line below
# to run it automatically from scratch.
#
# Outputs (written to tuolumne-migration/outputs/):
#   - tuolumne_inflow_twitter.png    map + top 10 origins
#   - tuolumne_outflow_twitter.png   map + top 10 destinations
#   - tuolumne_combined_twitter.png  both flows + top 5 each
# =============================================================================


# =============================================================================
# SECTION 1: DEPENDENCIES
# =============================================================================

# Uncomment to run the main script automatically before this one:
# source(here::here("tuolumne-migration", "tuolumne_migration.R"))

# Install Twitter-specific packages if needed
packages_extra <- c(
  "patchwork",  # combine multiple ggplot panels into one image
  "ggtext"      # rich text / markdown in ggplot labels (future use)
)

packages_to_install <- packages_extra[
  !packages_extra %in% installed.packages()[, "Package"]
]

if (length(packages_to_install) > 0) install.packages(packages_to_install)

library(patchwork)
library(ggtext)
library(here)      # should already be loaded, but just in case

# --- Safety check ---
# Verify all objects from tuolumne_migration.R are present in memory.
# If anything is missing, stop early with a helpful message rather than
# failing halfway through with a cryptic error.

required_objects <- c(
  "inflow_lines",    # sf linestring layer of inflow arcs
  "outflow_lines",   # sf linestring layer of outflow arcs
  "inflow_df",       # plain dataframe of inflow county data
  "outflow_df",      # plain dataframe of outflow county data
  "us_states",       # sf layer of US state boundaries
  "tuol_lon",        # Tuolumne County longitude
  "tuol_lat",        # Tuolumne County latitude
  "dark_flow_theme", # shared ggplot dark theme
  "OUTPUT_DIR"       # path to outputs folder
)

missing_objects <- required_objects[
  !sapply(required_objects, exists)
]

if (length(missing_objects) > 0) {
  stop(
    "Missing objects from tuolumne_migration.R:\n  ",
    paste(missing_objects, collapse = "\n  "),
    "\n\nPlease run tuolumne_migration.R first, or uncomment the ",
    "source() line at the top of this script."
  )
}

cat("All required objects found. Building Twitter images...\n")


# =============================================================================
# SECTION 2: HELPER — BUILD RANKINGS PANEL
# =============================================================================
# Returns a styled ggplot bar chart of the top N counties by migration volume.
# Called once for inflow and once for outflow.
#
# Arguments:
#   df        = inflow_df or outflow_df
#   direction = "IN" or "OUT" (controls colors, labels, and arrow direction)
#   n         = number of counties to show (default 10)

make_rankings_panel <- function(df, direction = "IN", n = 10) {
  
  # Accent color and labels change depending on flow direction
  accent_color <- if (direction == "IN") "#7b2d8b" else "#cc4c02"
  arrow        <- if (direction == "IN") "→ Tuolumne" else "Tuolumne →"
  header_label <- if (direction == "IN") "TOP ORIGINS" else "TOP DESTINATIONS"
  
  # Prepare top-N rows with display-ready fields
  # "Los Angeles County, California" --> "Los Angeles County"
  top_n_df <- df |>
    head(n) |>
    mutate(
      rank       = row_number(),
      short_name = str_remove(FULL2_NAME, ",.*$"),  # drop ", State" suffix
      short_name = str_trunc(short_name, 28),        # cap very long names
      bar_width  = estimate / max(estimate)           # normalize to 0-1 for bars
    )
  
  ggplot(top_n_df, aes(y = reorder(short_name, estimate))) +
    
    # Full-width background bar (subtle dark fill)
    geom_col(aes(x = 1),
             fill  = "#1e1e30",
             width = 0.75) +
    
    # Foreground bar scaled to migration estimate
    geom_col(aes(x = bar_width),
             fill  = accent_color,
             alpha = 0.85,
             width = 0.75) +
    
    # Rank number (#1, #2, ...) at left edge
    geom_text(aes(x = 0, label = paste0("#", rank)),
              hjust    = 0,
              nudge_x  = 0.01,
              color    = "#888888",
              size     = 3.2,
              fontface = "bold") +
    
    # County name
    geom_text(aes(x = 0.08, label = short_name),
              hjust = 0,
              color = "white",
              size  = 3.2) +
    
    # Migration count at right edge
    geom_text(aes(x = 1, label = format(estimate, big.mark = ",")),
              hjust    = 1,
              nudge_x  = -0.01,
              color    = "#dddddd",
              size     = 3.2,
              fontface = "bold") +
    
    # Flow direction label above the bars
    annotate("text",
             x        = 0.5,
             y        = n + 0.8,
             label    = arrow,
             color    = accent_color,
             size     = 3.5,
             fontface = "italic") +
    
    scale_x_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
    scale_y_discrete(expand = expansion(add = c(0.5, 1.2))) +
    
    labs(title = header_label) +
    
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "#0d0d1a", color = NA),
      panel.background = element_rect(fill = "#0d0d1a", color = NA),
      plot.title = element_text(
        color    = accent_color,
        size     = 11,
        face     = "bold",
        hjust    = 0.5,
        margin   = margin(b = 8, t = 4)
      ),
      plot.margin = margin(10, 12, 10, 12)
    )
}


# =============================================================================
# SECTION 3: SHARED ANNOTATION HELPER
# =============================================================================
# Both map panels need the same patchwork plot_annotation() theme block.
# Define it once as a function to avoid repetition.

twitter_annotation_theme <- function(title_text) {
  plot_annotation(
    title   = title_text,
    caption = paste0(
      "Data: U.S. Census Bureau ACS 2016\u20132020  |  ",
      "github.com/YOURUSERNAME/census-r-experiments"
    ),
    theme = theme(
      plot.background = element_rect(fill = "#0d0d1a", color = NA),
      plot.title = element_text(
        color    = "white",
        size     = 18,
        face     = "bold",
        hjust    = 0.5,
        margin   = margin(t = 12, b = 6)
      ),
      plot.caption = element_text(
        color  = "#555555",
        size   = 7.5,
        hjust  = 0.5,
        margin = margin(t = 4, b = 8)
      )
    )
  )
}

# Shared legend positioning — used on both single-flow map panels
legend_inside_theme <- theme(
  legend.position   = c(0.85, 0.08),
  legend.direction  = "horizontal",
  legend.key.width  = unit(1.2, "cm"),
  legend.key.height = unit(0.25, "cm"),
  plot.subtitle     = element_text(
    color  = "#aaaaaa",
    size   = 9,
    hjust  = 0.5,
    margin = margin(t = 2, b = 4)
  )
)


# =============================================================================
# SECTION 4: INFLOW TWITTER IMAGE
# =============================================================================
# Map (70% width) + top 10 origins ranked panel (30% width)

cat("Building inflow Twitter image...\n")

inflow_map_panel <- ggplot() +
  
  geom_sf(data = us_states, fill = "#1a1a2e",
          color = "#2d2d44", linewidth = 0.3) +
  
  geom_sf(data = inflow_lines,
          aes(color = estimate, linewidth = estimate, alpha = estimate)) +
  
  annotate("point", x = tuol_lon, y = tuol_lat,
           color = "yellow", size = 3) +
  
  annotate("text", x = tuol_lon + 1.2, y = tuol_lat + 0.7,
           label = "Tuolumne Co.",
           color = "yellow", size = 2.3, hjust = 0) +
  
  scale_color_viridis_c(option = "plasma", labels = scales::comma,
                        name = "People moving in") +
  scale_linewidth_continuous(range = c(0.3, 3.5), guide = "none") +
  scale_alpha_continuous(range    = c(0.4, 1.0),  guide = "none") +
  
  coord_sf(xlim = c(-125, -65), ylim = c(24, 50)) +
  
  labs(subtitle = "ACS 5-Year Estimates, 2016\u20132020") +
  
  dark_flow_theme +
  legend_inside_theme

inflow_rankings <- make_rankings_panel(inflow_df, direction = "IN", n = 10)

inflow_twitter <- (inflow_map_panel | inflow_rankings) +
  plot_layout(widths = c(0.70, 0.30)) +
  twitter_annotation_theme("Where Do Tuolumne County's New Residents Come From?")

inflow_twitter

ggsave(
  filename = here(OUTPUT_DIR, "tuolumne_inflow_twitter.png"),
  plot     = inflow_twitter,
  width    = 12,
  height   = 6.75,   # 16:9
  dpi      = 300,
  bg       = "#0d0d1a"
)
cat("Saved:", here(OUTPUT_DIR, "tuolumne_inflow_twitter.png"), "\n")


# =============================================================================
# SECTION 5: OUTFLOW TWITTER IMAGE
# =============================================================================
# Map (70% width) + top 10 destinations ranked panel (30% width)

cat("Building outflow Twitter image...\n")

outflow_map_panel <- ggplot() +
  
  geom_sf(data = us_states, fill = "#1a1a2e",
          color = "#2d2d44", linewidth = 0.3) +
  
  geom_sf(data = outflow_lines,
          aes(color = estimate, linewidth = estimate, alpha = estimate)) +
  
  annotate("point", x = tuol_lon, y = tuol_lat,
           color = "yellow", size = 3) +
  
  annotate("text", x = tuol_lon + 1.2, y = tuol_lat + 0.7,
           label = "Tuolumne Co.",
           color = "yellow", size = 2.3, hjust = 0) +
  
  scale_color_viridis_c(option = "inferno", labels = scales::comma,
                        name = "People moving out") +
  scale_linewidth_continuous(range = c(0.3, 3.5), guide = "none") +
  scale_alpha_continuous(range    = c(0.4, 1.0),  guide = "none") +
  
  coord_sf(xlim = c(-125, -65), ylim = c(24, 50)) +
  
  labs(subtitle = "ACS 5-Year Estimates, 2016\u20132020") +
  
  dark_flow_theme +
  legend_inside_theme

outflow_rankings <- make_rankings_panel(outflow_df, direction = "OUT", n = 10)

outflow_twitter <- (outflow_map_panel | outflow_rankings) +
  plot_layout(widths = c(0.70, 0.30)) +
  twitter_annotation_theme("Where Do Tuolumne County Residents Move To?")

outflow_twitter

ggsave(
  filename = here(OUTPUT_DIR, "tuolumne_outflow_twitter.png"),
  plot     = outflow_twitter,
  width    = 12,
  height   = 6.75,
  dpi      = 300,
  bg       = "#0d0d1a"
)
cat("Saved:", here(OUTPUT_DIR, "tuolumne_outflow_twitter.png"), "\n")


# =============================================================================
# SECTION 6: COMBINED TWITTER IMAGE (both flows, top 5 each)
# =============================================================================
# One map showing both inflow (purple) and outflow (orange) arcs,
# with a split rankings panel showing top 5 for each direction.
# Good for a single tweet that tells the whole story at once.

cat("Building combined Twitter image...\n")

both_rankings <- (
  make_rankings_panel(inflow_df,  direction = "IN",  n = 5) |
    make_rankings_panel(outflow_df, direction = "OUT", n = 5)
)

both_map_panel <- ggplot() +
  
  geom_sf(data = us_states, fill = "#1a1a2e",
          color = "#2d2d44", linewidth = 0.3) +
  
  # Draw outflow first so inflow arcs sit on top where they overlap
  geom_sf(data = outflow_lines,
          aes(linewidth = estimate, alpha = estimate),
          color = "#cc4c02") +
  
  geom_sf(data = inflow_lines,
          aes(linewidth = estimate, alpha = estimate),
          color = "#7b2d8b") +
  
  annotate("point", x = tuol_lon, y = tuol_lat,
           color = "yellow", size = 3) +
  
  annotate("text", x = tuol_lon + 1.2, y = tuol_lat + 0.7,
           label = "Tuolumne Co.",
           color = "yellow", size = 2.3, hjust = 0) +
  
  # Manual color legend (replaces ggplot default since we used fixed colors)
  annotate("segment",
           x = -123, xend = -121, y = 26.5, yend = 26.5,
           color = "#7b2d8b", linewidth = 1.5) +
  annotate("text", x = -120.5, y = 26.5,
           label = "Moving IN", color = "#7b2d8b",
           size = 3, hjust = 0) +
  
  annotate("segment",
           x = -123, xend = -121, y = 25.2, yend = 25.2,
           color = "#cc4c02", linewidth = 1.5) +
  annotate("text", x = -120.5, y = 25.2,
           label = "Moving OUT", color = "#cc4c02",
           size = 3, hjust = 0) +
  
  scale_linewidth_continuous(range = c(0.3, 3.5), guide = "none") +
  scale_alpha_continuous(range    = c(0.4, 0.9),  guide = "none") +
  
  coord_sf(xlim = c(-125, -65), ylim = c(24, 50)) +
  
  labs(subtitle = "ACS 5-Year Estimates, 2016\u20132020") +
  
  dark_flow_theme +
  theme(
    plot.subtitle = element_text(color = "#aaaaaa", size = 9,
                                 hjust = 0.5, margin = margin(t = 2, b = 4))
  )

combined_twitter <- (both_map_panel | both_rankings) +
  plot_layout(widths = c(0.60, 0.40)) +
  twitter_annotation_theme("Tuolumne County, CA \u2014 Migration In & Out")

combined_twitter

ggsave(
  filename = here(OUTPUT_DIR, "tuolumne_combined_twitter.png"),
  plot     = combined_twitter,
  width    = 12,
  height   = 6.75,
  dpi      = 300,
  bg       = "#0d0d1a"
)
cat("Saved:", here(OUTPUT_DIR, "tuolumne_combined_twitter.png"), "\n")


# =============================================================================
# DONE
# =============================================================================

cat("\nAll Twitter images written to:", OUTPUT_DIR, "\n")
cat("  tuolumne_inflow_twitter.png\n")
cat("  tuolumne_outflow_twitter.png\n")
cat("  tuolumne_combined_twitter.png\n")

# =============================================================================
# END OF SCRIPT
# =============================================================================