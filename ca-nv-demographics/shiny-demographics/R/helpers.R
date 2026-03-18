# =============================================================================
# HELPER FUNCTIONS
# =============================================================================
# Utility functions for the CA/NV Demographics Shiny app.
# Uses explicit package::function() notation throughout.

# Format a numeric value for display based on variable type
format_demo_value <- function(x, var_name) {
  is_dollar  <- grepl("income|rent", var_name, ignore.case = TRUE)
  is_percent <- grepl("rate|pct|percent|burden|born|occupied|vacancy|moved|bach",
                      var_name, ignore.case = TRUE)
  dplyr::case_when(
    is_dollar  ~ paste0("$", format(round(x), big.mark = ",")),
    is_percent ~ paste0(round(x, 1), "%"),
    TRUE       ~ as.character(round(x, 1))
  )
}

# Get palette function by name
get_palette_fn <- function(palette_name) {
  switch(palette_name,
         "viridis" = viridisLite::viridis,
         "inferno" = viridisLite::inferno,
         "plasma"  = viridisLite::plasma,
         "magma"   = viridisLite::magma,
         "cividis" = viridisLite::cividis,
         viridisLite::viridis  # default
  )
}