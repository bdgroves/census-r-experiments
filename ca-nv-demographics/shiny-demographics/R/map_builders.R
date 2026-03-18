# =============================================================================
# MAP BUILDER FUNCTIONS
# =============================================================================
# Standalone map building functions used by explore.R for prototyping.
#
# All functions use explicit package::function() notation because
# this file is source()d before library() calls take effect
# on shinyapps.io.
#
# Tooltip note:
#   popup (click) = full HTML — works reliably in both standalone + Shiny
#   tooltip (hover) = removed — unreliable in Shiny context
#
# NAME column note:
#   ACS format: "Census Tract X; County Name; State"
#   Uses semicolons — extract with "^[^;]+" and "(?<=; ).*(?=; )"

build_demo_map <- function(data,
                           col,
                           label,
                           palette = "viridis",
                           method  = "quantile",
                           style   = "dark-matter",
                           cap_at  = NULL) {
  
  pal_fn <- get_palette_fn(palette)
  
  if (!is.null(cap_at)) {
    data <- data |>
      dplyr::mutate(!!col := pmin(.data[[col]], cap_at))
  }
  
  clean <- data |> dplyr::filter(!is.na(.data[[col]]))
  
  scale <- mapgl::interpolate_palette(
    data    = clean,
    column  = col,
    method  = method,
    n       = 5,
    palette = pal_fn
  )
  
  is_dollar <- grepl("\\$|Income|Rent", label, ignore.case = TRUE)
  is_pct    <- grepl(
    "%|Rate|Born|Burden|Occupied|Vacancy|Moved|bach",
    label, ignore.case = TRUE
  )
  
  # Resolve tract_name and county — use pre-built if available
  has_tract  <- "tract_name" %in% names(data)
  has_county <- "county" %in% names(data) && !all(is.na(data$county))
  
  data_tt <- data |>
    dplyr::mutate(
      
      # Resolve display labels
      .t_name = if (has_tract) {
        tract_name
      } else {
        stringr::str_trim(stringr::str_extract(NAME, "^[^;]+"))
      },
      
      .c_name = if (has_county) {
        county
      } else {
        stringr::str_extract(NAME, "(?<=; ).*(?=; )")
      },
      
      # Format the value
      .fmt_val = {
        raw_val <- .data[[col]]
        dplyr::case_when(
          is.na(raw_val) ~ "No data",
          is_dollar      ~ paste0(
            "$", format(round(raw_val), big.mark = ",")
          ),
          is_pct         ~ paste0(round(raw_val, 1), "%"),
          TRUE           ~ as.character(round(raw_val, 1))
        )
      },
      
      # Click popup — styled HTML, works in both standalone and Shiny
      popup_html = paste0(
        "<div style='",
        "font-family: sans-serif;",
        "font-size: 13px;",
        "line-height: 1.6;",
        "min-width: 200px;",
        "padding: 4px 2px;",
        "'>",
        "<strong style='font-size:14px;'>",
        .t_name, "</strong><br>",
        "<span style='color:#666666; font-size:11px;'>",
        .c_name, ", ", state,
        "</span>",
        "<hr style='margin:6px 0; border-color:#eeeeee;'>",
        "<span style='color:#444444;'>", label, "</span><br>",
        "<strong style='font-size:16px; color:#2d8b7b;'>",
        .fmt_val, "</strong>",
        "</div>"
      )
    )
  
  mapgl::maplibre(
    bounds = data_tt,
    style  = mapgl::carto_style(style)
  ) |>
    mapgl::add_fill_layer(
      id                 = "demo_layer",
      source             = data_tt,
      fill_color         = scale$expression,
      fill_opacity       = 0.75,
      fill_outline_color = "rgba(0,0,0,0.08)",
      popup              = "popup_html"   # click only
    ) |>
    mapgl::add_legend(
      label,
      values        = scale$breaks,
      colors        = scale$colors,
      type          = "continuous",
      layer_id      = "demo_layer",
      interactive   = TRUE,
      filter_column = col,
      position      = "bottom-left"
    ) |>
    mapgl::add_screenshot_control(
      position    = "top-right",
      image_scale = 2
    )
}