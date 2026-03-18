# =============================================================================
# MAP BUILDER FUNCTIONS
# =============================================================================
# Standalone map building functions used by explore.R for prototyping
# and available for standalone testing outside of Shiny.
#
# The Shiny app (app.R) builds maps directly in renderMaplibre()
# for finer control — these functions are for quick standalone use.
#
# All functions use explicit package::function() notation because
# this file is source()d before library() calls take effect
# on shinyapps.io.
#
# Tooltip notes:
#   tooltip (hover) = must use <br> NOT \n — mapgl renders as HTML
#   popup   (click) = full HTML, supports all styling
#
# NAME column note:
#   ACS format: "Census Tract X; County Name; State"
#   Uses semicolons — extract with "^[^;]+" and "(?<=; ).*(?=; )"
#   NOT commas — comma-based regex returns NA for all rows

# =============================================================================
# BUILD A STANDALONE DEMO MAP
# =============================================================================
# Arguments:
#   data     sf dataframe — must have tract_name, county, state columns
#   col      column name to map (string)
#   label    legend and tooltip label (human readable string)
#   palette  viridis palette: "viridis","inferno","plasma","magma","cividis"
#   method   classification: "quantile","equal","jenks"
#   style    carto basemap: "dark-matter","positron","voyager"
#   cap_at   optional numeric — cap extreme values (e.g. rent_burden = 150)

build_demo_map <- function(data,
                           col,
                           label,
                           palette = "viridis",
                           method  = "quantile",
                           style   = "dark-matter",
                           cap_at  = NULL) {
  
  pal_fn <- get_palette_fn(palette)
  
  # Cap outliers if requested
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
  
  # Determine formatting type
  is_dollar <- grepl("\\$|Income|Rent", label, ignore.case = TRUE)
  is_pct    <- grepl(
    "%|Rate|Born|Burden|Occupied|Vacancy|Moved|bach",
    label, ignore.case = TRUE
  )
  
  # Build both tooltip columns
  data_tt <- data |>
    dplyr::mutate(
      
      # Hover tooltip — use <br> not \n (mapgl renders as HTML)
      tooltip_plain = {
        raw_val <- .data[[col]]
        formatted_val <- dplyr::case_when(
          is.na(raw_val) ~ "No data",
          is_dollar      ~ paste0(
            "$", format(round(raw_val), big.mark = ",")
          ),
          is_pct         ~ paste0(round(raw_val, 1), "%"),
          TRUE           ~ as.character(round(raw_val, 1))
        )
        
        # Use pre-built tract_name/county if available,
        # fall back to NAME extraction
        t_name <- if ("tract_name" %in% names(data)) {
          tract_name
        } else {
          stringr::str_trim(stringr::str_extract(NAME, "^[^;]+"))
        }
        c_name <- if ("county" %in% names(data) &&
                      !all(is.na(data$county))) {
          county
        } else {
          stringr::str_extract(NAME, "(?<=; ).*(?=; )")
        }
        
        paste0(
          t_name, "<br>",
          c_name, ", ", state, "<br>",
          label, ": <strong>", formatted_val, "</strong>"
        )
      },
      
      # Click popup — full styled HTML
      tooltip_html = {
        raw_val <- .data[[col]]
        formatted_val <- dplyr::case_when(
          is.na(raw_val) ~ "No data",
          is_dollar      ~ paste0(
            "$", format(round(raw_val), big.mark = ",")
          ),
          is_pct         ~ paste0(round(raw_val, 1), "%"),
          TRUE           ~ as.character(round(raw_val, 1))
        )
        
        t_name <- if ("tract_name" %in% names(data)) {
          tract_name
        } else {
          stringr::str_trim(stringr::str_extract(NAME, "^[^;]+"))
        }
        c_name <- if ("county" %in% names(data) &&
                      !all(is.na(data$county))) {
          county
        } else {
          stringr::str_extract(NAME, "(?<=; ).*(?=; )")
        }
        
        paste0(
          "<div style='",
          "font-family: sans-serif;",
          "font-size: 13px;",
          "line-height: 1.6;",
          "min-width: 200px;",
          "padding: 4px 2px;",
          "'>",
          "<strong style='font-size:14px;'>",
          t_name, "</strong><br>",
          "<span style='color:#666666; font-size:11px;'>",
          c_name, ", ", state,
          "</span>",
          "<hr style='margin:6px 0; border-color:#eeeeee;'>",
          "<span style='color:#444444;'>", label, "</span><br>",
          "<strong style='font-size:16px; color:#2d8b7b;'>",
          formatted_val, "</strong>",
          "</div>"
        )
      }
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
      tooltip            = "tooltip_plain",   # hover
      popup              = "tooltip_html"     # click
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