# =============================================================================
# LEAFLET MAP BUILDERS
# =============================================================================
# Builds interactive leaflet maps showing migration arc lines.
# Uses direct CartoDB tile URL (no leaflet.providers package needed).
# Falls back to OpenStreetMap if dark tiles don't load.

make_leaflet_map <- function(lines, lon, lat,
                             direction   = "in",
                             county_name = "") {
  
  # Color palette based on flow direction
  # Blue tones for inflow, red/orange tones for outflow
  pal_colors <- if (direction == "in") "Blues" else "Reds"
  
  # Build base map with dark CartoDB tiles via direct URL
  # This works without the leaflet.providers package
  base_map <- leaflet() |>
    
    # Dark tile layer (primary)
    addTiles(
      urlTemplate = paste0(
        "https://{s}.basemaps.cartocdn.com/dark_all/",
        "{z}/{x}/{y}{r}.png"
      ),
      attribution = paste0(
        "\u00a9 ",
        "<a href='https://www.openstreetmap.org/copyright'>",
        "OpenStreetMap</a> contributors \u00a9 ",
        "<a href='https://carto.com/attributions'>CARTO</a>"
      ),
      options = tileOptions(
        subdomains = "abcd",
        maxZoom    = 19
      ),
      group = "Dark"
    ) |>
    
    # Standard OSM tiles (fallback if dark tiles blocked by network)
    addTiles(group = "Street") |>
    
    # Layer toggle — collapsed so it doesn't clutter the map
    addLayersControl(
      baseGroups = c("Dark", "Street"),
      options    = layersControlOptions(collapsed = TRUE)
    )
  
  # If no arc lines, return base map with just the county marker
  if (is.null(lines) || nrow(lines) == 0) {
    return(
      base_map |>
        setView(lng = lon, lat = lat, zoom = 5) |>
        addCircleMarkers(
          lng         = lon,
          lat         = lat,
          radius      = 8,
          color       = "white",
          weight      = 2,
          fillColor   = "yellow",
          fillOpacity = 0.9,
          popup       = paste0("<b>", county_name, "</b>")
        )
    )
  }
  
  # Build color palette from actual estimate range
  pal <- colorNumeric(pal_colors, domain = lines$estimate)
  
  direction_label <- if (direction == "in") "Moving to" else "Moving from"
  
  base_map |>
    setView(lng = lon, lat = lat, zoom = 4) |>
    
    # Arc lines — color, weight, and opacity all scale with migrant count
    addPolylines(
      data    = lines,
      color   = ~pal(estimate),
      weight  = ~rescale(estimate, to = c(0.5, 5)),
      opacity = ~rescale(estimate, to = c(0.4, 0.9)),
      popup   = ~paste0(
        "<b>",
        ifelse(direction == "in", "From: ", "To: "),
        "</b>", FULL2_NAME, "<br>",
        "<b>", direction_label, " ", county_name, ": </b>",
        format(estimate, big.mark = ","),
        "<br><small>MOE: \u00b1",
        format(moe, big.mark = ","), "</small>"
      ),
      label = ~paste0(
        str_remove(FULL2_NAME, ",.*$"), ": ",
        format(estimate, big.mark = ",")
      )
    ) |>
    
    # Yellow dot marking the selected county
    addCircleMarkers(
      lng         = lon,
      lat         = lat,
      radius      = 8,
      color       = "white",
      weight      = 2,
      fillColor   = "yellow",
      fillOpacity = 0.9,
      popup       = paste0("<b>", county_name, "</b>")
    ) |>
    
    # Legend
    addLegend(
      position = "bottomright",
      pal      = pal,
      values   = lines$estimate,
      title    = ifelse(direction == "in", "Moving In", "Moving Out"),
      opacity  = 0.8
    )
}