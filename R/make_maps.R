# =============================================================================
# LEAFLET MAP BUILDERS
# =============================================================================

make_leaflet_map <- function(lines, lon, lat,
                             direction = "in",
                             county_name = "") {
  
  if (is.null(lines) || nrow(lines) == 0) {
    return(leaflet() |>
             addProviderTiles(providers$CartoDB.DarkMatter) |>
             setView(lon, lat, zoom = 5))
  }
  
  pal_colors <- if (direction == "in") "Blues" else "Reds"
  pal <- colorNumeric(pal_colors, domain = lines$estimate)
  
  popup_label <- if (direction == "in") "Moving to" else "Moving from"
  
  leaflet() |>
    addProviderTiles(providers$CartoDB.DarkMatter) |>
    setView(lng = lon, lat = lat, zoom = 4) |>
    addPolylines(
      data    = lines,
      color   = ~pal(estimate),
      weight  = ~rescale(estimate, to = c(0.5, 5)),
      opacity = ~rescale(estimate, to = c(0.4, 0.9)),
      popup   = ~paste0(
        "<b>", ifelse(direction == "in", "From: ", "To: "),
        "</b>", FULL2_NAME, "<br>",
        "<b>", popup_label, " ", county_name, ": </b>",
        format(estimate, big.mark = ","),
        "<br><small>MOE: ±",
        format(moe, big.mark = ","), "</small>"
      ),
      label   = ~paste0(
        str_remove(FULL2_NAME, ",.*$"), ": ",
        format(estimate, big.mark = ",")
      )
    ) |>
    addCircleMarkers(
      lng         = lon,
      lat         = lat,
      radius      = 7,
      color       = "white",
      weight      = 2,
      fillColor   = "yellow",
      fillOpacity = 0.9,
      popup       = paste0("<b>", county_name, "</b>")
    ) |>
    addLegend(
      position = "bottomright",
      pal      = pal,
      values   = lines$estimate,
      title    = ifelse(direction == "in",
                        "Moving In", "Moving Out"),
      opacity  = 0.8
    )
}