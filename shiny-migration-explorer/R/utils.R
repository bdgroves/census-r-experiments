# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================
# All dplyr/stringr functions use explicit package::function() notation
# because this file is source()d before library() calls take effect
# on shinyapps.io. Explicit namespaces guarantee functions are found
# regardless of load order.

# Auto-generate story bullets from migration data
# Called by output$stories_panel in app.R
# Returns a tagList of styled div cards for display in the UI

generate_stories <- function(d) {
  
  stories <- list()
  
  # ------------------------------------------------------------------
  # Story 1: Net migration — is this county gaining or losing people?
  # ------------------------------------------------------------------
  net <- d$total_in - d$total_out
  
  stories[[1]] <- div(
    class = "story-card",
    tags$b(
      ifelse(net >= 0,
             "\U0001F4C8 Net Gainer",   # unicode for 📈
             "\U0001F4C9 Net Loser"),    # unicode for 📉
      style = ifelse(net >= 0,
                     "color: #44cc88;",
                     "color: #ff6666;")
    ),
    tags$p(
      paste0(
        d$county_name, " had a net migration of ",
        ifelse(net >= 0, "+", ""),
        format(net, big.mark = ","),
        " people over this period."
      ),
      style = "color: #cccccc; margin: 6px 0 0 0;"
    )
  )
  
  # ------------------------------------------------------------------
  # Story 2: Biggest origin county
  # ------------------------------------------------------------------
  if (!is.null(d$inflow) && nrow(d$inflow) > 0) {
    
    top_in <- dplyr::slice(d$inflow, 1)
    
    stories[[2]] <- div(
      class = "story-card",
      tags$b(
        "\U0001F3C6 Biggest Origin",    # unicode for 🏆
        style = "color: #7b2d8b;"
      ),
      tags$p(
        paste0(
          stringr::str_remove(top_in$FULL2_NAME, ",.*$"),
          " sent the most people: ",
          format(top_in$estimate, big.mark = ","),
          " (\u00b1", format(top_in$moe, big.mark = ","), ")"  # ±
        ),
        style = "color: #cccccc; margin: 6px 0 0 0;"
      )
    )
  }
  
  # ------------------------------------------------------------------
  # Story 3: Top destination county
  # ------------------------------------------------------------------
  if (!is.null(d$outflow) && nrow(d$outflow) > 0) {
    
    top_out <- dplyr::slice(d$outflow, 1)
    
    stories[[3]] <- div(
      class = "story-card",
      tags$b(
        "\U0001F4CD Top Destination",   # unicode for 📍
        style = "color: #cc4c02;"
      ),
      tags$p(
        paste0(
          stringr::str_remove(top_out$FULL2_NAME, ",.*$"),
          " received the most: ",
          format(top_out$estimate, big.mark = ","),
          " (\u00b1", format(top_out$moe, big.mark = ","), ")"
        ),
        style = "color: #cccccc; margin: 6px 0 0 0;"
      )
    )
  }
  
  # ------------------------------------------------------------------
  # Story 4: In-state vs out-of-state share
  # ------------------------------------------------------------------
  stories[[4]] <- div(
    class = "story-card",
    tags$b(
      "\U0001F5FA\uFE0F Geographic Reach",  # unicode for 🗺️
      style = "color: #aaaaaa;"
    ),
    tags$p(
      paste0(
        round(d$pct_in_state),
        "% of arrivals came from within the same state. ",
        round(100 - d$pct_in_state),
        "% came from out of state."
      ),
      style = "color: #cccccc; margin: 6px 0 0 0;"
    )
  )
  
  # ------------------------------------------------------------------
  # Story 5: One-way inflow pipelines
  # Counties that send people here but receive nobody back
  # ------------------------------------------------------------------
  if (!is.null(d$inflow)  && nrow(d$inflow)  > 0 &&
      !is.null(d$outflow) && nrow(d$outflow) > 0) {
    
    one_way_in <- d$inflow |>
      dplyr::filter(!GEOID2 %in% d$outflow$GEOID2) |>
      dplyr::filter(estimate >= 20)
    
    if (nrow(one_way_in) > 0) {
      
      stories[[5]] <- div(
        class = "story-card",
        tags$b(
          "\U0001F500 One-Way Inflows",  # unicode for 🔀
          style = "color: #7b2d8b;"
        ),
        tags$p(
          paste0(
            nrow(one_way_in),
            " counties send people here with no return flow ",
            "(largest: ",
            stringr::str_remove(one_way_in$FULL2_NAME[1], ",.*$"),
            ", ",
            format(one_way_in$estimate[1], big.mark = ","),
            " people)."
          ),
          style = "color: #cccccc; margin: 6px 0 0 0;"
        )
      )
    }
  }
  
  tagList(stories)
}