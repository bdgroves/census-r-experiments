# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================
# All dplyr/stringr functions use explicit package::function() notation
# because this file is source()d before library() calls take effect
# on shinyapps.io.

# =============================================================================
# GENERATE STORY BULLETS
# =============================================================================
# Auto-generates insight cards from migration data.
# Called by output$stories_panel in app.R.

generate_stories <- function(d) {
  
  stories <- list()
  
  # ------------------------------------------------------------------
  # Story 1: Net migration
  # ------------------------------------------------------------------
  net <- d$total_in - d$total_out
  
  stories[[1]] <- div(
    class = "story-card",
    tags$b(
      ifelse(net >= 0,
             "\U0001F4C8 Net Gainer",
             "\U0001F4C9 Net Loser"),
      style = ifelse(net >= 0, "color: #44cc88;", "color: #ff6666;")
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
    cv_in  <- top_in$moe / top_in$estimate
    
    # Flag reliability of the top finding itself
    reliability_note <- dplyr::case_when(
      cv_in <= 0.30 ~ "",
      cv_in <= 0.50 ~ " \u26a0\ufe0f estimate has high uncertainty",
      TRUE          ~ " \u26a0\ufe0f estimate is unreliable (MOE \u2265 50%)"
    )
    
    stories[[2]] <- div(
      class = "story-card",
      tags$b(
        "\U0001F3C6 Biggest Origin",
        style = "color: #7b2d8b;"
      ),
      tags$p(
        paste0(
          stringr::str_remove(top_in$FULL2_NAME, ",.*$"),
          " sent the most people: ",
          format(top_in$estimate, big.mark = ","),
          " (\u00b1", format(top_in$moe, big.mark = ","), ")",
          reliability_note
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
    cv_out  <- top_out$moe / top_out$estimate
    
    reliability_note <- dplyr::case_when(
      cv_out <= 0.30 ~ "",
      cv_out <= 0.50 ~ " \u26a0\ufe0f estimate has high uncertainty",
      TRUE           ~ " \u26a0\ufe0f estimate is unreliable (MOE \u2265 50%)"
    )
    
    stories[[3]] <- div(
      class = "story-card",
      tags$b(
        "\U0001F4CD Top Destination",
        style = "color: #cc4c02;"
      ),
      tags$p(
        paste0(
          stringr::str_remove(top_out$FULL2_NAME, ",.*$"),
          " received the most: ",
          format(top_out$estimate, big.mark = ","),
          " (\u00b1", format(top_out$moe, big.mark = ","), ")",
          reliability_note
        ),
        style = "color: #cccccc; margin: 6px 0 0 0;"
      )
    )
  }
  
  # ------------------------------------------------------------------
  # Story 4: In-state vs out-of-state
  # ------------------------------------------------------------------
  stories[[4]] <- div(
    class = "story-card",
    tags$b(
      "\U0001F5FA\uFE0F Geographic Reach",
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
          "\U0001F500 One-Way Inflows",
          style = "color: #7b2d8b;"
        ),
        tags$p(
          paste0(
            nrow(one_way_in),
            " counties send people here with no recorded return flow ",
            "(largest: ",
            stringr::str_remove(one_way_in$FULL2_NAME[1], ",.*$"),
            ", ",
            format(one_way_in$estimate[1], big.mark = ","),
            " people). Note: zero return flow may reflect ",
            "sample gaps rather than a true one-way pattern."
          ),
          style = "color: #cccccc; margin: 6px 0 0 0;"
        )
      )
    }
  }
  
  # ------------------------------------------------------------------
  # Story 6: Reliability summary
  # How much of this county's data can we actually trust?
  # ------------------------------------------------------------------
  if (!is.null(d$inflow) && nrow(d$inflow) > 0) {
    
    all_flows <- bind_rows(
      d$inflow  |> dplyr::mutate(dir = "in"),
      d$outflow |> dplyr::mutate(dir = "out")
    )
    
    cv_summary <- all_flows |>
      dplyr::mutate(cv = moe / estimate) |>
      dplyr::summarise(
        high_conf  = sum(cv <= 0.30, na.rm = TRUE),
        unreliable = sum(cv >  0.50, na.rm = TRUE),
        total      = dplyr::n()
      )
    
    pct_unreliable <- round(
      cv_summary$unreliable / cv_summary$total * 100
    )
    
    reliability_color <- dplyr::case_when(
      pct_unreliable >= 80 ~ "#ff6666",
      pct_unreliable >= 50 ~ "#ffaa44",
      TRUE                 ~ "#44cc88"
    )
    
    stories[[6]] <- div(
      class = "story-card",
      style = "border-left-color: #444466;",
      tags$b(
        "\u26a0\ufe0f Data Reliability",
        style = paste0("color: ", reliability_color, ";")
      ),
      tags$p(
        paste0(
          pct_unreliable, "% of individual flow estimates ",
          "have margins of error exceeding 50% of the estimate. ",
          "This is normal for ACS migration data — ",
          "the survey samples only a fraction of households. ",
          "Small flows (under ~50 people) may represent ",
          "just 1-2 survey respondents weighted up. ",
          "Focus on patterns across many counties ",
          "rather than any single number."
        ),
        style = "color: #aaaaaa; margin: 6px 0 0 0; font-size: 0.85em;"
      )
    )
  }
  
  tagList(stories)
}


# =============================================================================
# RELIABILITY CLASSIFICATION
# =============================================================================
# Used by make_charts.R and potentially other functions.
# Returns a human-readable reliability grade for a given estimate + MOE.

classify_reliability <- function(estimate, moe) {
  cv <- moe / estimate
  dplyr::case_when(
    estimate >= 200 & cv <= 0.30 ~ "High",
    estimate >= 100 & cv <= 0.50 ~ "Moderate",
    estimate >=  50 & cv <= 0.75 ~ "Low",
    estimate >=  20              ~ "Very low",
    TRUE                         ~ "Unreliable"
  )
}