# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

# Auto-generate story bullets from migration data
generate_stories <- function(d) {
  
  stories <- list()
  
  # Net migration story
  net <- d$total_in - d$total_out
  stories[[1]] <- div(class = "story-card",
                      tags$b(ifelse(net >= 0, "📈 Net Gainer", "📉 Net Loser"),
                             style = ifelse(net >= 0, "color: #44cc88;",
                                            "color: #ff6666;")),
                      tags$p(paste0(
                        d$county_name, " had a net migration of ",
                        ifelse(net >= 0, "+", ""),
                        format(net, big.mark = ","),
                        " people over this period."
                      ), style = "color: #cccccc; margin: 6px 0 0 0;")
  )
  
  # Top origin
  if (nrow(d$inflow) > 0) {
    top_in <- d$inflow |> slice(1)
    stories[[2]] <- div(class = "story-card",
                        tags$b("🏆 Biggest Origin",
                               style = "color: #7b2d8b;"),
                        tags$p(paste0(
                          str_remove(top_in$FULL2_NAME, ",.*$"),
                          " sent the most people: ",
                          format(top_in$estimate, big.mark = ","),
                          " (±", format(top_in$moe, big.mark = ","), ")"
                        ), style = "color: #cccccc; margin: 6px 0 0 0;")
    )
  }
  
  # Top destination
  if (nrow(d$outflow) > 0) {
    top_out <- d$outflow |> slice(1)
    stories[[3]] <- div(class = "story-card",
                        tags$b("📍 Top Destination",
                               style = "color: #cc4c02;"),
                        tags$p(paste0(
                          str_remove(top_out$FULL2_NAME, ",.*$"),
                          " received the most: ",
                          format(top_out$estimate, big.mark = ","),
                          " (±", format(top_out$moe, big.mark = ","), ")"
                        ), style = "color: #cccccc; margin: 6px 0 0 0;")
    )
  }
  
  # In-state vs out-of-state
  stories[[4]] <- div(class = "story-card",
                      tags$b("🗺️ Geographic Reach",
                             style = "color: #aaaaaa;"),
                      tags$p(paste0(
                        round(d$pct_in_state), "% of arrivals came from within ",
                        "the same state. ",
                        round(100 - d$pct_in_state), "% came from out of state."
                      ), style = "color: #cccccc; margin: 6px 0 0 0;")
  )
  
  # One-way pipelines
  one_way_in <- d$inflow |>
    filter(!GEOID2 %in% d$outflow$GEOID2) |>
    filter(estimate >= 20)
  
  if (nrow(one_way_in) > 0) {
    stories[[5]] <- div(class = "story-card",
                        tags$b("🔀 One-Way Inflows",
                               style = "color: #7b2d8b;"),
                        tags$p(paste0(
                          nrow(one_way_in),
                          " counties send people here with no return flow ",
                          "(largest: ",
                          str_remove(one_way_in$FULL2_NAME[1], ",.*$"),
                          ", ", format(one_way_in$estimate[1], big.mark=","),
                          " people)."
                        ), style = "color: #cccccc; margin: 6px 0 0 0;")
    )
  }
  
  tagList(stories)
}