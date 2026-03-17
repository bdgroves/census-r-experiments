# =============================================================================
# MIGRATION EXPLORER — SHINY APP
# =============================================================================
# Interactive county-to-county migration explorer using Census ACS data.
# Users select any US county and get flow maps, charts, and summary stats.
#
# Run with: shiny::runApp("shiny-migration-explorer")
# =============================================================================

library(shiny)
library(bslib)          # modern Bootstrap themes
library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(geosphere)
library(scales)
library(ggplot2)
library(plotly)         # interactive charts
library(DT)             # interactive tables
library(here)
library(waiter)         # loading screens

# Load helper functions
source("R/get_migration.R")
source("R/make_maps.R")
source("R/make_charts.R")
source("R/utils.R")

# Create cache directory if it doesn't exist
if (!dir.exists("data/cache")) dir.create("data/cache", recursive = TRUE)

# =============================================================================
# STATE AND COUNTY REFERENCE DATA
# =============================================================================
# Load FIPS lookup — used to populate dropdowns
# Built from tidycensus::fips_codes which ships with the package

county_lookup <- tidycensus::fips_codes |>
  mutate(
    state_fips  = state_code,
    county_fips = county_code,
    full_fips   = paste0(state_code, county_code),
    display     = paste0(county, ", ", state)
  ) |>
  filter(!state %in% c("PR", "UM", "VI", "GU", "AS", "MP")) |>
  arrange(state_name, county)

state_choices <- county_lookup |>
  distinct(state_name, state) |>
  arrange(state_name) |>
  tibble::deframe()   # named vector: "California" = "CA"

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title = "🗺️ Migration Explorer",
  theme = bs_theme(
    bg           = "#0d0d1a",
    fg           = "white",
    primary      = "#7b2d8b",
    secondary    = "#cc4c02",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter"),
    bootswatch   = "darkly"
  ),
  
  # ---- CUSTOM CSS ----
  tags$head(
    tags$style(HTML("
      .navbar { border-bottom: 1px solid #2a2a3a; }
      .card   { background-color: #13132a;
                border: 1px solid #2a2a3a; }
      .well   { background-color: #13132a;
                border: 1px solid #2a2a3a; }
      .selectize-input  { background-color: #1e1e35 !important;
                          border-color: #2a2a3a !important;
                          color: white !important; }
      .selectize-dropdown { background-color: #1e1e35 !important;
                             color: white !important; }
      .stat-box { background: #1e1e35;
                  border: 1px solid #2a2a3a;
                  border-radius: 8px;
                  padding: 16px;
                  text-align: center;
                  margin: 4px; }
      .stat-number { font-size: 2em;
                     font-weight: bold;
                     color: #7b2d8b; }
      .stat-number.negative { color: #cc4c02; }
      .stat-label  { font-size: 0.85em;
                     color: #aaaaaa;
                     margin-top: 4px; }
      .story-card  { background: #1e1e35;
                     border-left: 3px solid #7b2d8b;
                     border-radius: 4px;
                     padding: 12px 16px;
                     margin: 8px 0; }
    "))
  ),
  
  # ---- LOADING SCREEN ----
  use_waiter(),
  
  # ============================================================
  # TAB 1: EXPLORE
  # ============================================================
  nav_panel(
    title = "Explore",
    icon  = icon("map"),
    
    layout_sidebar(
      
      # ---- SIDEBAR ----
      sidebar = sidebar(
        width = 280,
        bg    = "#13132a",
        
        tags$h5("Select a County",
                style = "color: white; margin-bottom: 16px;"),
        
        selectInput(
          "selected_state",
          label   = "State",
          choices = c("Select a state..." = "", state_choices),
          width   = "100%"
        ),
        
        selectInput(
          "selected_county",
          label   = "County",
          choices = c("Select a county..." = ""),
          width   = "100%"
        ),
        
        selectInput(
          "selected_year",
          label   = "ACS Period",
          choices = c(
            "2016-2020 (5-year)" = 2020,
            "2015-2019 (5-year)" = 2019,
            "2014-2018 (5-year)" = 2018
          ),
          width   = "100%"
        ),
        
        hr(style = "border-color: #2a2a3a;"),
        
        actionButton(
          "go_button",
          label = "Explore Migration",
          icon  = icon("arrow-right"),
          width = "100%",
          class = "btn-primary"
        ),
        
        hr(style = "border-color: #2a2a3a;"),
        
        # Example counties to try
        tags$p("Try these:",
               style = "color: #aaaaaa; font-size: 0.85em;"),
        
        actionLink("ex_tuolumne",  "Tuolumne County, CA"),
        tags$br(),
        actionLink("ex_eldorado",  "El Dorado County, CA"),
        tags$br(),
        actionLink("ex_missoula",  "Missoula County, MT"),
        tags$br(),
        actionLink("ex_buncombe",  "Buncombe County, NC"),
        tags$br(),
        actionLink("ex_travis",    "Travis County, TX"),
        
        hr(style = "border-color: #2a2a3a;"),
        
        tags$p(
          "Data: U.S. Census Bureau ACS 5-Year Estimates.",
          tags$br(),
          "Note: Most county-level migration estimates carry",
          "high margins of error. Interpret directional patterns",
          "rather than exact counts.",
          style = "color: #555555; font-size: 0.75em;"
        )
      ),
      
      # ---- MAIN PANEL ----
      div(
        
        # Welcome message (shown before any county selected)
        conditionalPanel(
          condition = "output.county_selected == false",
          div(
            style = "text-align: center; padding: 80px 20px;",
            tags$h2("County Migration Explorer",
                    style = "color: white;"),
            tags$p(
              "Select any US county to explore who moves in,",
              "who moves out, and what the patterns reveal.",
              style = "color: #aaaaaa; font-size: 1.1em;"
            ),
            tags$p(
              "Powered by the U.S. Census Bureau ACS migration",
              "flow data via the tidycensus R package.",
              style = "color: #555555;"
            )
          )
        ),
        
        # Results (shown after county selected)
        conditionalPanel(
          condition = "output.county_selected == true",
          
          # County header
          uiOutput("county_header"),
          
          # Summary stat boxes
          uiOutput("stat_boxes"),
          
          tags$br(),
          
          # Map row
          fluidRow(
            column(6,
                   card(
                     card_header("🟣 Where New Residents Come From"),
                     leafletOutput("inflow_map", height = "380px")
                   )
            ),
            column(6,
                   card(
                     card_header("🟠 Where Residents Move To"),
                     leafletOutput("outflow_map", height = "380px")
                   )
            )
          ),
          
          tags$br(),
          
          # Charts row
          fluidRow(
            column(6,
                   card(
                     card_header("Top Origins & Destinations"),
                     plotlyOutput("comparison_chart", height = "380px")
                   )
            ),
            column(6,
                   card(
                     card_header("Migration by Distance"),
                     plotlyOutput("distance_chart", height = "380px")
                   )
            )
          ),
          
          tags$br(),
          
          # Stories row
          fluidRow(
            column(6,
                   card(
                     card_header("📊 Neighbor County Scorecard"),
                     DTOutput("neighbor_table")
                   )
            ),
            column(6,
                   card(
                     card_header("🔍 Key Stories"),
                     uiOutput("stories_panel")
                   )
            )
          ),
          
          tags$br(),
          
          # Full data table
          card(
            card_header("📋 Full Migration Data"),
            DTOutput("full_table")
          ),
          
          tags$br()
        )
      )
    )
  ),
  
  # ============================================================
  # TAB 2: COMPARE
  # ============================================================
  nav_panel(
    title = "Compare Counties",
    icon  = icon("chart-bar"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        bg    = "#13132a",
        
        tags$h5("Compare Two Counties",
                style = "color: white; margin-bottom: 16px;"),
        
        tags$p("County 1", style = "color: #7b2d8b; font-weight: bold;"),
        selectInput("compare_state_1", "State", 
                    choices = c("Select..." = "", state_choices),
                    width = "100%"),
        selectInput("compare_county_1", "County",
                    choices = c("Select..." = ""), width = "100%"),
        
        tags$hr(style = "border-color: #2a2a3a;"),
        
        tags$p("County 2", style = "color: #cc4c02; font-weight: bold;"),
        selectInput("compare_state_2", "State",
                    choices = c("Select..." = "", state_choices),
                    width = "100%"),
        selectInput("compare_county_2", "County",
                    choices = c("Select..." = ""), width = "100%"),
        
        tags$hr(style = "border-color: #2a2a3a;"),
        
        actionButton("compare_go", "Compare",
                     icon = icon("balance-scale"),
                     width = "100%", class = "btn-primary")
      ),
      
      uiOutput("compare_output")
    )
  ),
  
  # ============================================================
  # TAB 3: ABOUT
  # ============================================================
  nav_panel(
    title = "About",
    icon  = icon("info-circle"),
    
    div(style = "max-width: 800px; margin: 0 auto; padding: 40px 20px;",
        
        tags$h2("About This App", style = "color: white;"),
        
        tags$p(
          "This app visualizes county-to-county domestic migration",
          "flows using data from the U.S. Census Bureau's American",
          "Community Survey (ACS). Select any US county to see where",
          "new residents come from and where departing residents go.",
          style = "color: #cccccc;"
        ),
        
        tags$h4("Data", style = "color: white; margin-top: 24px;"),
        tags$p(
          "ACS 5-Year County-to-County Migration Flow Estimates.",
          "Accessed via the tidycensus R package.",
          tags$a("Census migration documentation →",
                 href = paste0("https://www.census.gov/topics/population/",
                               "migration/guidance/",
                               "county-to-county-migration-flows.html"),
                 target = "_blank",
                 style = "color: #7b2d8b;"),
          style = "color: #cccccc;"
        ),
        
        tags$h4("Important Caveat", style = "color: white; margin-top: 24px;"),
        tags$div(
          class = "story-card",
          tags$p(
            "Most county-level ACS migration estimates carry margins of",
            "error exceeding 50% of the estimate. This is normal for",
            "small-geography migration data. Interpret directional",
            "patterns across multiple counties rather than relying on",
            "any single number.",
            style = "color: #cccccc; margin: 0;"
          )
        ),
        
        tags$h4("Built With", style = "color: white; margin-top: 24px;"),
        tags$ul(
          style = "color: #cccccc;",
          tags$li(tags$a("tidycensus",
                         href = "https://walker-data.com/tidycensus/",
                         target = "_blank",
                         style = "color: #7b2d8b;"),
                  " — Census API access (Kyle Walker)"),
          tags$li(tags$a("Shiny",
                         href = "https://shiny.posit.co/",
                         target = "_blank",
                         style = "color: #7b2d8b;"),
                  " — web app framework"),
          tags$li(tags$a("leaflet",
                         href = "https://rstudio.github.io/leaflet/",
                         target = "_blank",
                         style = "color: #7b2d8b;"),
                  " — interactive maps"),
          tags$li(tags$a("plotly",
                         href = "https://plotly.com/r/",
                         target = "_blank",
                         style = "color: #7b2d8b;"),
                  " — interactive charts"),
          tags$li(tags$a("geosphere",
                         href = "https://cran.r-project.org/package=geosphere",
                         target = "_blank",
                         style = "color: #7b2d8b;"),
                  " — great circle arc geometry")
        ),
        
        tags$h4("Source Code", style = "color: white; margin-top: 24px;"),
        tags$p(
          tags$a("github.com/YOURUSERNAME/census-r-experiments",
                 href = "https://github.com/YOURUSERNAME/census-r-experiments",
                 target = "_blank",
                 style = "color: #7b2d8b;"),
          style = "color: #cccccc;"
        )
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Waiter loading screen
  w <- Waiter$new(
    html = tagList(
      spin_fading_circles(),
      tags$br(),
      tags$p("Pulling Census data...",
             style = "color: white; margin-top: 16px;")
    ),
    color = "#0d0d1a"
  )
  
  # ---- REACTIVE: Update county dropdown when state changes ----
  observeEvent(input$selected_state, {
    req(input$selected_state)
    
    counties <- county_lookup |>
      filter(state == input$selected_state) |>
      arrange(county)
    
    county_choices <- setNames(counties$full_fips, counties$county)
    
    updateSelectInput(session, "selected_county",
                      choices  = c("Select a county..." = "",
                                   county_choices))
  })
  
  # Same for compare tab county 1
  observeEvent(input$compare_state_1, {
    req(input$compare_state_1)
    counties <- county_lookup |>
      filter(state == input$compare_state_1) |>
      arrange(county)
    updateSelectInput(session, "compare_county_1",
                      choices = c("Select..." = "",
                                  setNames(counties$full_fips,
                                           counties$county)))
  })
  
  # Same for compare tab county 2
  observeEvent(input$compare_state_2, {
    req(input$compare_state_2)
    counties <- county_lookup |>
      filter(state == input$compare_state_2) |>
      arrange(county)
    updateSelectInput(session, "compare_county_2",
                      choices = c("Select..." = "",
                                  setNames(counties$full_fips,
                                           counties$county)))
  })
  
  # ---- EXAMPLE COUNTY SHORTCUTS ----
  observeEvent(input$ex_tuolumne, {
    updateSelectInput(session, "selected_state",   selected = "CA")
    updateSelectInput(session, "selected_county",  selected = "06109")
  })
  observeEvent(input$ex_eldorado, {
    updateSelectInput(session, "selected_state",   selected = "CA")
    updateSelectInput(session, "selected_county",  selected = "06017")
  })
  observeEvent(input$ex_missoula, {
    updateSelectInput(session, "selected_state",   selected = "MT")
    updateSelectInput(session, "selected_county",  selected = "30063")
  })
  observeEvent(input$ex_buncombe, {
    updateSelectInput(session, "selected_state",   selected = "NC")
    updateSelectInput(session, "selected_county",  selected = "37021")
  })
  observeEvent(input$ex_travis, {
    updateSelectInput(session, "selected_state",   selected = "TX")
    updateSelectInput(session, "selected_county",  selected = "48453")
  })
  
  # ---- REACTIVE: Migration data ----
  migration_data <- eventReactive(input$go_button, {
    req(input$selected_county, input$selected_state)
    
    w$show()
    on.exit(w$hide())
    
    county_info <- county_lookup |>
      filter(full_fips == input$selected_county) |>
      slice(1)
    
    data <- get_migration_cached(
      state      = county_info$state,
      county     = county_info$county,
      fips       = input$selected_county,
      year       = as.integer(input$selected_year)
    )
    
    data
  })
  
  # ---- OUTPUT: county_selected flag ----
  output$county_selected <- reactive({
    !is.null(migration_data()) && nrow(migration_data()$inflow) > 0
  })
  outputOptions(output, "county_selected", suspendWhenHidden = FALSE)
  
  # ---- OUTPUT: County header ----
  output$county_header <- renderUI({
    req(migration_data())
    d <- migration_data()
    
    tags$div(
      style = "padding: 8px 0 16px 0;",
      tags$h2(d$county_name,
              style = "color: white; margin: 0;"),
      tags$p(
        paste0("ACS 5-Year Estimates, ",
               as.integer(input$selected_year) - 4,
               "–",
               input$selected_year),
        style = "color: #aaaaaa; margin: 4px 0 0 0;"
      )
    )
  })
  
  # ---- OUTPUT: Stat boxes ----
  output$stat_boxes <- renderUI({
    req(migration_data())
    d   <- migration_data()
    net <- d$total_in - d$total_out
    net_color <- if (net >= 0) "" else " negative"
    
    fluidRow(
      column(2, div(class = "stat-box",
                    div(class = paste0("stat-number"), format(d$total_in, big.mark=",")),
                    div(class = "stat-label", "Moved In"))),
      
      column(2, div(class = "stat-box",
                    div(class = "stat-number negative",
                        format(d$total_out, big.mark=",")),
                    div(class = "stat-label", "Moved Out"))),
      
      column(2, div(class = "stat-box",
                    div(class = paste0("stat-number", net_color),
                        ifelse(net >= 0,
                               paste0("+", format(net, big.mark=",")),
                               format(net, big.mark=","))),
                    div(class = "stat-label", "Net Migration"))),
      
      column(2, div(class = "stat-box",
                    div(class = "stat-number", nrow(d$inflow)),
                    div(class = "stat-label", "Origin Counties"))),
      
      column(2, div(class = "stat-box",
                    div(class = "stat-number negative", nrow(d$outflow)),
                    div(class = "stat-label", "Destination Counties"))),
      
      column(2, div(class = "stat-box",
                    div(class = "stat-number",
                        paste0(round(d$pct_in_state), "%")),
                    div(class = "stat-label", "From Within State")))
    )
  })
  
  # ---- OUTPUT: Inflow map ----
  output$inflow_map <- renderLeaflet({
    req(migration_data())
    d <- migration_data()
    make_leaflet_map(d$inflow_lines, d$county_lon, d$county_lat,
                     direction = "in", county_name = d$county_name)
  })
  
  # ---- OUTPUT: Outflow map ----
  output$outflow_map <- renderLeaflet({
    req(migration_data())
    d <- migration_data()
    make_leaflet_map(d$outflow_lines, d$county_lon, d$county_lat,
                     direction = "out", county_name = d$county_name)
  })
  
  # ---- OUTPUT: Comparison chart ----
  output$comparison_chart <- renderPlotly({
    req(migration_data())
    d <- migration_data()
    make_comparison_chart(d$inflow, d$outflow) |>
      ggplotly(tooltip = "text") |>
      layout(
        paper_bgcolor = "#13132a",
        plot_bgcolor  = "#13132a",
        font          = list(color = "white")
      )
  })
  
  # ---- OUTPUT: Distance chart ----
  output$distance_chart <- renderPlotly({
    req(migration_data())
    d <- migration_data()
    make_distance_chart(d$inflow, d$county_lon, d$county_lat) |>
      ggplotly(tooltip = "text") |>
      layout(
        paper_bgcolor = "#13132a",
        plot_bgcolor  = "#13132a",
        font          = list(color = "white")
      )
  })
  
  # ---- OUTPUT: Neighbor table ----
  output$neighbor_table <- renderDT({
    req(migration_data())
    d <- migration_data()
    make_neighbor_table(d$inflow, d$outflow, d$neighbor_fips) |>
      datatable(
        options = list(
          pageLength = 8,
          dom        = "t",
          ordering   = TRUE
        ),
        rownames = FALSE,
        class    = "compact"
      ) |>
      formatStyle(columns = "Net",
                  color = styleInterval(0, c("#cc4c02", "#7b2d8b")))
  })
  
  # ---- OUTPUT: Stories panel ----
  output$stories_panel <- renderUI({
    req(migration_data())
    d <- migration_data()
    generate_stories(d)
  })
  
  # ---- OUTPUT: Full data table ----
  output$full_table <- renderDT({
    req(migration_data())
    d <- migration_data()
    
    bind_rows(
      d$inflow  |> mutate(Direction = "Moving In"),
      d$outflow |> mutate(Direction = "Moving Out")
    ) |>
      mutate(
        County    = str_remove(FULL2_NAME, ",.*$"),
        State     = str_extract(FULL2_NAME, "[^,]+$") |> str_trim(),
        Estimate  = format(estimate, big.mark = ","),
        MOE       = format(moe, big.mark = ","),
        CV        = percent(moe / estimate, accuracy = 0.1),
        Reliable  = case_when(
          moe/estimate <= 0.30 ~ "✅",
          moe/estimate <= 0.50 ~ "⚠️",
          TRUE                 ~ "❌"
        )
      ) |>
      select(County, State, Direction, Estimate, MOE, CV, Reliable) |>
      datatable(
        filter  = "top",
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  # ---- COMPARE TAB ----
  compare_data <- eventReactive(input$compare_go, {
    req(input$compare_county_1, input$compare_county_2)
    
    w$show()
    on.exit(w$hide())
    
    c1_info <- county_lookup |>
      filter(full_fips == input$compare_county_1) |> slice(1)
    c2_info <- county_lookup |>
      filter(full_fips == input$compare_county_2) |> slice(1)
    
    list(
      county1 = get_migration_cached(
        state  = c1_info$state,
        county = c1_info$county,
        fips   = input$compare_county_1,
        year   = 2020
      ),
      county2 = get_migration_cached(
        state  = c2_info$state,
        county = c2_info$county,
        fips   = input$compare_county_2,
        year   = 2020
      )
    )
  })
  
  output$compare_output <- renderUI({
    req(compare_data())
    cd <- compare_data()
    
    fluidRow(
      column(6,
             tags$h3(cd$county1$county_name,
                     style = "color: #7b2d8b; text-align: center;"),
             tags$p(paste("In:", format(cd$county1$total_in, big.mark=",")),
                    " | Out:", format(cd$county1$total_out, big.mark=","),
                    " | Net:", cd$county1$total_in - cd$county1$total_out,
                    style = "color: #aaaaaa; text-align: center;"),
             leafletOutput("compare_map_1", height = "350px")
      ),
      column(6,
             tags$h3(cd$county2$county_name,
                     style = "color: #cc4c02; text-align: center;"),
             tags$p(paste("In:", format(cd$county2$total_in, big.mark=",")),
                    " | Out:", format(cd$county2$total_out, big.mark=","),
                    " | Net:", cd$county2$total_in - cd$county2$total_out,
                    style = "color: #aaaaaa; text-align: center;"),
             leafletOutput("compare_map_2", height = "350px")
      )
    )
  })
  
  output$compare_map_1 <- renderLeaflet({
    req(compare_data())
    cd <- compare_data()
    make_leaflet_map(cd$county1$inflow_lines,
                     cd$county1$county_lon,
                     cd$county1$county_lat,
                     direction   = "in",
                     county_name = cd$county1$county_name)
  })
  
  output$compare_map_2 <- renderLeaflet({
    req(compare_data())
    cd <- compare_data()
    make_leaflet_map(cd$county2$inflow_lines,
                     cd$county2$county_lon,
                     cd$county2$county_lat,
                     direction   = "in",
                     county_name = cd$county2$county_name)
  })
}

shinyApp(ui, server)