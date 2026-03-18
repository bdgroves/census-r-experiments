# =============================================================================
# MIGRATION EXPLORER — SHINY APP
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(geosphere)
library(scales)
library(ggplot2)
library(plotly)
library(DT)
library(waiter)
library(shinyjs)

# =============================================================================
# CENSUS API KEY SETUP
# =============================================================================
# Non-fatal — app loads even without key, cached counties still work

local({
  key <- Sys.getenv("CENSUS_API_KEY")
  if (nchar(key) == 0) key <- Sys.getenv("TIDYCENSUS_API_KEY")
  if (nchar(key) > 0) {
    tidycensus::census_api_key(key)
    message("Census API key set.")
  } else {
    message("No Census API key found — cached counties only.")
  }
})

# =============================================================================
# LOAD HELPER FUNCTIONS
# =============================================================================
# Each source() wrapped individually so we can see exactly which file fails

message("Loading R/get_migration.R ...")
source("R/get_migration.R")
message("OK")

message("Loading R/make_maps.R ...")
source("R/make_maps.R")
message("OK")

message("Loading R/make_charts.R ...")
source("R/make_charts.R")
message("OK")

message("Loading R/utils.R ...")
source("R/utils.R")
message("OK")

# =============================================================================
# WORKING DIRECTORY + CACHE
# =============================================================================

message("Working directory: ", getwd())

cache_dir <- file.path("data", "cache")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}
message("Cache dir: ", normalizePath(cache_dir))

# =============================================================================
# STATE AND COUNTY REFERENCE DATA
# =============================================================================

county_lookup <- tidycensus::fips_codes |>
  dplyr::mutate(
    full_fips = paste0(state_code, county_code),
    display   = paste0(county, ", ", state)
  ) |>
  dplyr::filter(!state %in% c("PR", "UM", "VI", "GU", "AS", "MP")) |>
  dplyr::arrange(state_name, county)

state_choices <- county_lookup |>
  dplyr::distinct(state_name, state) |>
  dplyr::arrange(state_name) |>
  tibble::deframe()

message("County lookup built: ", nrow(county_lookup), " rows")
message("State choices built: ", length(state_choices), " states")


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
  
  header = tagList(
    
    use_waiter(),
    useShinyjs(),       # FIX 1: added — must be in UI for shinyjs to work
    
    # FIX 2: expanded CSS with dark theme overrides + table dark styles
    tags$head(tags$style(HTML("

      /* ---- DARK THEME OVERRIDES ---- */
      body, html                { background-color: #0d0d1a !important;
                                  color: white !important; }
      .bslib-page-navbar        { background-color: #0d0d1a !important; }
      .tab-content              { background-color: #0d0d1a !important; }
      .bslib-sidebar-layout     { background-color: #0d0d1a !important; }
      .bslib-sidebar-layout > .sidebar
                                { background-color: #13132a !important; }
      .main                     { background-color: #0d0d1a !important; }

      /* ---- COMPONENT STYLES ---- */
      .navbar              { border-bottom: 1px solid #2a2a3a; }
      .card                { background-color: #13132a;
                             border: 1px solid #2a2a3a; }
      .stat-box            { background: #1e1e35;
                             border: 1px solid #2a2a3a;
                             border-radius: 8px;
                             padding: 16px;
                             text-align: center;
                             margin: 4px; }
      .stat-number         { font-size: 2em;
                             font-weight: bold;
                             color: #7b2d8b; }
      .stat-number.negative { color: #cc4c02; }
      .stat-label          { font-size: 0.85em;
                             color: #aaaaaa;
                             margin-top: 4px; }
      .story-card          { background: #1e1e35;
                             border-left: 3px solid #7b2d8b;
                             border-radius: 4px;
                             padding: 12px 16px;
                             margin: 8px 0; }
      .selectize-input     { background-color: #1e1e35 !important;
                             border-color: #2a2a3a !important;
                             color: white !important; }
      .selectize-dropdown  { background-color: #1e1e35 !important;
                             color: white !important; }

      /* ---- TABLE DARK THEME ---- */
      .dataTables_wrapper        { color: #cccccc !important; }
      table.dataTable            { background-color: #13132a !important;
                                   color: #cccccc !important; }
      table.dataTable thead th   { background-color: #1e1e35 !important;
                                   color: white !important;
                                   border-bottom: 1px solid #2a2a3a !important; }
      table.dataTable tbody tr   { background-color: #13132a !important; }
      table.dataTable tbody tr:hover
                                 { background-color: #1e1e35 !important; }
      .dataTables_filter input,
      .dataTables_length select  { background-color: #1e1e35 !important;
                                   color: white !important;
                                   border: 1px solid #2a2a3a !important; }
      .dataTables_info,
      .dataTables_paginate       { color: #aaaaaa !important; }
      .paginate_button           { color: #aaaaaa !important; }
      .paginate_button.current   { background: #7b2d8b !important;
                                   color: white !important; }
    ")))
  ),
  
  # ==========================================================================
  # TAB 1: EXPLORE
  # ==========================================================================
  nav_panel(
    title = "Explore",
    icon  = icon("map"),
    
    layout_sidebar(
      
      sidebar = sidebar(
        width = 280,
        bg    = "#13132a",
        
        tags$h5("Select a County",
                style = "color: white; margin-bottom: 16px;"),
        
        selectInput("selected_state", "State",
                    choices = c("Select a state..." = "", state_choices),
                    width   = "100%"),
        
        selectInput("selected_county", "County",
                    choices = c("Select a county..." = ""),
                    width   = "100%"),
        
        selectInput("selected_year", "ACS Period",
                    choices = c(
                      "2016-2020 (5-year)" = 2020,
                      "2015-2019 (5-year)" = 2019,
                      "2014-2018 (5-year)" = 2018
                    ),
                    width = "100%"),
        
        hr(style = "border-color: #2a2a3a;"),
        
        actionButton("go_button", "Explore Migration",
                     icon  = icon("arrow-right"),
                     width = "100%",
                     class = "btn-primary"),
        
        hr(style = "border-color: #2a2a3a;"),
        
        tags$p("Try these:",
               style = "color: #aaaaaa; font-size: 0.85em;"),
        
        actionLink("ex_tuolumne", "Tuolumne County, CA"), tags$br(),
        actionLink("ex_eldorado", "El Dorado County, CA"), tags$br(),
        actionLink("ex_missoula", "Missoula County, MT"),  tags$br(),
        actionLink("ex_buncombe", "Buncombe County, NC"),  tags$br(),
        actionLink("ex_travis",   "Travis County, TX"),
        
        hr(style = "border-color: #2a2a3a;"),
        
        tags$p(
          "Data: U.S. Census Bureau ACS 2016\u20132020.",
          tags$br(),
          "Migration flows are survey estimates.",
          "Small flows (",
          tags$i("< 50 people"),
          ") may represent 1\u20132 survey respondents.",
          "Zero values may reflect sample gaps.",
          "See the About tab for full data quality notes.",
          style = "color: #555555; font-size: 0.75em;"
        )
      ),
      
      div(
        
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
            ),
            tags$br(),
            tags$p("\u2190 Use the sidebar to get started",
                   style = "color: #7b2d8b; font-size: 1.1em;")
          )
        ),
        
        conditionalPanel(
          condition = "output.county_selected == true",
          
          uiOutput("county_header"),
          uiOutput("stat_boxes"),
          
          tags$br(),
          
          fluidRow(
            column(6, card(
              card_header("\U0001F7E3 Where New Residents Come From"),
              leafletOutput("inflow_map", height = "380px")
            )),
            column(6, card(
              card_header("\U0001F7E0 Where Residents Move To"),
              leafletOutput("outflow_map", height = "380px")
            ))
          ),
          
          tags$br(),
          
          fluidRow(
            column(6, card(
              card_header("Top Origins & Destinations"),
              plotlyOutput("comparison_chart", height = "380px")
            )),
            column(6, card(
              card_header("Migration by Distance"),
              plotlyOutput("distance_chart", height = "380px")
            ))
          ),
          
          tags$br(),
          
          fluidRow(
            column(6, card(
              card_header("Neighbor County Scorecard"),
              DTOutput("neighbor_table")
            )),
            column(6, card(
              card_header("Key Stories"),
              uiOutput("stories_panel")
            ))
          ),
          
          tags$br(),
          
          card(
            card_header("Full Migration Data"),
            DTOutput("full_table")
          ),
          
          tags$br()
        )
      )
    )
  ),
  
  # ==========================================================================
  # TAB 2: COMPARE
  # ==========================================================================
  nav_panel(
    title = "Compare Counties",
    icon  = icon("chart-bar"),
    
    layout_sidebar(
      
      sidebar = sidebar(
        width = 280,
        bg    = "#13132a",
        
        tags$h5("Compare Two Counties",
                style = "color: white; margin-bottom: 16px;"),
        
        tags$p("County 1",
               style = "color: #7b2d8b; font-weight: bold;"),
        selectInput("compare_state_1", "State",
                    choices = c("Select..." = "", state_choices),
                    width   = "100%"),
        selectInput("compare_county_1", "County",
                    choices = c("Select..." = ""),
                    width   = "100%"),
        
        hr(style = "border-color: #2a2a3a;"),
        
        tags$p("County 2",
               style = "color: #cc4c02; font-weight: bold;"),
        selectInput("compare_state_2", "State",
                    choices = c("Select..." = "", state_choices),
                    width   = "100%"),
        selectInput("compare_county_2", "County",
                    choices = c("Select..." = ""),
                    width   = "100%"),
        
        hr(style = "border-color: #2a2a3a;"),
        
        actionButton("compare_go", "Compare",
                     icon  = icon("balance-scale"),
                     width = "100%",
                     class = "btn-primary")
      ),
      
      uiOutput("compare_output")
    )
  ),
  
  # ==========================================================================
  # TAB 3: ABOUT
  # ==========================================================================
  nav_panel(
    title = "About",
    icon  = icon("info-circle"),
    
    div(
      style = "max-width: 800px; margin: 0 auto; padding: 40px 20px;",
      
      tags$h2("About This App", style = "color: white;"),
      
      tags$p(
        "This app visualizes county-to-county domestic migration flows",
        "using data from the U.S. Census Bureau's American Community",
        "Survey (ACS). Select any US county to see where new residents",
        "come from and where departing residents go.",
        style = "color: #cccccc;"
      ),
      
      # ---- DATA SECTION ----
      tags$h4("Data", style = "color: white; margin-top: 24px;"),
      tags$p(
        "ACS 5-Year County-to-County Migration Flow Estimates,",
        "accessed via the tidycensus R package.",
        tags$a("Census migration documentation \u2192",
               href   = paste0(
                 "https://www.census.gov/topics/population/migration/",
                 "guidance/county-to-county-migration-flows.html"
               ),
               target = "_blank",
               style  = "color: #7b2d8b;"),
        style = "color: #cccccc;"
      ),
      
      tags$p(
        tags$b("Current data: ", style = "color: white;"),
        "ACS 2016\u20132020 (5-year estimates). ",
        "County-to-county migration flows for 2021 and later ",
        "have not yet been released by the Census Bureau as of early 2026. ",
        "The main ACS tables (income, education, etc.) released January 2026, ",
        "but migration flows are a supplemental product released separately, ",
        "typically 6\u201312 months later. ",
        "This app will be updated when newer county-level flows become available.",
        style = "color: #aaaaaa; font-size: 0.9em;"
      ),
      
      # ---- IMPORTANT DATA QUALITY SECTION ----
      tags$h4("Understanding the Data \u2014 Please Read",
              style = "color: white; margin-top: 24px;"),
      
      div(
        class = "story-card",
        style = "border-left-color: #cc4c02;",
        tags$b("\u26a0\ufe0f The ACS is a Survey, Not a Census",
               style = "color: #cc4c02;"),
        tags$p(
          "The American Community Survey samples approximately 3.5 million",
          "households per year \u2014 about 12% of all US households.",
          "Over a 5-year period, roughly 15\u201317 million households",
          "are surveyed. This means:",
          style = "color: #cccccc; margin: 8px 0 4px 0;"
        ),
        tags$ul(
          style = "color: #cccccc; margin: 4px 0;",
          tags$li(
            "Small migration flows may represent just 1\u20132 survey",
            "respondents whose responses are statistically",
            "\u201cweighted up\u201d to represent the full population."
          ),
          tags$li(
            "A flow showing zero people does not necessarily mean",
            "nobody made that move \u2014 it may mean nobody in the",
            "sample made that move. Zero is the most misleading",
            "number in migration data."
          ),
          tags$li(
            "94% of individual county-level flow estimates in a",
            "typical small county have margins of error exceeding",
            "50% of the estimate itself."
          )
        )
      ),
      
      tags$br(),
      
      div(
        class = "story-card",
        style = "border-left-color: #7b2d8b;",
        tags$b("\u2705 What You CAN Rely On",
               style = "color: #7b2d8b;"),
        tags$p(
          "Despite high individual uncertainty, patterns across",
          "multiple counties are meaningful:",
          style = "color: #cccccc; margin: 8px 0 4px 0;"
        ),
        tags$ul(
          style = "color: #cccccc; margin: 4px 0;",
          tags$li(
            "If 8 of 8 Bay Area counties all show net migration",
            "toward a county, that directional signal is real",
            "even if the exact counts are uncertain."
          ),
          tags$li(
            "Large flows (200+ people) with low margins of error",
            "are generally reliable."
          ),
          tags$li(
            "The reliability column in the Full Migration Data table",
            "flags each estimate: \u2705 reliable, \u26a0\ufe0f caution,",
            "\u274c unreliable."
          )
        )
      ),
      
      tags$br(),
      
      div(
        class = "story-card",
        style = "border-left-color: #444466;",
        tags$b("\U0001F4CA How to Read the MOE Column",
               style = "color: #aaaaaa;"),
        tags$p(
          "MOE = Margin of Error at 90% confidence.",
          "An estimate of 138 with MOE of 134 means the true value",
          "is somewhere between 4 and 272 \u2014 not a precise count.",
          "CV (coefficient of variation) = MOE \u00f7 Estimate.",
          "CV under 15% = high confidence.",
          "CV over 50% = treat as directional signal only.",
          style = "color: #aaaaaa; margin: 6px 0 0 0; font-size: 0.9em;"
        )
      ),
      
      # ---- BUILT WITH ----
      tags$h4("Built With", style = "color: white; margin-top: 24px;"),
      tags$ul(
        style = "color: #cccccc;",
        tags$li(
          tags$a("tidycensus",
                 href   = "https://walker-data.com/tidycensus/",
                 target = "_blank", style = "color: #7b2d8b;"),
          " \u2014 Census API (Kyle Walker)"
        ),
        tags$li(
          tags$a("Shiny + bslib",
                 href   = "https://shiny.posit.co/",
                 target = "_blank", style = "color: #7b2d8b;"),
          " \u2014 web app framework"
        ),
        tags$li(
          tags$a("leaflet",
                 href   = "https://rstudio.github.io/leaflet/",
                 target = "_blank", style = "color: #7b2d8b;"),
          " \u2014 interactive maps"
        ),
        tags$li(
          tags$a("plotly",
                 href   = "https://plotly.com/r/",
                 target = "_blank", style = "color: #7b2d8b;"),
          " \u2014 interactive charts"
        ),
        tags$li(
          tags$a("geosphere",
                 href   = "https://cran.r-project.org/package=geosphere",
                 target = "_blank", style = "color: #7b2d8b;"),
          " \u2014 great circle arcs"
        )
      ),
      
      # ---- SOURCE CODE ----
      tags$h4("Source Code", style = "color: white; margin-top: 24px;"),
      tags$p(
        tags$a("github.com/brooksgroves/census-r-experiments",
               href   = "https://github.com/brooksgroves/census-r-experiments",
               target = "_blank",
               style  = "color: #7b2d8b;"),
        style = "color: #cccccc;"
      ),
      
      # ---- ACKNOWLEDGEMENTS ----
      tags$h4("Acknowledgements", style = "color: white; margin-top: 24px;"),
      tags$p(
        "Data quality insights from ",
        tags$a("@leafyoreo",
               href   = "https://twitter.com/leafyoreo",
               target = "_blank",
               style  = "color: #7b2d8b;"),
        " on X/Twitter, who correctly noted that small ACS migration",
        "flows may represent just one or two survey respondents",
        "and that zero values can reflect sample gaps rather than",
        "true absence of migration. That's good Census literacy.",
        style = "color: #aaaaaa; font-size: 0.9em;"
      ),
      
      tags$br(),
      tags$br()
    )
  )
  
) # end page_navbar


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  w <- Waiter$new(
    html  = tagList(
      spin_fading_circles(),
      tags$br(),
      tags$p("Pulling Census data...",
             style = "color: white; margin-top: 16px;")
    ),
    color = "#0d0d1a"
  )
  
  # ==========================================================================
  # DROPDOWN UPDATES
  # ==========================================================================
  
  observeEvent(input$selected_state, {
    req(input$selected_state)
    counties <- county_lookup |>
      filter(state == input$selected_state) |>
      arrange(county)
    updateSelectInput(session, "selected_county",
                      choices = c("Select a county..." = "",
                                  setNames(counties$full_fips,
                                           counties$county)))
  })
  
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
  
  # ==========================================================================
  # EXAMPLE COUNTY SHORTCUTS
  # shinyjs::delay() waits for the county dropdown to repopulate
  # after the state change before setting the county selection
  # ==========================================================================
  
  observeEvent(input$ex_tuolumne, {
    updateSelectInput(session, "selected_state", selected = "CA")
    shinyjs::delay(300, {
      updateSelectInput(session, "selected_county", selected = "06109")
    })
  })
  
  observeEvent(input$ex_eldorado, {
    updateSelectInput(session, "selected_state", selected = "CA")
    shinyjs::delay(300, {
      updateSelectInput(session, "selected_county", selected = "06017")
    })
  })
  
  observeEvent(input$ex_missoula, {
    updateSelectInput(session, "selected_state", selected = "MT")
    shinyjs::delay(300, {
      updateSelectInput(session, "selected_county", selected = "30063")
    })
  })
  
  observeEvent(input$ex_buncombe, {
    updateSelectInput(session, "selected_state", selected = "NC")
    shinyjs::delay(300, {
      updateSelectInput(session, "selected_county", selected = "37021")
    })
  })
  
  observeEvent(input$ex_travis, {
    updateSelectInput(session, "selected_state", selected = "TX")
    shinyjs::delay(300, {
      updateSelectInput(session, "selected_county", selected = "48453")
    })
  })
  
  # ==========================================================================
  # CORE REACTIVE: FETCH MIGRATION DATA
  # ==========================================================================
  
  migration_data <- eventReactive(input$go_button, {
    req(input$selected_county, input$selected_state)
    
    w$show()
    on.exit(w$hide())
    
    county_info <- county_lookup |>
      filter(full_fips == input$selected_county) |>
      slice(1)
    
    get_migration_cached(
      state  = county_info$state,
      county = county_info$county,
      fips   = input$selected_county,
      year   = as.integer(input$selected_year)
    )
  })
  
  # ==========================================================================
  # FLAG: HAS A COUNTY BEEN LOADED?
  # ==========================================================================
  
  output$county_selected <- reactive({
    !is.null(migration_data()) && nrow(migration_data()$inflow) > 0
  })
  outputOptions(output, "county_selected", suspendWhenHidden = FALSE)
  
  # ==========================================================================
  # EXPLORE TAB OUTPUTS
  # ==========================================================================
  
  output$county_header <- renderUI({
    req(migration_data())
    d <- migration_data()
    tags$div(
      style = "padding: 8px 0 16px 0;",
      tags$h2(d$county_name, style = "color: white; margin: 0;"),
      tags$p(
        paste0("ACS 5-Year Estimates, ",
               as.integer(input$selected_year) - 4,
               "\u2013",
               input$selected_year),
        style = "color: #aaaaaa; margin: 4px 0 0 0;"
      )
    )
  })
  
  output$stat_boxes <- renderUI({
    req(migration_data())
    d   <- migration_data()
    net <- d$total_in - d$total_out
    
    fluidRow(
      column(2, div(class = "stat-box",
                    div(class = "stat-number",
                        format(d$total_in, big.mark = ",")),
                    div(class = "stat-label", "Moved In")
      )),
      column(2, div(class = "stat-box",
                    div(class = "stat-number negative",
                        format(d$total_out, big.mark = ",")),
                    div(class = "stat-label", "Moved Out")
      )),
      column(2, div(class = "stat-box",
                    div(class = paste0("stat-number",
                                       ifelse(net < 0, " negative", "")),
                        ifelse(net >= 0,
                               paste0("+", format(net, big.mark = ",")),
                               format(net, big.mark = ","))),
                    div(class = "stat-label", "Net Migration")
      )),
      column(2, div(class = "stat-box",
                    div(class = "stat-number", nrow(d$inflow)),
                    div(class = "stat-label", "Origin Counties")
      )),
      column(2, div(class = "stat-box",
                    div(class = "stat-number negative", nrow(d$outflow)),
                    div(class = "stat-label", "Destination Counties")
      )),
      column(2, div(class = "stat-box",
                    div(class = "stat-number",
                        paste0(round(d$pct_in_state), "%")),
                    div(class = "stat-label", "From Within State")
      ))
    )
  })
  
  output$inflow_map <- renderLeaflet({
    req(migration_data())
    d <- migration_data()
    make_leaflet_map(d$inflow_lines, d$county_lon, d$county_lat,
                     direction   = "in",
                     county_name = d$county_name)
  })
  
  output$outflow_map <- renderLeaflet({
    req(migration_data())
    d <- migration_data()
    make_leaflet_map(d$outflow_lines, d$county_lon, d$county_lat,
                     direction   = "out",
                     county_name = d$county_name)
  })
  
  output$comparison_chart <- renderPlotly({
    req(migration_data())
    d <- migration_data()
    make_comparison_chart(d$inflow, d$outflow) |>
      ggplotly(tooltip = "text") |>
      layout(paper_bgcolor = "#13132a",
             plot_bgcolor  = "#13132a",
             font          = list(color = "white"))
  })
  
  output$distance_chart <- renderPlotly({
    req(migration_data())
    d <- migration_data()
    make_distance_chart(d$inflow, d$county_lon, d$county_lat) |>
      ggplotly(tooltip = "text") |>
      layout(paper_bgcolor = "#13132a",
             plot_bgcolor  = "#13132a",
             font          = list(color = "white"))
  })
  
  output$neighbor_table <- renderDT({
    req(migration_data())
    d <- migration_data()
    make_neighbor_table(d$inflow, d$outflow, d$neighbor_fips) |>
      datatable(
        options  = list(pageLength = 8, dom = "t", ordering = TRUE),
        rownames = FALSE,
        class    = "compact"
      ) |>
      formatStyle("Net",
                  color = styleInterval(0, c("#cc4c02", "#7b2d8b")))
  })
  
  output$full_table <- renderDT({
    req(migration_data())
    d <- migration_data()
    
    bind_rows(
      d$inflow  |> mutate(Direction = "Moving In"),
      d$outflow |> mutate(Direction = "Moving Out")
    ) |>
      mutate(
        County   = str_remove(FULL2_NAME, ",.*$"),
        State    = str_extract(FULL2_NAME, "[^,]+$") |> str_trim(),
        Estimate = format(estimate, big.mark = ","),
        MOE      = format(moe, big.mark = ","),
        CV       = percent(moe / estimate, accuracy = 0.1),
        # FIX 3: unicode escapes instead of literal emoji
        # avoids Windows encoding issues
        Reliable = case_when(
          moe / estimate <= 0.30 ~ "\u2705",
          moe / estimate <= 0.50 ~ "\u26a0\ufe0f",
          TRUE                   ~ "\u274c"
        )
      ) |>
      select(County, State, Direction, Estimate, MOE, CV, Reliable) |>
      datatable(
        filter   = "top",
        options  = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  output$stories_panel <- renderUI({
    req(migration_data())
    generate_stories(migration_data())
  })
  
  # ==========================================================================
  # COMPARE TAB
  # ==========================================================================
  
  compare_data <- eventReactive(input$compare_go, {
    req(input$compare_county_1, input$compare_county_2)
    
    w$show()
    on.exit(w$hide())
    
    c1 <- county_lookup |>
      filter(full_fips == input$compare_county_1) |> slice(1)
    c2 <- county_lookup |>
      filter(full_fips == input$compare_county_2) |> slice(1)
    
    list(
      county1 = get_migration_cached(
        c1$state, c1$county, input$compare_county_1, 2020
      ),
      county2 = get_migration_cached(
        c2$state, c2$county, input$compare_county_2, 2020
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
             tags$p(
               paste0("In: ",  format(cd$county1$total_in,  big.mark = ","),
                      " | Out: ", format(cd$county1$total_out, big.mark = ","),
                      " | Net: ",
                      ifelse(
                        cd$county1$total_in - cd$county1$total_out >= 0,
                        paste0("+", cd$county1$total_in - cd$county1$total_out),
                        cd$county1$total_in - cd$county1$total_out
                      )),
               style = "color: #aaaaaa; text-align: center;"
             ),
             leafletOutput("compare_map_1", height = "350px")
      ),
      column(6,
             tags$h3(cd$county2$county_name,
                     style = "color: #cc4c02; text-align: center;"),
             tags$p(
               paste0("In: ",  format(cd$county2$total_in,  big.mark = ","),
                      " | Out: ", format(cd$county2$total_out, big.mark = ","),
                      " | Net: ",
                      ifelse(
                        cd$county2$total_in - cd$county2$total_out >= 0,
                        paste0("+", cd$county2$total_in - cd$county2$total_out),
                        cd$county2$total_in - cd$county2$total_out
                      )),
               style = "color: #aaaaaa; text-align: center;"
             ),
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
  
} # end server


shinyApp(ui, server)