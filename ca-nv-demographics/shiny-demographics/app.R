# =============================================================================
# CA/NV DEMOGRAPHICS EXPLORER — SHINY APP
# =============================================================================
# Interactive demographic map explorer for California and Nevada.
# Uses mapgl for interactive choropleth maps with filterable legends.
#
# Run with: shiny::runApp("ca-nv-demographics/shiny-demographics")
#
# DEPENDS ON: ca-nv-demographics/explore.R
# Run explore.R first to build the cache files, OR
# uncomment the source() line below.
#
# Tooltip behavior:
#   Hover = plain text (tract name, county, value)
#   Click = styled HTML popup (formatted value with color)
# =============================================================================

# source(here::here("ca-nv-demographics", "explore.R"))

library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(mapgl)
library(scales)
library(waiter)
library(shinyjs)
library(DT)

source("R/helpers.R")
source("R/map_builders.R")

message("Working directory: ", getwd())

cache_dir <- file.path("data", "cache")
if (!dir.exists(cache_dir)) {
  stop("Cache not found. Run explore.R first to build data cache.")
}

# =============================================================================
# LOAD DATA
# =============================================================================

message("Loading tract data from cache...")

# Repair function: extracts county and tract_name from NAME column
# ACS NAME format: "Census Tract X; County Name; State"
# Uses semicolon delimiters — NOT commas
repair_tracts <- function(df) {
  df |>
    dplyr::mutate(
      county = stringr::str_extract(NAME, "(?<=; ).*(?=; )"),
      tract_name = stringr::str_trim(
        stringr::str_extract(NAME, "^[^;]+")
      )
    )
}

ca_tracts    <- readRDS(file.path(cache_dir, "ca_tracts_2022.rds")) |>
  repair_tracts()
nv_tracts    <- readRDS(file.path(cache_dir, "nv_tracts_2022.rds")) |>
  repair_tracts()
ca_nv_tracts <- dplyr::bind_rows(ca_tracts, nv_tracts)

message("Data loaded.")
message("  CA tracts: ",     nrow(ca_tracts))
message("  NV tracts: ",     nrow(nv_tracts))
message("  Sample county: ", ca_tracts$county[1])
message("  Sample tract:  ", ca_tracts$tract_name[1])

# =============================================================================
# VARIABLE DEFINITIONS
# =============================================================================

variable_choices <- list(
  "Education" = c(
    "Bachelor's Degree or Higher (%)"   = "bach_plus"
  ),
  "Economics" = c(
    "Median Household Income ($)"       = "median_income",
    "Poverty Rate (%)"                  = "poverty_rate",
    "Unemployment Rate (%)"             = "unemployment"
  ),
  "Housing" = c(
    "Median Gross Rent ($)"             = "median_rent",
    "Owner-Occupied Housing (%)"        = "owner_occupied",
    "Vacancy Rate (%)"                  = "vacancy_rate",
    "Rent Burden (Rent as % of Income)" = "rent_burden"
  ),
  "Demographics" = c(
    "Median Age"                        = "median_age",
    "Foreign Born (%)"                  = "foreign_born"
  ),
  "Mobility" = c(
    "Moved in Past Year (%)"            = "diff_house"
  )
)

variable_meta <- list(
  bach_plus = list(
    label   = "Bachelor's Degree or Higher (%)",
    palette = "viridis",
    note    = "% of adults 25+ with bachelor's degree or higher"
  ),
  median_income = list(
    label   = "Median Household Income ($)",
    palette = "viridis",
    note    = "Median household income in the past 12 months"
  ),
  poverty_rate = list(
    label   = "Poverty Rate (%)",
    palette = "inferno",
    note    = "% of population below the federal poverty line"
  ),
  unemployment = list(
    label   = "Unemployment Rate (%)",
    palette = "inferno",
    note    = "% of civilian labor force that is unemployed"
  ),
  median_rent = list(
    label   = "Median Gross Rent ($)",
    palette = "plasma",
    note    = "Median monthly rent including utilities"
  ),
  owner_occupied = list(
    label   = "Owner-Occupied Housing (%)",
    palette = "viridis",
    note    = "% of occupied housing units that are owner-occupied"
  ),
  vacancy_rate = list(
    label   = "Vacancy Rate (%)",
    palette = "plasma",
    note    = "% of housing units that are vacant"
  ),
  rent_burden = list(
    label   = "Rent Burden (%)",
    palette = "inferno",
    note    = paste0(
      "Annual rent as % of income. ",
      ">30% = cost burdened. >50% = severely burdened. ",
      "Capped at 150% for display."
    )
  ),
  median_age = list(
    label   = "Median Age",
    palette = "viridis",
    note    = "Median age of the population"
  ),
  foreign_born = list(
    label   = "Foreign Born (%)",
    palette = "plasma",
    note    = "% of population born outside the United States"
  ),
  diff_house = list(
    label   = "Moved in Past Year (%)",
    palette = "plasma",
    note    = "% of residents living in a different house 1 year ago"
  )
)


# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title = "\U0001F30E CA/NV Demographics",
  
  theme = bs_theme(
    bg           = "#0d0d1a",
    fg           = "white",
    primary      = "#2d8b7b",
    secondary    = "#cc7b02",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter"),
    bootswatch   = "darkly"
  ),
  
  header = tagList(
    
    use_waiter(),
    useShinyjs(),
    
    tags$head(tags$style(HTML("
      body, html              { background-color: #0d0d1a !important;
                                color: white !important; }
      .bslib-page-navbar      { background-color: #0d0d1a !important; }
      .tab-content            { background-color: #0d0d1a !important; }
      .bslib-sidebar-layout   { background-color: #0d0d1a !important; }
      .bslib-sidebar-layout > .sidebar
                              { background-color: #13132a !important; }
      .navbar                 { border-bottom: 1px solid #2a2a3a; }
      .card                   { background-color: #13132a;
                                border: 1px solid #2a2a3a; }
      .stat-box               { background: #1e1e35;
                                border: 1px solid #2a2a3a;
                                border-radius: 8px;
                                padding: 16px;
                                text-align: center;
                                margin: 4px; }
      .stat-number            { font-size: 1.8em;
                                font-weight: bold;
                                color: #2d8b7b; }
      .stat-label             { font-size: 0.8em;
                                color: #aaaaaa;
                                margin-top: 4px; }
      .info-card              { background: #1e1e35;
                                border-left: 3px solid #2d8b7b;
                                border-radius: 4px;
                                padding: 12px 16px;
                                margin: 8px 0; }
      .selectize-input        { background-color: #1e1e35 !important;
                                border-color: #2a2a3a !important;
                                color: white !important; }
      .selectize-dropdown     { background-color: #1e1e35 !important;
                                color: white !important; }
      .dataTables_wrapper     { color: #cccccc !important; }
      table.dataTable         { background-color: #13132a !important;
                                color: #cccccc !important; }
      table.dataTable thead th
                              { background-color: #1e1e35 !important;
                                color: white !important; }
      table.dataTable tbody tr
                              { background-color: #13132a !important; }
      table.dataTable tbody tr:hover
                              { background-color: #1e1e35 !important; }
      .dataTables_info,
      .dataTables_paginate    { color: #aaaaaa !important; }
    ")))
  ),
  
  # ==========================================================================
  # TAB 1: MAP
  # ==========================================================================
  nav_panel(
    title = "Map",
    icon  = icon("map"),
    
    layout_sidebar(
      
      sidebar = sidebar(
        width = 300,
        bg    = "#13132a",
        
        tags$h5("Demographics Explorer",
                style = "color: white; margin-bottom: 16px;"),
        
        selectInput(
          "selected_geo", "Geography",
          choices = c(
            "California + Nevada" = "both",
            "California only"     = "CA",
            "Nevada only"         = "NV"
          ),
          width = "100%"
        ),
        
        selectInput(
          "selected_var", "Variable",
          choices  = variable_choices,
          selected = "bach_plus",
          width    = "100%"
        ),
        
        selectInput(
          "selected_palette", "Color Palette",
          choices = c(
            "Viridis (purple \u2192 yellow)" = "viridis",
            "Inferno (black \u2192 orange)"  = "inferno",
            "Plasma (purple \u2192 yellow)"  = "plasma",
            "Magma (black \u2192 beige)"     = "magma",
            "Cividis (blue \u2192 yellow)"   = "cividis"
          ),
          width = "100%"
        ),
        
        selectInput(
          "map_style", "Map Style",
          choices = c(
            "Dark Matter" = "dark-matter",
            "Positron"    = "positron",
            "Voyager"     = "voyager"
          ),
          width = "100%"
        ),
        
        selectInput(
          "class_method", "Classification",
          choices = c(
            "Quantile (equal counts)" = "quantile",
            "Equal interval"          = "equal",
            "Jenks natural breaks"    = "jenks"
          ),
          width = "100%"
        ),
        
        hr(style = "border-color: #2a2a3a;"),
        
        actionButton(
          "update_map", "Update Map",
          icon  = icon("refresh"),
          width = "100%",
          class = "btn-primary"
        ),
        
        hr(style = "border-color: #2a2a3a;"),
        
        uiOutput("var_description"),
        
        hr(style = "border-color: #2a2a3a;"),
        
        tags$p(
          "Data: U.S. Census Bureau ACS 2018\u20132022.",
          tags$br(),
          "Census tract level. ACS estimates carry",
          "margins of error \u2014 see About tab.",
          tags$br(), tags$br(),
          "Hover over a tract for quick info.",
          tags$br(),
          "Click a tract for full detail.",
          style = "color: #555555; font-size: 0.75em;"
        )
      ),
      
      div(
        uiOutput("summary_stats"),
        tags$br(),
        card(
          card_header(uiOutput("map_title")),
          mapgl::maplibreOutput("main_map", height = "600px")
        ),
        tags$br(),
        card(
          card_header("County Comparison"),
          DTOutput("county_table")
        ),
        tags$br()
      )
    )
  ),
  
  # ==========================================================================
  # TAB 2: CA VS NV
  # ==========================================================================
  nav_panel(
    title = "CA vs NV",
    icon  = icon("chart-bar"),
    
    div(
      style = "padding: 20px;",
      
      tags$h3("California vs Nevada: By the Numbers",
              style = "color: white;"),
      tags$p(
        "Median values by census tract across both states.",
        style = "color: #aaaaaa;"
      ),
      
      tags$br(),
      
      fluidRow(
        column(6, card(
          card_header("Distribution Comparison"),
          plotOutput("comparison_plot", height = "500px")
        )),
        column(6, card(
          card_header("State Summary Statistics"),
          DTOutput("state_comparison_table")
        ))
      ),
      
      tags$br(),
      
      fluidRow(
        column(12, card(
          card_header("The Rent Burden Story"),
          plotOutput("rent_story_plot", height = "350px")
        ))
      )
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
        "Interactive demographic explorer for California and Nevada",
        "census tracts, using ACS 2018\u20132022 5-year estimates.",
        "Part of an ongoing series exploring U.S. Census data with R.",
        style = "color: #cccccc;"
      ),
      
      tags$h4("How to Use the Map",
              style = "color: white; margin-top: 24px;"),
      div(class = "info-card",
          tags$ul(
            style = "color: #cccccc; margin: 0; padding-left: 18px;",
            tags$li("Select a variable and geography from the sidebar"),
            tags$li("Click Update Map to render"),
            tags$li(
              "Drag the legend handles to filter by value range"
            ),
            tags$li("Hover over a tract for a quick summary"),
            tags$li("Click a tract for a detailed styled popup"),
            tags$li(
              "Use the camera icon (top right of map) to save a PNG"
            )
          )
      ),
      
      tags$h4("Connection to the Migration Project",
              style = "color: white; margin-top: 24px;"),
      div(class = "info-card",
          tags$p(
            "This app is a companion to the Tuolumne County",
            "Migration Explorer. The migration analysis found that",
            "people move from expensive California coastal counties",
            "to Nevada and rural California. This app maps the",
            "demographic patterns that help explain why: rent burden,",
            "income gaps, and housing availability.",
            style = "color: #cccccc; margin: 0;"
          )
      ),
      
      tags$h4("Data", style = "color: white; margin-top: 24px;"),
      tags$p(
        "U.S. Census Bureau, American Community Survey",
        "2018\u20132022 5-Year Estimates, census tract level.",
        "Accessed via the tidycensus R package.",
        style = "color: #cccccc;"
      ),
      
      tags$h4("Data Quality Note",
              style = "color: white; margin-top: 24px;"),
      div(class = "info-card",
          style = "border-left-color: #cc7b02;",
          tags$p(
            "ACS tract-level estimates carry margins of error,",
            "especially for small tracts or rare characteristics.",
            "Values shown are point estimates. Patterns across many",
            "tracts are more reliable than any individual value.",
            style = "color: #cccccc; margin: 0;"
          )
      ),
      
      tags$h4("Built With",
              style = "color: white; margin-top: 24px;"),
      tags$ul(
        style = "color: #cccccc;",
        tags$li(
          tags$a("mapgl",
                 href   = "https://walker-data.com/mapgl/",
                 target = "_blank",
                 style  = "color: #2d8b7b;"),
          " \u2014 MapLibre GL maps for R (Kyle Walker)"
        ),
        tags$li(
          tags$a("tidycensus",
                 href   = "https://walker-data.com/tidycensus/",
                 target = "_blank",
                 style  = "color: #2d8b7b;"),
          " \u2014 Census API access"
        ),
        tags$li(
          tags$a("Shiny + bslib",
                 href   = "https://shiny.posit.co/",
                 target = "_blank",
                 style  = "color: #2d8b7b;"),
          " \u2014 web app framework"
        )
      ),
      
      tags$h4("Related Project",
              style = "color: white; margin-top: 24px;"),
      tags$p(
        tags$a(
          "Tuolumne County Migration Explorer \u2192",
          href   = paste0(
            "https://brooksgroves.shinyapps.io/migration-explorer/"
          ),
          target = "_blank",
          style  = "color: #2d8b7b;"
        ),
        style = "color: #cccccc;"
      ),
      
      tags$h4("Source Code",
              style = "color: white; margin-top: 24px;"),
      tags$p(
        tags$a(
          "github.com/brooksgroves/census-r-experiments",
          href   = paste0(
            "https://github.com/brooksgroves/census-r-experiments"
          ),
          target = "_blank",
          style  = "color: #2d8b7b;"
        ),
        style = "color: #cccccc;"
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
      tags$p("Building map...",
             style = "color: white; margin-top: 16px;")
    ),
    color = "#0d0d1a"
  )
  
  # ==========================================================================
  # REACTIVES
  # ==========================================================================
  
  active_data <- reactive({
    switch(input$selected_geo,
           "CA"   = ca_tracts,
           "NV"   = nv_tracts,
           "both" = ca_nv_tracts
    )
  })
  
  active_col <- reactive({
    if (input$selected_var == "rent_burden") {
      "rent_burden"
    } else {
      paste0("estimate_", input$selected_var)
    }
  })
  
  # ==========================================================================
  # SIDEBAR OUTPUTS
  # ==========================================================================
  
  output$var_description <- renderUI({
    meta <- variable_meta[[input$selected_var]]
    div(
      class = "info-card",
      style = "border-left-color: #2d8b7b; margin-top: 8px;",
      tags$p(
        meta$note,
        style = "color: #aaaaaa; font-size: 0.8em; margin: 0;"
      )
    )
  })
  
  output$map_title <- renderUI({
    meta <- variable_meta[[input$selected_var]]
    geo  <- switch(input$selected_geo,
                   "CA"   = "California",
                   "NV"   = "Nevada",
                   "both" = "California & Nevada")
    tags$span(
      paste(meta$label, "\u2014", geo),
      style = "color: white;"
    )
  })
  
  # ==========================================================================
  # SUMMARY STAT BOXES
  # ==========================================================================
  
  output$summary_stats <- renderUI({
    data <- active_data()
    col  <- active_col()
    meta <- variable_meta[[input$selected_var]]
    
    vals <- data[[col]]
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NULL)
    
    is_dollar <- grepl("\\$|Income|Rent", meta$label)
    is_pct    <- grepl(
      "%|Rate|Born|Burden|Occupied|Vacancy|Moved|bach",
      meta$label
    )
    
    fmt <- function(x) {
      if (is_dollar)   paste0("$", format(round(x), big.mark = ","))
      else if (is_pct) paste0(round(x, 1), "%")
      else             as.character(round(x, 1))
    }
    
    fluidRow(
      column(2, div(class = "stat-box",
                    div(class = "stat-number", fmt(median(vals))),
                    div(class = "stat-label", "Median")
      )),
      column(2, div(class = "stat-box",
                    div(class = "stat-number", fmt(mean(vals))),
                    div(class = "stat-label", "Mean")
      )),
      column(2, div(class = "stat-box",
                    div(class = "stat-number", fmt(min(vals))),
                    div(class = "stat-label", "Min")
      )),
      column(2, div(class = "stat-box",
                    div(class = "stat-number", fmt(max(vals))),
                    div(class = "stat-label", "Max")
      )),
      column(2, div(class = "stat-box",
                    div(class = "stat-number",
                        format(sum(!is.na(data[[col]])), big.mark = ",")),
                    div(class = "stat-label", "Tracts")
      )),
      column(2, div(class = "stat-box",
                    div(class = "stat-number",
                        paste0(round(
                          sum(is.na(data[[col]])) / nrow(data) * 100, 1
                        ), "%")),
                    div(class = "stat-label", "Missing")
      ))
    )
  })
  
  # ==========================================================================
  # MAIN MAP
  # ==========================================================================
  
  output$main_map <- mapgl::renderMaplibre({
    
    w$show()
    on.exit(w$hide())
    
    data <- active_data()
    col  <- active_col()
    meta <- variable_meta[[input$selected_var]]
    
    pal_fn <- get_palette_fn(input$selected_palette)
    
    # Cap rent burden outliers before color scale calculation
    if (col == "rent_burden") {
      data <- data |>
        dplyr::mutate(!!col := pmin(.data[[col]], 150))
    }
    
    clean_data <- data |> dplyr::filter(!is.na(.data[[col]]))
    
    color_scale <- tryCatch(
      interpolate_palette(
        data    = clean_data,
        column  = col,
        method  = input$class_method,
        n       = 5,
        palette = pal_fn
      ),
      error = function(e) {
        interpolate_palette(
          data    = clean_data,
          column  = col,
          method  = "quantile",
          n       = 5,
          palette = pal_fn
        )
      }
    )
    
    # Determine value type once — scalar logicals
    is_dollar <- grepl("\\$|Income|Rent",   meta$label)
    is_pct    <- grepl(
      "%|Rate|Born|Burden|Occupied|Vacancy|Moved|bach",
      meta$label
    )
    
    # Build both tooltip columns inside mutate
    # tooltip (hover) = HTML with <br> line breaks — plain text \n is invisible
    # popup   (click) = richer styled HTML
    data_mapped <- data |>
      dplyr::mutate(
        
        # --- HOVER TOOLTIP: quick summary ---
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
          
          # FIX: use <br> not \n — mapgl renders tooltips as HTML
          paste0(
            tract_name, "<br>",
            county, ", ", state, "<br>",
            meta$label, ": <strong>", formatted_val, "</strong>"
          )
        },
        
        # --- CLICK POPUP: styled detail card ---
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
          
          paste0(
            "<div style='",
            "font-family: sans-serif;",
            "font-size: 13px;",
            "line-height: 1.6;",
            "min-width: 200px;",
            "padding: 4px 2px;",
            "'>",
            "<strong style='font-size:14px;'>",
            tract_name, "</strong><br>",
            "<span style='color:#666666; font-size:11px;'>",
            county, ", ", state,
            "</span>",
            "<hr style='margin:6px 0; border-color:#eeeeee;'>",
            "<span style='color:#444444;'>",
            meta$label, "</span><br>",
            "<strong style='font-size:16px; color:#2d8b7b;'>",
            formatted_val, "</strong>",
            "</div>"
          )
        }
      )
    
    maplibre(
      bounds = data_mapped,
      style  = carto_style(input$map_style)
    ) |>
      add_fill_layer(
        id                 = "demo_layer",
        source             = data_mapped,
        fill_color         = color_scale$expression,
        fill_opacity       = 0.75,
        fill_outline_color = "rgba(0,0,0,0.08)",
        tooltip            = "tooltip_plain",   # hover
        popup              = "tooltip_html"     # click
      ) |>
      add_legend(
        meta$label,
        values        = color_scale$breaks,
        colors        = color_scale$colors,
        type          = "continuous",
        layer_id      = "demo_layer",
        interactive   = TRUE,
        filter_column = col,
        position      = "bottom-left"
      ) |>
      add_screenshot_control(
        position    = "top-right",
        image_scale = 2
      )
    
  }) |>
    bindEvent(input$update_map, ignoreNULL = FALSE)
  
  # ==========================================================================
  # COUNTY COMPARISON TABLE
  # ==========================================================================
  
  output$county_table <- renderDT({
    data <- active_data()
    col  <- active_col()
    
    data |>
      sf::st_drop_geometry() |>
      dplyr::filter(!is.na(county), !is.na(.data[[col]])) |>
      dplyr::group_by(state, county) |>
      dplyr::summarise(
        median_val = round(median(.data[[col]], na.rm = TRUE), 1),
        mean_val   = round(mean(.data[[col]],   na.rm = TRUE), 1),
        tracts     = dplyr::n(),
        .groups    = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(median_val)) |>
      dplyr::rename(
        State  = state,
        County = county,
        Median = median_val,
        Mean   = mean_val,
        Tracts = tracts
      ) |>
      datatable(
        options  = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ) |>
      formatRound(columns = c("Median", "Mean"), digits = 1)
  })
  
  # ==========================================================================
  # COMPARE TAB: DISTRIBUTION PLOT
  # ==========================================================================
  
  output$comparison_plot <- renderPlot({
    
    col  <- active_col()
    meta <- variable_meta[[input$selected_var]]
    
    plot_data <- ca_nv_tracts |>
      sf::st_drop_geometry() |>
      dplyr::filter(!is.na(.data[[col]]))
    
    # Cap at 99th percentile
    q99 <- quantile(plot_data[[col]], 0.99, na.rm = TRUE)
    plot_data <- plot_data |> dplyr::filter(.data[[col]] <= q99)
    
    ggplot(plot_data,
           aes(x = .data[[col]], fill = state, color = state)) +
      
      geom_density(alpha = 0.4, linewidth = 0.8) +
      
      scale_fill_manual(values  = c("California" = "#2d8b7b",
                                    "Nevada"     = "#cc7b02")) +
      scale_color_manual(values = c("California" = "#2d8b7b",
                                    "Nevada"     = "#cc7b02")) +
      
      labs(
        title    = paste("Distribution:", meta$label),
        subtitle = "Census tract level | ACS 2018\u20132022",
        x        = meta$label,
        y        = "Density",
        fill     = NULL,
        color    = NULL,
        caption  = "Source: U.S. Census Bureau ACS 2018\u20132022"
      ) +
      
      theme_minimal() +
      theme(
        plot.background   = element_rect(fill = "#13132a", color = NA),
        panel.background  = element_rect(fill = "#13132a", color = NA),
        panel.grid        = element_line(color = "#2a2a3a"),
        panel.grid.minor  = element_blank(),
        plot.title        = element_text(color = "white",
                                         face = "bold", size = 13),
        plot.subtitle     = element_text(color = "#aaaaaa", size = 10),
        plot.caption      = element_text(color = "#555555", size = 8),
        axis.text         = element_text(color = "#cccccc"),
        axis.title        = element_text(color = "#888888"),
        legend.background = element_rect(fill = "#13132a", color = NA),
        legend.text       = element_text(color = "white", size = 11),
        legend.position   = "top"
      )
  })
  
  # ==========================================================================
  # COMPARE TAB: STATE SUMMARY TABLE
  # ==========================================================================
  
  output$state_comparison_table <- renderDT({
    
    summary_data <- purrr::map_dfr(names(variable_meta), function(var) {
      col <- if (var == "rent_burden") "rent_burden" else
        paste0("estimate_", var)
      
      if (!col %in% names(ca_nv_tracts)) return(NULL)
      
      ca_nv_tracts |>
        sf::st_drop_geometry() |>
        dplyr::filter(!is.na(.data[[col]])) |>
        dplyr::group_by(state) |>
        dplyr::summarise(
          median  = round(median(.data[[col]], na.rm = TRUE), 1),
          .groups = "drop"
        ) |>
        tidyr::pivot_wider(names_from = state, values_from = median) |>
        dplyr::mutate(
          Variable   = variable_meta[[var]]$label,
          Difference = round(Nevada - California, 1)
        ) |>
        dplyr::select(Variable, California, Nevada, Difference)
    })
    
    summary_data |>
      datatable(
        options  = list(pageLength = 15, dom = "t"),
        rownames = FALSE
      ) |>
      formatStyle(
        "Difference",
        color = styleInterval(0, c("#cc7b02", "#2d8b7b"))
      )
  })
  
  # ==========================================================================
  # COMPARE TAB: RENT BURDEN STORY PLOT
  # ==========================================================================
  
  output$rent_story_plot <- renderPlot({
    
    ca_nv_tracts |>
      sf::st_drop_geometry() |>
      dplyr::filter(!is.na(rent_burden), rent_burden <= 120) |>
      dplyr::mutate(
        burden_cat = dplyr::case_when(
          rent_burden <  30 ~ "Not burdened\n(<30%)",
          rent_burden <  50 ~ "Cost burdened\n(30\u201350%)",
          TRUE               ~ "Severely burdened\n(>50%)"
        ),
        burden_cat = factor(burden_cat, levels = c(
          "Not burdened\n(<30%)",
          "Cost burdened\n(30\u201350%)",
          "Severely burdened\n(>50%)"
        ))
      ) |>
      dplyr::count(state, burden_cat) |>
      dplyr::group_by(state) |>
      dplyr::mutate(pct = n / sum(n)) |>
      dplyr::ungroup() |>
      
      ggplot(aes(x = burden_cat, y = pct, fill = state)) +
      
      geom_col(position = "dodge", alpha = 0.85, width = 0.6) +
      
      geom_text(
        aes(label = scales::percent(pct, accuracy = 0.1)),
        position = position_dodge(width = 0.6),
        vjust    = -0.4,
        color    = "white",
        size     = 3.5
      ) +
      
      scale_fill_manual(values = c("California" = "#2d8b7b",
                                   "Nevada"     = "#cc7b02")) +
      scale_y_continuous(
        labels = scales::percent,
        expand = expansion(mult = c(0, 0.15))
      ) +
      
      labs(
        title    = "Rent Burden: California vs Nevada",
        subtitle = paste0(
          "% of census tracts by burden category | ",
          "ACS 2018\u20132022"
        ),
        x        = NULL,
        y        = "Share of tracts",
        fill     = NULL,
        caption  = paste0(
          "Source: U.S. Census Bureau ACS 2018\u20132022\n",
          "Rent burden = annual gross rent as % of household income. ",
          "Tracts >120% excluded."
        )
      ) +
      
      theme_minimal() +
      theme(
        plot.background    = element_rect(fill = "#13132a", color = NA),
        panel.background   = element_rect(fill = "#13132a", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#2a2a3a"),
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(color = "white",
                                          face = "bold", size = 13),
        plot.subtitle      = element_text(color = "#aaaaaa", size = 10),
        plot.caption       = element_text(color = "#555555", size = 8),
        axis.text          = element_text(color = "#cccccc", size = 10),
        axis.title.y       = element_text(color = "#888888"),
        legend.background  = element_rect(fill = "#13132a", color = NA),
        legend.text        = element_text(color = "white", size = 11),
        legend.position    = "top"
      )
  })
  
} # end server

shinyApp(ui, server)