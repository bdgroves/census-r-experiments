# Census R Experiments

Exploring U.S. Census Bureau data using R — migration flows, 
demographic maps, and spatial analysis.

## Tools
- [`tidycensus`](https://walker-data.com/tidycensus/) — Census API access
- [`sf`](https://r-spatial.github.io/sf/) — spatial data
- [`ggplot2`](https://ggplot2.tidyverse.org/) — static maps
- [`leaflet`](https://rstudio.github.io/leaflet/) — interactive maps
- [`geosphere`](https://cran.r-project.org/package=geosphere) — great circle arcs

## Projects

### Tuolumne County Migration Flow Map
**Folder:** `tuolumne-migration/`

Radial flow map showing where Tuolumne County, CA residents 
move to and where new residents come from, using ACS 
5-year estimates (2016–2020).

**Outputs:**
- Static inflow map (PNG)
- Static outflow map (PNG)  
- Interactive map with toggleable layers (HTML)

**Data source:** U.S. Census Bureau, American Community Survey,
County-to-County Migration Flows, 2016–2020.

## Setup

1. Install R and RStudio
2. Get a free Census API key at https://api.census.gov/data/key_signup.html
3. Open any project script and follow the setup section at the top

## Notes
- Census API key is never stored in this repo (it lives in your local `.Renviron`)
- Output PNG and HTML files are included for reference