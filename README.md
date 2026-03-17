# Census R Experiments 🗺️

Exploring U.S. Census Bureau migration data using R —
flow maps, demographic patterns, spatial analysis,
and an interactive Shiny web app.

---

## 🚀 Live App: Migration Explorer

> **Try it:** [migration-explorer.shinyapps.io](https://migration-explorer.shinyapps.io)
> *(update this link after deploying to shinyapps.io)*

Select any US county to instantly explore:
- Where new residents come from
- Where departing residents go
- Distance patterns, neighbor scorecards, and key stories
- Side-by-side county comparison

Built with R + tidycensus. Data: U.S. Census Bureau ACS 2016–2020.

---

## 📍 Project: Tuolumne County Migration

> *Where do people from Tuolumne County, CA go —*
> *and where do newcomers come from?*

Tuolumne County sits in the Sierra Nevada foothills of California,
home to Yosemite's western gateway and about 55,000 residents.
Using ACS 5-year migration flow data (2016–2020), this project maps
and analyzes the county's domestic migration patterns.

---

### The Maps

**Where do Tuolumne County's new residents come from?**

![Inflow Map](tuolumne-migration/outputs/tuolumne_inflow_twitter.png)

---

**Where do Tuolumne County residents move to?**

![Outflow Map](tuolumne-migration/outputs/tuolumne_outflow_twitter.png)

---

**Both flows together — top 5 origins and destinations:**

![Combined Map](tuolumne-migration/outputs/tuolumne_combined_twitter.png)

---

### The Numbers

| Metric | Value | Confidence |
|---|---|---|
| People moved IN (2016–2020) | 4,426 | Moderate |
| People moved OUT (2016–2020) | 3,803 | Moderate |
| **Net migration** | **+623** | Moderate |
| Counties sending people here | 62 | High |
| Counties receiving people from here | 99 | High |
| Migration within California | 86% in / 75% out | High |
| Bay Area net contribution | +491 | Moderate |
| Biggest single relationship | Stanislaus Co. (900 exchanges) | Moderate |
| Biggest net drain | Sacramento Co. (–201) | Moderate |
| Weighted avg move distance | 275 miles | Moderate |

> **A note on data quality:** 94% of individual county-level flow
> estimates carry margins of error exceeding 50% of the estimate —
> normal for small-geography ACS migration data.
> Directional patterns across multiple counties are more reliable
> than any single number.

---

### The Stories in the Data

#### 🏔️ A Sierra-Wide Pattern

Every Sierra foothill county except Mariposa is a net migration
gainer. El Dorado County leads with +2,789 net migrants.
Tuolumne (+623) sits in the middle of the pack. This is a
regional story about California's housing costs pushing people
toward the mountains.

![Sierra Comparison](tuolumne-migration/outputs/tuolumne_sierra_comparison.png)

---

#### 🌊 The Bay Area Cash-Out

8 of 8 Bay Area counties favor Tuolumne over the reverse.
Alameda County (Oakland/Berkeley) sends nearly **12 people to
Tuolumne for every 1 who goes back**.

![Bay Area Analysis](tuolumne-migration/outputs/tuolumne_bayarea_deep.png)

| County | Moved In | Moved Out | Net | Ratio |
|---|---|---|---|---|
| Alameda | 270 | 23 | +247 | 11.7:1 |
| Contra Costa | 140 | 68 | +72 | 2.1:1 |
| Santa Clara | 80 | 0 | +80 | ∞ |
| San Francisco | 63 | 15 | +48 | 4.2:1 |
| Sonoma | 46 | 0 | +46 | ∞ |
| **Total** | **638** | **147** | **+491** | **4.3:1** |

---

#### 📉 Sacramento and Stockton Pull Hardest

Despite winning against the coast, Tuolumne loses to
Sacramento (–201) and San Joaquin/Stockton (–138).

---

#### 🧂 The Utah/Nevada Split

Nevada **sends** people TO Tuolumne (+158 net, mostly Las Vegas).
Utah **takes** people FROM Tuolumne (–122 net, mostly Salt Lake City).

---

#### 🎖️ The Military Pipeline

Cumberland County, NC (Fort Liberty) and Guadalupe County, TX
(near Randolph AFB) together account for **161 departures with
zero return flow**.

---

#### 🌋 Mariposa: The Exception

The only Sierra county losing people (–40 net). Top destination:
Clark County, NV (Las Vegas).

---

#### 🔀 Flow Patterns: Every County Mapped

![Flow Patterns](tuolumne-migration/outputs/tuolumne_flow_patterns.png)

---

#### 📏 How Far Do People Travel?

![Distance Chart](tuolumne-migration/outputs/tuolumne_distance_chart.png)

| Distance | Migrants | Share |
|---|---|---|
| Local (< 100 mi) | 1,241 | 28% |
| Regional (100–300 mi) | 2,395 | 54% |
| Mid-range (300–600 mi) | 526 | 12% |
| Long distance (600–1,500 mi) | 89 | 2% |
| Cross-country (1,500+ mi) | 175 | 4% |

Weighted average move distance: **275 miles**.

---

#### 📊 In vs Out: Head to Head

![Comparison Chart](tuolumne-migration/outputs/tuolumne_comparison_chart.png)

---

## 🗂️ Repository Structure

```
census-r-experiments/
│
├── README.md
├── .gitignore
│
├── tuolumne-migration/               ← analysis scripts + outputs
│   ├── tuolumne_migration.R          ← pull data, build maps, save outputs
│   ├── tuolumne_migration_twitter.R  ← Twitter/social images
│   ├── tuolumne_migration_explore.R  ← data exploration + charts
│   ├── tuolumne_migration_deep.R     ← deep dive analysis
│   └── outputs/                      ← all PNGs and HTML
│       ├── *.png
│       └── tuolumne_migration.html   ← interactive map (open in browser)
│
└── shiny-migration-explorer/         ← interactive web app
    ├── app.R                         ← main Shiny app
    ├── R/
    │   ├── get_migration.R           ← Census API fetch + caching
    │   ├── make_maps.R               ← leaflet map builders
    │   ├── make_charts.R             ← ggplot/plotly chart builders
    │   └── utils.R                   ← story generation helpers
    └── data/
        └── cache/                    ← cached API responses (gitignored)
```

---

## 📋 Scripts Reference

### Tuolumne Analysis Scripts

| Script | Purpose | Run Order |
|---|---|---|
| `tuolumne_migration.R` | Pull Census data, build arc lines, save base maps + interactive HTML | 1st |
| `tuolumne_migration_twitter.R` | Twitter/X optimized images with ranked data panels | 2nd |
| `tuolumne_migration_explore.R` | Summary stats, distance analysis, surprise flows | 3rd |
| `tuolumne_migration_deep.R` | MOE checks, Bay Area ratios, one-way pipelines, Sierra comparison | 4th |

**Always run `tuolumne_migration.R` first** — all other scripts
depend on objects it creates.

### Shiny App Scripts

| Script | Purpose |
|---|---|
| `app.R` | UI layout, server logic, reactive data flow |
| `R/get_migration.R` | Wraps `get_flows()` with file-based caching |
| `R/make_maps.R` | Builds leaflet arc maps for inflow/outflow |
| `R/make_charts.R` | Builds plotly charts and DT neighbor table |
| `R/utils.R` | Auto-generates Key Stories text from data |

---

## 🚀 Running the Shiny App Locally

```r
# From your project root in RStudio:
shiny::runApp(
  "shiny-migration-explorer",
  launch.browser = TRUE
)
```

First run for any county takes 15–30 seconds (Census API call).
Subsequent runs load from cache instantly.

---

## ⚙️ Setup

**Requirements:**
- R ≥ 4.1
- RStudio (recommended)
- Free Census API key from
  [api.census.gov](https://api.census.gov/data/key_signup.html)

**First time only:**
```r
tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)
# Restart R after running this
```

**Install required packages:**
```r
install.packages(c(
  "tidycensus", "tidyverse", "sf", "ggplot2",
  "leaflet", "geosphere", "scales", "viridis",
  "htmlwidgets", "here", "patchwork", "ggrepel",
  "shiny", "bslib", "waiter", "plotly", "DT", "shinyjs"
))
```

Your API key is stored in `.Renviron` and is
**never committed to this repo** — `.gitignore` excludes it.
The app's data cache is also gitignored.

---

## 🛠️ Tools & Packages

| Package | Purpose |
|---|---|
| [`tidycensus`](https://walker-data.com/tidycensus/) | Census API — `get_flows()` for migration data |
| [`sf`](https://r-spatial.github.io/sf/) | Spatial data handling |
| [`ggplot2`](https://ggplot2.tidyverse.org/) | Static maps and charts |
| [`leaflet`](https://rstudio.github.io/leaflet/) | Interactive maps |
| [`geosphere`](https://cran.r-project.org/package=geosphere) | Great circle arc geometry |
| [`patchwork`](https://patchwork.data-imaginist.com/) | Combine ggplot panels |
| [`ggrepel`](https://ggrepel.slowkow.com/) | Non-overlapping chart labels |
| [`shiny`](https://shiny.posit.co/) | Web app framework |
| [`bslib`](https://rstudio.github.io/bslib/) | Modern Bootstrap themes |
| [`waiter`](https://waiter.john-coene.com/) | Loading screens |
| [`plotly`](https://plotly.com/r/) | Interactive charts |
| [`DT`](https://rstudio.github.io/DT/) | Interactive data tables |
| [`shinyjs`](https://deanattali.com/shinyjs/) | JavaScript helpers for Shiny |
| [`here`](https://here.r-lib.org/) | Project-relative file paths |

---

## 📊 Data Source

U.S. Census Bureau, American Community Survey
**County-to-County Migration Flows, 2016–2020 (5-Year Estimates)**
Accessed via the `tidycensus` R package and Census API.

- [ACS Migration Flows documentation](https://www.census.gov/topics/population/migration/guidance/county-to-county-migration-flows.html)
- [tidycensus `get_flows()` docs](https://walker-data.com/tidycensus/reference/get_flows.html)
- [Kyle Walker's tidycensus book](https://walker-data.com/census-r/)

---

## 📝 License

MIT — use freely, attribution appreciated.

---

*Built with R, tidycensus, and curiosity about a small mountain county.*
*Data: U.S. Census Bureau ACS 2016–2020.*