# Census R Experiments 🗺️

Exploring U.S. Census Bureau migration data using R —
flow maps, demographic patterns, and spatial analysis.

---

## 📍 Project: Tuolumne County Migration

> *Where do people from Tuolumne County, CA go —*
> *and where do newcomers come from?*

Tuolumne County sits in the Sierra Nevada foothills of California,
home to Yosemite's western gateway and about 55,000 residents.
Using ACS 5-year migration flow data (2016–2020), this project maps
and analyzes the county's domestic migration patterns — and finds
some genuinely surprising stories in the numbers.

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
> this is normal for small-geography ACS migration data.
> Directional patterns across multiple counties are more reliable
> than any single number. The Bay Area cash-out signal is supported
> by consistent results across all 8 Bay Area counties and is the
> most statistically robust finding in this analysis.

---

### The Stories in the Data

#### 🏔️ A Sierra-Wide Pattern

Every Sierra foothill county except Mariposa is a net migration
gainer. El Dorado County leads with +2,789 net migrants over the
period. Tuolumne (+623) sits in the middle of the pack. This is a
regional story about California's housing costs pushing people
toward the mountains.

![Sierra Comparison](tuolumne-migration/outputs/tuolumne_sierra_comparison.png)

---

#### 🗺️ Tuolumne's Unique Geography

The only Sierra foothill county whose biggest migration relationship
is with a fellow foothill county (Stanislaus) rather than a major
city. Every neighbor — Amador, Calaveras, El Dorado, Mariposa —
has a major metro as its top origin and destination.
Tuolumne sits in a different migration orbit.

**Neighbor county scorecard:**

| Neighbor | In | Out | Net | Verdict |
|---|---|---|---|---|
| Stanislaus | 490 | 410 | +80 | Massive churn, Tuolumne wins |
| San Joaquin | 99 | 237 | –138 | Stockton pulls people away |
| Calaveras | 66 | 115 | –49 | Neighbor wins |
| Fresno | 71 | 77 | –6 | Essentially balanced |
| Amador | 34 | 25 | +9 | Small Tuolumne win |
| Madera | 13 | 40 | –27 | Madera wins |

---

#### 🌊 The Bay Area Cash-Out

8 of 8 Bay Area counties favor Tuolumne over the reverse.
Alameda County (Oakland/Berkeley) sends nearly **12 people to
Tuolumne for every 1 who goes back**. People are cashing out
expensive Bay Area housing for Sierra Nevada foothills living.

![Bay Area Analysis](tuolumne-migration/outputs/tuolumne_bayarea_deep.png)

**Bay Area county breakdown:**

| County | Moved In | Moved Out | Net | Ratio |
|---|---|---|---|---|
| Alameda | 270 | 23 | +247 | 11.7:1 |
| Contra Costa | 140 | 68 | +72 | 2.1:1 |
| Santa Clara | 80 | 0 | +80 | ∞ |
| San Francisco | 63 | 15 | +48 | 4.2:1 |
| Sonoma | 46 | 0 | +46 | ∞ |
| Solano | 25 | 17 | +8 | 1.5:1 |
| Marin | 8 | 18 | –10 | 0.4:1 |
| San Mateo | 6 | 6 | 0 | 1:1 |
| **Total** | **638** | **147** | **+491** | **4.3:1** |

---

#### 📉 Sacramento and Stockton Pull Hardest

Despite winning against the coast, Tuolumne loses badly to
Sacramento (–201) and San Joaquin/Stockton (–138). Likely younger
residents seeking employment, healthcare, and urban services
available in the valley below.

---

#### 🧂 The Utah/Nevada Split

Nevada **sends** people TO Tuolumne (+158 net, mostly Las Vegas).
Utah **takes** people FROM Tuolumne (–122 net, mostly Salt Lake City).
Two neighboring states, completely opposite directions.

---

#### 🎖️ The Military Pipeline

Cumberland County, NC (Fort Liberty / formerly Fort Bragg) and
Guadalupe County, TX (near Randolph AFB / Fort Sam Houston)
together account for **161 departures with zero return flow** —
consistent with veterans or military families relocating near
bases after California service.

---

#### 🌋 Mariposa: The Exception

The only Sierra county losing people (–40 net). Most isolated,
smallest population. Its top destination is Clark County, NV
(Las Vegas) — a pattern shared by no other Sierra county.

---

#### 🔀 Flow Patterns: Every County Mapped

![Flow Patterns](tuolumne-migration/outputs/tuolumne_flow_patterns.png)

Each dot is one county. Counties above the dashed line send more
people to Tuolumne than they receive. Counties below lose more to
Tuolumne than they gain. Dot size = total migration volume.
Color = flow type.

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

The regional band captures the Bay Area, Sacramento, Central Valley,
and LA Basin — which explains why it dominates.
Weighted average move distance: **275 miles**.

---

#### 📊 In vs Out: Head to Head

![Comparison Chart](tuolumne-migration/outputs/tuolumne_comparison_chart.png)

---

### Scripts

| Script | Purpose | Run Order |
|---|---|---|
| [`tuolumne_migration.R`](tuolumne-migration/tuolumne_migration.R) | Pull Census data, build arc lines, save base maps and interactive HTML | 1st |
| [`tuolumne_migration_twitter.R`](tuolumne-migration/tuolumne_migration_twitter.R) | Twitter/X optimized images with ranked data panels | 2nd |
| [`tuolumne_migration_explore.R`](tuolumne-migration/tuolumne_migration_explore.R) | Summary stats, distance analysis, surprise flows | 3rd |
| [`tuolumne_migration_deep.R`](tuolumne-migration/tuolumne_migration_deep.R) | MOE checks, Bay Area ratios, one-way pipelines, Sierra comparison | 4th |

**Always run `tuolumne_migration.R` first** — all other scripts
depend on objects it creates.

---

### All Outputs

```
tuolumne-migration/outputs/
│
├── Maps
│   ├── tuolumne_inflow_map.png          static inflow flow map
│   ├── tuolumne_outflow_map.png         static outflow flow map
│   └── tuolumne_migration.html          interactive map (open in browser)
│
├── Twitter/Social
│   ├── tuolumne_inflow_twitter.png      map + top 10 origins
│   ├── tuolumne_outflow_twitter.png     map + top 10 destinations
│   └── tuolumne_combined_twitter.png   both flows + top 5 each
│
└── Charts
    ├── tuolumne_comparison_chart.png    in vs out diverging bar chart
    ├── tuolumne_distance_chart.png      migration by distance band
    ├── tuolumne_bayarea_deep.png        Bay Area county ratios
    ├── tuolumne_flow_patterns.png       scatter plot: flow type by county
    └── tuolumne_sierra_comparison.png   Sierra foothills county comparison
```

---

## 🛠️ Tools

| Package | Purpose |
|---|---|
| [`tidycensus`](https://walker-data.com/tidycensus/) | Census API — `get_flows()` for migration data |
| [`sf`](https://r-spatial.github.io/sf/) | Spatial data handling |
| [`ggplot2`](https://ggplot2.tidyverse.org/) | Static maps and charts |
| [`leaflet`](https://rstudio.github.io/leaflet/) | Interactive maps |
| [`geosphere`](https://cran.r-project.org/package=geosphere) | Great circle arc geometry |
| [`patchwork`](https://patchwork.data-imaginist.com/) | Combine ggplot panels |
| [`ggrepel`](https://ggrepel.slowkow.com/) | Non-overlapping chart labels |
| [`here`](https://here.r-lib.org/) | Project-relative file paths |

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

Your API key is stored in `.Renviron` and is
**never committed to this repo** — `.gitignore` excludes it.

---

## 📁 Repo Structure

```
census-r-experiments/
│
├── README.md
├── .gitignore
│
└── tuolumne-migration/
    ├── tuolumne_migration.R
    ├── tuolumne_migration_twitter.R
    ├── tuolumne_migration_explore.R
    ├── tuolumne_migration_deep.R
    └── outputs/
        ├── *.png
        └── *.html
```

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