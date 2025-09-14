# susneo-shiny
## Take-Home Assignment — R Shiny Dashboard (MVP)

**Status:** MVP delivered and **paused**.  
Includes: CSV upload, **date & facility filters**, **KPIs**, **time-series** chart, **comparison** chart (facility/type) and **summary data table**.  
Out of scope for this round: **CI pipeline** and **unit tests** — these would push well beyond the suggested 4–6 hours as CI/tests are new to me.

---

## Setup Instructions

1. **Clone the repository**
   ```bash
   git clone https://github.com/Datapronostika/susneo-shiny.git
   cd susneo-shiny
````

2. **Open** the project in **RStudio**.

3. **Install required dependencies**

   ```r
   install.packages(c(
     "shiny","ggplot2","dplyr","readr","DT","janitor","scales","lubridate","R6"
   ))
   # (Optional) golem dev helpers:
   # install.packages("golem")
   ```

4. **Run the app locally**

   ```r
   shiny::runApp()
   # or (during development with golem)
   # golem::run_dev()
   ```

---

## App Overview

Interactive dashboard to analyze energy consumption and carbon emissions across multiple facilities.

**Features**

* CSV upload or sample data
* **Filters:** Date range & multi-facility
* **KPIs:** Total consumption, Average consumption, Total emissions (kg CO₂e)
* **Charts:**

  * **Energy Consumption Over Time** (line)
  * **Total Consumption by Facility / Energy Type** (bar with selector)
* **Summary Data Table** (facility × energy type, totals/avg/min/max/emissions/records)

---

## Architecture

**Modular Shiny**

* `mod_data_upload`: CSV upload, preview, sample-data; time-series & comparison at upload step
* `mod_dashboard`: filters, KPIs, histogram (optional), time-series, comparison, summary table

**Business logic**

* Optional `BusinessLogic` (R6) to apply `date_range` and `site_filter` consistently

**Styling**

* Light CSS for cards/layout in `www/` (if present)

**Project structure**

```
app.R
R/
  mod_data_upload.R
  mod_dashboard.R
  utils_data_cleaning.R
  business_logic.R   # (if used by mod_dashboard)
www/                 # (optional assets)
README.md
DESCRIPTION
```

**Planned (not included in this MVP)**

```
tests/               # testthat suite
.github/workflows/   # CI pipeline (GitHub Actions)
```

---

## Data

Dataset includes:

* `id`: unique identifier
* `site`: facility code
* `date`: date of energy usage
* `type`: energy type
* `value`: energy usage amount
* `carbon_emission_kgco2e`: CO₂ emissions

Data is cleaned via `clean_energy_data()` in `R/utils_data_cleaning.R` (standardizes headers, parses dates/numerics, maps emission aliases).

If you click **Use sample data**, a demo dataset is generated and saved as `data/sample_data.csv`.

---

## Notes

* I paused here to avoid over-extending into CI and a full test suite within the timebox.
* The MVP demonstrates the core flow: upload → filters → KPIs → charts → summary table.
* The golem modular pattern was new to me and I found it genuinely interesting; I usually ship full-app Shiny structures and can adapt further if needed.

---

## Next steps (on request)

* Minimal **testthat** coverage: cleaning, aggregations, KPIs.
* **GitHub Actions** workflow for R CMD check + tests.
* Optional: emissions time-series, energy-type filter in the dashboard, CSV export of summaries.

```

