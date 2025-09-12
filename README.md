# susneo-shiny
# # Take-Home Assignment Round 1 for Susneo: R Shiny dashboard built with golem # #

# ---- Setup Instructions ------------------------------------------
1. Clone the repository:
    git clone https://github.com/Datapronostika/susneo-shiny-mdoloresperez.git

2. Open the project in RStudio.

3. Install required dependencies:
    install.packages(c("golem", "shiny", "dplyr", "ggplot2", "plotly", "DT", "testthat"))

4. Run the app locally:
    golem::run_dev()
   
# ----- App Overview ------------------------------------------------
This Shiny app provides an interactive dashboard for analyzing energy consumption and carbon 
emissions across multiple facilities.
* Features:

- CSV upload or sample data
- Date range & facility filters
- KPI cards (total consumption, average consumption, total emissions)
- Time series visualization
- Facility comparison visualization
- Summary data table
- Screenshot to be added here.

# ---- Architecture -------------------------------------------------
- Golem framework with modular design

- Modules:
  · mod_data_upload: data input & validation

  · mod_dashboard: KPIs, charts, tables

- R6 class: core business logic for data filtering & KPI calculations

#---- Expected repo structure 
R/                # Golem modules and functions
tests/            # Unit tests with testthat
inst/app/www/     # Assets
data/             # Sample dataset
.github/workflows # CI pipeline
DESCRIPTION       # Dependencies
README.md         # Documentation
app.R             # App launcher

# ----- Data --------------------------------------------------------
Dataset includes:

- id: unique identifier

- site: facility code

- date: date of energy usage

- type: energy type

- value: energy usage amount

- carbon_emission_kgco2e: CO₂ emissions

Sample data is provided in /data/sample_data.csv

# -----Testing -----------------------------------------------------

Unit tests are written with testthat and cover:

- Data validation

- KPI calculations

- Helper functions

Run tests locally:
  devtools::test()

# ---- CI/CD -------------------------------------------------------
This repo includes a GitHub Actions workflow (.github/workflows/ci.yml) that:

  - Installs dependencies

  - Runs tests

  - Executes R CMD check

# ----- Notes ------------------------------------------------------

- Time spent: 

- Assumptions: 

- Future improvements: 
