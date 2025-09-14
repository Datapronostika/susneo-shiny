# app.R — simple launcher (no golem)
library(shiny)
source("R/mod_data_upload.R")
source("R/mod_dashboard.R")
source("R/utils_data_cleaning.R")

ui <- fluidPage(
  titlePanel("Susneo — Energy Dashboard (MVP)"),
  mod_data_upload_ui("upload"),
  tags$hr(),
  mod_dashboard_ui("dashboard")
)
server <- function(input, output, session){
  data <- mod_data_upload_server("upload")  # devuelve reactive()
  mod_dashboard_server("dashboard", data = data)
}
shinyApp(ui, server)
