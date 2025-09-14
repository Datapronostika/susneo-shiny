#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  data <- mod_data_upload_server("data_upload")
  mod_dashboard_server("dashboard", data = data)
}
