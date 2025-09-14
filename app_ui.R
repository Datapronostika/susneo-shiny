#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fluidPage(
      class = "susneo-page",
      title = "Energy Dashboard",
      
      # Header
      div(class = "app-header",
          h1("Energy Consumption & Carbon Emissions", class = "app-title"),
          p(class = "app-subtitle", "Upload your data → Explore KPIs → Drill into trends")
      ),
      
      # Upload section
      div(class = "section",
          h2(class = "section-title", "Upload data"),
          p(class = "section-help",
            "Upload a CSV. Keep column names consistent to enable quick KPIs and charts."),
          mod_data_upload_ui("data_upload")
      ),
      
      div(class = "divider"),
      
      # Dashboard section
      div(class = "section",
          h2(class = "section-title", "Dashboard"),
          p(class = "section-help",
            "Key metrics, trends, and distributions. Hover charts for details."),
          div(class = "dashboard-wrapper",
              mod_dashboard_ui("dashboard")
          )
      ),
      
      # Footer
      div(class = "app-footer",
          HTML("&copy; Susneo · Built with Shiny · Visual design tuned for clarity")
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    # Meta para responsive
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    # Fuente Google (Inter)
    tags$link(
      rel = "preconnect",
      href = "https://fonts.googleapis.com"
    ),
    tags$link(
      rel = "preconnect",
      href = "https://fonts.gstatic.com",
      crossorigin = "anonymous"
    ),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap",
      rel  = "stylesheet"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
    ,
    
    favicon(),
    
    # Bundle de recursos (incluye style.css si está en inst/app/www)
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "susneoShiny"
    ),
    
    # Asegura la carga del CSS principal (por si no usas {sass})
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
  )
}
