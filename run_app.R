#' Run the Shiny Application
#'
#' This is the package entry point used by golem::run_dev() and by
#' calling susneoShiny::run_app() after installation.
#'
#' @param ... Named options passed to `golem::with_golem_options()`.
#' @return A shiny app object.
#' @export
#' @import shiny
#' @importFrom golem with_golem_options
run_app <- function(...) {
  with_golem_options(
    app = shinyApp(
      ui     = app_ui,
      server = app_server
    ),
    golem_opts = list(...)
  )
}
