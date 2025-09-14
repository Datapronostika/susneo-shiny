#' Clean Energy Data
#'
#' @description Utility function to standardize column names and match expected schema.
#'
#' @param df A data.frame or tibble with raw energy data.
#'
#' @return A cleaned tibble with consistent column names.
#' @export
#'
clean_energy_data <- function(df) {
  df <- df |>
    janitor::clean_names()
  
  if ("carbon_emission_in_kgco2e" %in% names(df)) {
    df <- df |> dplyr::rename(
      carbon_emission_kgco2e = carbon_emission_in_kgco2e
    )
  }
  
  if ("carbon_emissions_kgco2e" %in% names(df)) {
    df <- df |> dplyr::rename(
      carbon_emission_kgco2e = carbon_emissions_kgco2e
    )
  }
  if ("date" %in% names(df)) {
    df$date <- tryCatch({
      as.Date(df$date, tryFormats = c("%d-%m-%Y", "%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d"))
    }, error = function(e) {
      warning("Could not parse dates, keeping as character")
      df$date
    })
  }
  
  # Clean numeric columns
  if ("value" %in% names(df)) {
    df$value <- as.numeric(as.character(df$value))
  }
  
  if ("carbon_emission_kgco2e" %in% names(df)) {
    df$carbon_emission_kgco2e <- as.numeric(as.character(df$carbon_emission_kgco2e))
  }
  
  return(df)
}

