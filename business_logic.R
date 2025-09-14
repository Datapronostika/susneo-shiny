BusinessLogic <- R6::R6Class(
  "BusinessLogic",
  public = list(
    #' @field data The energy data
    data = NULL,
    
    #' Initialize the business logic
    #' @param data A data.frame with energy consumption data
    initialize = function(data = NULL) {
      if (!is.null(data)) {
        self$data <- data
        self$validate_data()
      }
    },
    
    #' Validate data structure
    validate_data = function() {
      required_cols <- c("date", "value")
      missing_cols <- setdiff(required_cols, names(self$data))
      if (length(missing_cols) > 0) {
        stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      }
    },
    
    #' Filter data by site and date range
    #' @param site Character vector of sites to include
    #' @param date_range Date vector of length 2 (start, end)
    #' @return Filtered data.frame
    filter_data = function(site = NULL, date_range = NULL) {
      filtered_data <- self$data
      
      if (!is.null(site) && "site" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$site %in% site, ]
      }
      
      if (!is.null(date_range) && "date" %in% names(filtered_data)) {
        filtered_data <- filtered_data[
          filtered_data$date >= date_range[1] & 
            filtered_data$date <= date_range[2], 
        ]
      }
      
      return(filtered_data)
    },
    
    #' Calculate Key Performance Indicators
    #' @param filtered_data Optional pre-filtered data
    #' @return List of KPI values
    calculate_kpis = function(filtered_data = NULL) {
      if (is.null(filtered_data)) {
        filtered_data <- self$data
      }
      
      if (is.null(filtered_data) || nrow(filtered_data) == 0) {
        return(list(
          total_consumption = 0,
          average_consumption = 0,
          total_emissions = 0,
          max_consumption = 0,
          min_consumption = 0,
          consumption_std = 0
        ))
      }
      
      kpis <- list(
        total_consumption = sum(filtered_data$value, na.rm = TRUE),
        average_consumption = mean(filtered_data$value, na.rm = TRUE),
        max_consumption = max(filtered_data$value, na.rm = TRUE),
        min_consumption = min(filtered_data$value, na.rm = TRUE),
        consumption_std = sd(filtered_data$value, na.rm = TRUE)
      )
      
      if ("carbon_emission_kgco2e" %in% names(filtered_data)) {
        kpis$total_emissions <- sum(filtered_data$carbon_emission_kgco2e, na.rm = TRUE)
      } else {
        kpis$total_emissions <- 0
      }
      
      return(kpis)
    },
    
    #' Generate summary statistics
    #' @return Data.frame with summary statistics
    get_summary_stats = function() {
      if (is.null(self$data)) return(NULL)
      
      numeric_cols <- sapply(self$data, is.numeric)
      summary_stats <- lapply(self$data[numeric_cols], function(x) {
        c(
          mean = mean(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          min = min(x, na.rm = TRUE),
          max = max(x, na.rm = TRUE),
          na_count = sum(is.na(x))
        )
      })
      
      return(do.call(rbind, summary_stats))
    }
  )
)