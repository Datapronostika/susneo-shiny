#' dashboard UI Function
#'
#' @description A shiny Module for displaying KPIs and charts.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList div span fluidRow column plotOutput textOutput radioButtons dateRangeInput selectInput
mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # FILTERS SECTION - Required by assignment
    div(class = "filters-section",
        h3("Filters"),
        fluidRow(
          column(6,
                 dateRangeInput(
                   ns("date_range"), 
                   "Select Date Range:",
                   start = "2025-08-01",
                   end = "2025-09-30",
                   format = "yyyy-mm-dd"
                 )
          ),
          column(6,
                 selectInput(
                   ns("site_filter"), 
                   "Select Facilities:", 
                   choices = NULL,
                   multiple = TRUE,
                   selected = NULL
                 )
          )
        ),
        br()
    ),
    
    # KPI cards (use classes defined in style.css)
    div(class = "kpi-grid",
        div(class = "kpi-card",
            div(class = "kpi-topline",
                span(class = "kpi-label", "Total Consumption"),
                span(class = "kpi-pill", "kWh")
            ),
            div(class = "kpi-value", textOutput(ns("kpi_total"))),
            div(class = "kpi-delta up", textOutput(ns("kpi_delta_total")))
        ),
        div(class = "kpi-card",
            div(class = "kpi-topline",
                span(class = "kpi-label", "Average Consumption"),
                span(class = "kpi-pill", "kWh")
            ),
            div(class = "kpi-value", textOutput(ns("kpi_avg")))
        ),
        div(class = "kpi-card",
            div(class = "kpi-topline",
                span(class = "kpi-label", "Total Emissions"),
                span(class = "kpi-pill", "kg CO2e")
            ),
            div(class = "kpi-value", textOutput(ns("kpi_emissions")))
        )
    ),
    
    # Original histogram plot
    div(class = "plot-card",
        plotOutput(ns("plot_consumption"), height = "360px")
    ),
    
    # Assignment visualizations
    fluidRow(
      column(
        width = 6,
        div(class = "plot-card",
            plotOutput(ns("plot_timeseries"), height = "360px")  # Time series
        )
      ),
      column(
        width = 6,
        div(class = "plot-card",
            div(style = "margin-bottom: 8px;",
                radioButtons(
                  ns("cmp_dim"), "Comparison by:",
                  choices  = c("Facility" = "site", "Energy type" = "type"),
                  selected = "site",
                  inline   = TRUE
                )
            ),
            plotOutput(ns("plot_comparison"), height = "360px")  # Comparison
        )
      )
    ),
    
    # SUMMARY DATA TABLE - Required by assignment
    br(),
    div(class = "plot-card",
        h4("Summary Data Table"),
        DT::dataTableOutput(ns("summary_table"))
    )
  )
}

#' dashboard Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer renderText renderPlot req validate need observe updateSelectInput
mod_dashboard_server <- function(id, data) {
  moduleServer(id, function(input, output, session){
    
    # Update site filter choices when data changes
    observe({
      req(data())
      if ("site" %in% names(data())) {
        sites <- unique(data()$site)
        sites <- sites[!is.na(sites)]
        updateSelectInput(
          session, 
          "site_filter",
          choices = sites,
          selected = sites  # Select all by default
        )
      }
    })
    
    # REACTIVE FILTERED DATA using BusinessLogic R6 class
    filtered_data <- reactive({
      req(data())
      
      # Initialize BusinessLogic with current data
      bl <- BusinessLogic$new(data())
      
      # Apply filters
      date_range <- input$date_range
      selected_sites <- input$site_filter
      
      # Use BusinessLogic filter method
      filtered <- bl$filter_data(
        site = selected_sites,
        date_range = date_range
      )
      
      return(filtered)
    })
    
    # --- KPIs using filtered data ---
    output$kpi_total <- renderText({
      req(filtered_data())
      validate(need("value" %in% names(filtered_data()), "No data available"))
      total <- sum(filtered_data()$value, na.rm = TRUE)
      scales::comma(total)
    })
    
    # Simple week-over-week delta demo
    output$kpi_delta_total <- renderText({
      req(filtered_data())
      if (all(c("date","value") %in% names(filtered_data()))) {
        df <- filtered_data()[, c("date","value")]
        df <- df[!is.na(df$date), ]
        if (nrow(df) < 2) return("")
        # split by median date to fake previous vs current (illustrative only)
        mid <- stats::median(as.numeric(as.Date(df$date)))
        cur <- sum(df$value[as.numeric(as.Date(df$date)) > mid], na.rm = TRUE)
        prev <- sum(df$value[as.numeric(as.Date(df$date)) <= mid], na.rm = TRUE)
        if (prev == 0) return("")
        delta <- (cur - prev) / prev
        paste0(ifelse(delta >= 0, "▲ ", "▼ "),
               scales::percent(delta, accuracy = 0.1))
      } else ""
    })
    
    output$kpi_avg <- renderText({
      req(filtered_data())
      validate(need("value" %in% names(filtered_data()), "No data available"))
      avg <- mean(filtered_data()$value, na.rm = TRUE)
      scales::comma(round(avg, 1))
    })
    
    output$kpi_emissions <- renderText({
      req(filtered_data())
      validate(need("carbon_emission_kgco2e" %in% names(filtered_data()), "No data available"))
      total_em <- sum(filtered_data()$carbon_emission_kgco2e, na.rm = TRUE)
      scales::comma(total_em)
    })
    
    # --- Plot: histogram with filtered data ---
    output$plot_consumption <- renderPlot({
      req(filtered_data())
      validate(need("value" %in% names(filtered_data()), "No 'value' column found."))
      
      library(ggplot2)
      library(scales)
      
      v <- suppressWarnings(as.numeric(filtered_data()[["value"]]))
      v <- v[is.finite(v)]
      validate(need(length(v) > 0, "No numeric values in 'value'."))
      
      ggplot(data.frame(value = v), aes(x = value)) +
        geom_histogram(bins = 8, fill = "#34495e", color = "white", linewidth = 0.4) +
        scale_x_continuous(labels = comma) +
        labs(
          title = "Energy Usage Distribution (Filtered)",
          x = "Energy Consumption (kWh)",
          y = "Frequency"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          panel.grid.minor = element_blank()
        )
    })
    
    # ── TIME SERIES with filtered data ─────────────────────────────────────────
    output$plot_timeseries <- renderPlot({
      req(filtered_data())
      validate(
        need(all(c("date","value") %in% names(filtered_data())), "Need 'date' and 'value' columns.")
      )
      
      df <- filtered_data()[, c("date","value")]
      df <- df[!is.na(df$date) & is.finite(df$value), , drop = FALSE]
      validate(need(nrow(df) > 0, "No valid rows to plot."))
      
      ts <- dplyr::group_by(df, date) |>
        dplyr::summarise(consumption = sum(value, na.rm = TRUE), .groups = "drop")
      
      ggplot2::ggplot(ts, ggplot2::aes(x = date, y = consumption)) +
        ggplot2::geom_line(linewidth = 0.9, color = "#3498db") +
        ggplot2::geom_point(size = 1.4, color = "#3498db") +
        ggplot2::labs(
          title = "Energy Consumption Over Time (Filtered)",
          x = "Date", 
          y = "Energy Consumption"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text = ggplot2::element_text(size = 10),
          axis.title = ggplot2::element_text(size = 11),
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::scale_y_continuous(labels = scales::comma)
    })
    
    # ── COMPARISON with filtered data ────────────────────────────────────
    output$plot_comparison <- renderPlot({
      req(filtered_data())
      by <- input$cmp_dim
      if (is.null(by)) by <- "site"
      
      validate(
        need("value" %in% names(filtered_data()), "Missing 'value' column."),
        need(by %in% names(filtered_data()), sprintf("Column '%s' doesn't exist in data.", by))
      )
      
      df <- filtered_data()
      df <- df[!is.na(df[[by]]) & is.finite(df$value), , drop = FALSE]
      validate(need(nrow(df) > 0, "No data available for comparison."))
      
      cmp <- dplyr::group_by(df, .data[[by]]) |>
        dplyr::summarise(consumption = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(consumption))
      
      ggplot2::ggplot(
        cmp,
        ggplot2::aes(x = stats::reorder(.data[[by]], consumption), y = consumption)
      ) +
        ggplot2::geom_col(fill = "#e74c3c", alpha = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste("Total Consumption by", ifelse(by == "site", "Facility", "Energy Type"), "(Filtered)"),
          x = NULL, 
          y = "Total Consumption"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text = ggplot2::element_text(size = 10),
          axis.title = ggplot2::element_text(size = 11),
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::scale_y_continuous(labels = scales::comma)
    })
    
    # ── SUMMARY DATA TABLE ────────────────────────────────────────────────────
    output$summary_table <- DT::renderDataTable({
      req(filtered_data())
      
      # Create comprehensive summary table
      summary_df <- filtered_data() |>
        dplyr::group_by(site, type) |>
        dplyr::summarise(
          total_consumption = sum(value, na.rm = TRUE),
          avg_consumption = round(mean(value, na.rm = TRUE), 2),
          max_consumption = max(value, na.rm = TRUE),
          min_consumption = min(value, na.rm = TRUE),
          total_emissions = sum(carbon_emission_kgco2e, na.rm = TRUE),
          records_count = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(total_consumption))
      
      # Format numbers for better display
      summary_df$total_consumption <- scales::comma(summary_df$total_consumption)
      summary_df$avg_consumption <- scales::comma(summary_df$avg_consumption)
      summary_df$max_consumption <- scales::comma(summary_df$max_consumption)
      summary_df$min_consumption <- scales::comma(summary_df$min_consumption)
      summary_df$total_emissions <- scales::comma(summary_df$total_emissions)
      
      # Rename columns for better display
      names(summary_df) <- c("Facility", "Energy Type", "Total Consumption", 
                             "Avg Consumption", "Max Consumption", "Min Consumption",
                             "Total Emissions (kg CO2e)", "Record Count")
      
      summary_df
    }, options = list(
      scrollX = TRUE,
      pageLength = 10,
      searching = TRUE,
      ordering = TRUE,
      info = TRUE
    ))
  })
}