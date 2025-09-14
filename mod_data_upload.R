#' data_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Data Input Section
    fluidRow(
      column(6,
             fileInput(ns("file"), "Upload CSV", accept = ".csv")
      ),
      column(6,
             br(),
             actionButton(ns("use_sample"), "Use sample data", class = "btn-info")
      )
    ),
    
    # Data Preview
    h4("Data Preview"),
    DT::dataTableOutput(ns("preview")),
    
    br(),
    
    # Visualization Controls
    conditionalPanel(
      condition = "output.has_data",
      ns = ns,
      fluidRow(
        column(6,
               h4("Time Series Analysis")
        ),
        column(6,
               h4("Comparison Analysis"),
               selectInput(ns("cmp_dim"), "Compare by:", 
                           choices = list("Facility" = "site", "Energy Type" = "type"),
                           selected = "site")
        )
      ),
      
      # Plots
      fluidRow(
        column(6,
               plotOutput(ns("plot_timeseries"), height = "400px")
        ),
        column(6,
               plotOutput(ns("plot_comparison"), height = "400px")
        )
      )
    )
  )
}

#' data_upload Server Functions
#'
#' @noRd 
mod_data_upload_server <- function(id){
  moduleServer(id, function(input, output, session){
    data <- reactiveVal(NULL)
    
    # File upload handler
    observeEvent(input$file, {
      req(input$file)
      tryCatch({
        df <- readr::read_csv(input$file$datapath, show_col_types = FALSE) |>
          clean_energy_data()
        data(df)
        showNotification("File uploaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
      })
    })
    
    # Sample data handler - FIXED VERSION
    observeEvent(input$use_sample, {
      tryCatch({
        # Create sample data matching the exact structure from assignment
        sample_data <- data.frame(
          id = 1:100,
          site = sample(c("VDRK", "AKBB", "GFBN", "GECE"), 100, replace = TRUE),
          date = sample(seq.Date(from = as.Date("2025-08-01"), 
                                 to = as.Date("2025-09-30"), 
                                 by = "day"), 100, replace = TRUE),
          type = sample(c("Water", "Electricity", "Waste", "Gas"), 100, replace = TRUE),
          value = sample(8000:96000, 100, replace = TRUE),
          carbon_emission_kgco2e = sample(25:80, 100, replace = TRUE)
        )
        
        # Ensure data directory exists
        if (!dir.exists("data")) dir.create("data", recursive = TRUE)
        readr::write_csv(sample_data, "data/sample_data.csv")
        
        # Clean and set the data
        df <- sample_data |> clean_energy_data()
        data(df)
        showNotification("Sample data loaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error loading sample data:", e$message), type = "error")
      })
    })
    
    # Data preview table
    output$preview <- DT::renderDataTable({
      req(data())
      head(data(), 10)
    }, options = list(scrollX = TRUE, pageLength = 5))
    
    # Check if data exists (for conditional panel)
    output$has_data <- reactive({
      !is.null(data())
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)
    
    # ── TIME SERIES: consumption over time ─────────────────────────────────────────
    output$plot_timeseries <- renderPlot({
      req(data())
      validate(
        need(all(c("date","value") %in% names(data())), "Need 'date' and 'value' columns.")
      )
      
      df <- data()[, c("date","value")]
      df <- df[!is.na(df$date) & is.finite(df$value), , drop = FALSE]
      validate(need(nrow(df) > 0, "No valid rows to plot."))
      
      ts <- dplyr::group_by(df, date) |>
        dplyr::summarise(consumption = sum(value, na.rm = TRUE), .groups = "drop")
      
      ggplot2::ggplot(ts, ggplot2::aes(x = date, y = consumption)) +
        ggplot2::geom_line(linewidth = 0.9, color = "steelblue") +
        ggplot2::geom_point(size = 1.4, color = "steelblue") +
        ggplot2::labs(
          title = "Energy Consumption Over Time",
          x = "Date", 
          y = "Energy Consumption"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text = ggplot2::element_text(size = 10),
          axis.title = ggplot2::element_text(size = 11)
        ) +
        ggplot2::scale_y_continuous(labels = scales::comma)
    })
    
    # ── COMPARISON: by Facility or Energy type ────────────────────────────────────
    output$plot_comparison <- renderPlot({
      req(data())
      by <- input$cmp_dim
      if (is.null(by)) by <- "site"
      
      validate(
        need("value" %in% names(data()), "Missing 'value' column."),
        need(by %in% names(data()), sprintf("Column '%s' doesn't exist in data.", by))
      )
      
      df <- data()
      df <- df[!is.na(df[[by]]) & is.finite(df$value), , drop = FALSE]
      validate(need(nrow(df) > 0, "No data available for comparison."))
      
      cmp <- dplyr::group_by(df, .data[[by]]) |>
        dplyr::summarise(consumption = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(consumption))
      
      ggplot2::ggplot(
        cmp,
        ggplot2::aes(x = stats::reorder(.data[[by]], consumption), y = consumption)
      ) +
        ggplot2::geom_col(fill = "lightcoral", alpha = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste("Total Consumption by", ifelse(by == "site", "Facility", "Energy Type")),
          x = NULL, 
          y = "Total Consumption"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text = ggplot2::element_text(size = 10),
          axis.title = ggplot2::element_text(size = 11)
        ) +
        ggplot2::scale_y_continuous(labels = scales::comma)
    })
    
    return(data)
  })
}