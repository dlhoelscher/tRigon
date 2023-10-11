descriptiveStats <- fluidRow(
  tags$style(HTML(".selected {background-color: #3EB489 !important;}")),
  column(width = 4,
    shinydashboard::box(title = "Descriptive Statistics", width = NULL,
      div(style = "margin-top: -10px;"),
      # Input for selecting which variable to analyze
      pickerInput(
        inputId = "feature_ds_variableSelect",
        label = "Select features",
        choices = "... no features found",
        options = list(
          'live-search'=TRUE), 
        multiple = FALSE
      ),
      # Grouping variable input
      textInput("groupVar_ds", "Enter grouping variable"),
      # Button for statistics
      actionButton("ds_statisticsBtn", "Calculate", class = "btn-primary", icon = icon("play")),
      div(style = "margin-top: 5px"),
      downloadButton("download_ds", "Save Data"),
      div(style = "margin-top: 5px"),
      downloadButton("report_ds", "Save Report")
    )
  ),
  column(width = 8,
    shinydashboard::box(title= "Summary", width = NULL,
      # Output for displaying summary statistics
      fluid = TRUE,
      DT::dataTableOutput("dsTable"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
    )
  )
)