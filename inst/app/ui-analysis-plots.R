plots <- fluidRow(
  column(width = 4,
    shinydashboard::box(title = "Plots", width = NULL,
      div(style = "margin-top: -10px;"),
      # Input for selecting which variable to plot
      pickerInput(
        inputId = "feature_p_variableSelect",
        label = "Select features",
        choices = "... no features found",
        options = list(
          'live-search'=TRUE), 
        multiple = FALSE
      ),
      # Grouping variable input
      textInput("groupVar_p", "Enter grouping variable"),
      # specify which plot format
      selectInput(
        inputId = "p_PlotSelect",
        label = "Select graph",
        choices = list("...", Plots = c("Violin plot", "Boxplot", "Violin with Boxplot", "Ridgeline")),
        multiple = FALSE, selectize = FALSE
      ),
      switchInput(inputId = "logscale_switch", "log-scale", labelWidth = "70px"),
      actionButton("plotBtn", "Plot", class = "btn-primary", icon = icon("play")),
      div(style = "margin-top: 5px"),
      downloadButton("download_plot", "Save Plot"),
      div(style = "margin-top: 5px"),
      downloadButton("report_plot", "Save Report")
    )
  ),
  column(width = 8,
    shinydashboard::box(title = "Feature Plot", width = NULL,
      # Output for displaying plots"
      fluid = TRUE,
      plotOutput("Feature_Plot")
    )
  )
)