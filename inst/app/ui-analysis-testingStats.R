testingStats <- fluidRow(
  column(width = 4,
    shinydashboard::box(title= "Statistical Tests", width = NULL,
      div(style = "margin-top: -10px;"),
      # Input for selecting which variable to test for
      pickerInput(
        inputId = "feature_stats_variableSelect",
        label = "Select features",
        choices = "... no features found",
        options = list(
          'live-search'=TRUE), 
        multiple = FALSE
      ),
      # Grouping variable input
      textInput("groupVar_stats", "Enter comparison variable"),
      radioButtons("select_test", "Select test", list("pairwise Wilcoxon-Rank", "bootstrapped CI", "Kruskal-Wallis")),
      # Button for computation
      actionButton("statisticsBtn", "Compute Test", class = "btn-primary", icon = icon("play")),
      div(style = "margin-top: 5px"),
      downloadButton("download_statsdf", "Save Data"),
      div(style = "margin-top: 5px"),
      downloadButton("report_stats", "Save Report")
    )
  ),
  column(width = 8,
    shinydashboard::tabBox(width = NULL,
      # Output for displaying data and/or metadata summary
      tabPanel("Output",
       fluid = TRUE,
       verbatimTextOutput("stats_output")
     ),
     tabPanel("Data",
       fluid = TRUE,
      DT::dataTableOutput("stats_table"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
      ),
    )
  )
)