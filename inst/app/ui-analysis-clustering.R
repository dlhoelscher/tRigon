clustering <- fluidRow(
  column(width = 4,
    shinydashboard::box(title= "k-means Clustering", width = NULL,
      div(style = "margin-top: -10px;"),
      # Input for selecting which variables to analyze
      pickerInput(
        inputId = "feature_cluster_variableSelect",
        label = "Select features", selected = "",
        choices = "...no features found",
        options = list(
          `actions-box` = TRUE, 
          size = 12, 'live-search'=TRUE
        ), 
        multiple = TRUE
      ),
      sliderInput("cluster_n", "Number of clusters",
        min = 2, max = 12,
        value = 6
      ),
      switchInput(inputId = "cluster_groups_en", "show groups", size = "normal", labelWidth = "100px"),
      conditionalPanel(
        condition = "input.cluster_groups_en==1",
        textInput(inputId = "groupVar_cluster", "Enter grouping variable")
      ),
      actionButton("clusterBtn", "Run Algorithm", class = "btn-primary", icon = icon("play")),
      div(style = "margin-top: 5px"),
      downloadButton("download_clusterplot", "Save Plot"),
      div(style = "margin-top: 5px"),
      downloadButton("report_clustering", "Save Report")
    )
  ),
  column(width = 8,
    shinydashboard::tabBox(width = NULL,
      tabPanel("Summary",
        fluid = TRUE,
        verbatimTextOutput("cluster_output")
      ),
      tabPanel("Plot",
        fluid = TRUE,
        plotOutput("cluster_plot")
      )
    )
  )
)
