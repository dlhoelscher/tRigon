loadingData <- fluidRow(
  column(width = 4,
    shinydashboard::box(title = "Load Data", width = NULL,
      div(style = "margin-top: -10px;"),
      # Input for uploading already processed data
      fileInput("preprocData", "Select processed data", multiple = FALSE, accept = c(".csv")),
      div(style = "margin-top: -15px;"),
      # Button for processing 
      actionButton("loaddataBtn", "Load", class = "btn-primary", icon = icon("play")),
      actionButton("findfeaturesBtn_loadeddata", "Find Features", class = "btn-primary", icon = icon("play")),
      div(style = "margin-top: 5px"),
      downloadButton("report_loadeddata", "Save Report")
    )
  ),
  column(width = 8,
    shinydashboard::box(title = "Data", width = NULL,
      # Output for displaying loaded data
      fluid = TRUE,
      DT::dataTableOutput("LoadedDataTable"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
    )
  )
)