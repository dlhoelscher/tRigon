processingData <- fluidRow(
  column(width = 4,
    shinydashboard::box(title= "Process Data", width = NULL,
      div(style = "margin-top: -10px;"),
      # Input for selecting clinical data
      fileInput("featureData", "Select feature files",
                multiple = TRUE,
                accept = c(".csv"),
                buttonLabel = "Browse"
      ),
      div(style = "margin-top: -15px;"),
      # Input for selecting clinical data
      fileInput("metaData", "Select meta data",
                multiple = FALSE,
                accept = c(".xlsx"),
                buttonLabel = "Browse"
      ),
      div(style = "margin-top: -15px;"),
      # Button for processing
      actionButton("preprocessBtn", "Process", class = "btn-primary", icon = icon("play"), style = "color: primary"),
      div(style = "margin-top: 5px"),
      actionButton("findfeaturesBtn_processeddata", "Find Features", class = "btn-primary", icon = icon("play")),
      div(style = "margin-top: 5px"),
      downloadButton("download_processed", "Save Data"),
      div(style = "margin-top: 5px"),
      downloadButton("report_processeddata", "Save Report")
    )
  ),
  column(width = 8,
    shinydashboard::tabBox(width = NULL,
      # Output for displaying data and/or metadata summary
      tabPanel("Processed Data",
               fluid = TRUE,
               DT::dataTableOutput("SummaryTable"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
      ),
      tabPanel("Meta Data",
               fluid = TRUE,
               DT::dataTableOutput("MetadataTable"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
      )
    )
  )
)