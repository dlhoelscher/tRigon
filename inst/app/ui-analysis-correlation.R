correlation <- fluidRow(
  column(width = 4,
    shinydashboard::tabBox(id = "corr_input", width = NULL,
      tabPanel("Simple",
               div(style = "margin-top: -10px;"),
               #features input for correlation
               pickerInput(
                 inputId = "feature_scorr_variableSelect",
                 label = "Select features", selected = "",
                 choices = "... no features found",
                 options = list(
                   `actions-box` = TRUE, 
                   size = 12, 'live-search'=TRUE
                 ), 
                 multiple = TRUE
               ),
               # grouping input
               switchInput(inputId = "input_scorr_subgroups", "subgroups", labelWidth = "100px"),
               conditionalPanel(
                 condition = "input.input_scorr_subgroups==1",
                 textInput(inputId = "groupVar_scorr", "Enter grouping variable")
               ),
               conditionalPanel(
                 condition = "input.input_scorr_subgroups==1",
                 textInput(inputId = "groupVar_scorr_name", "Enter subgroup name")
               ),
               actionButton("s_correlationBtn", "Compute", class = "btn-primary", icon = icon("play")),
               div(style = "margin-top: 5px"),
               downloadButton("download_s_corrplot", "Save Plot"),
               div(style = "margin-top: 5px"),
               downloadButton("download_s_corrdf", "Save Data"),
               div(style = "margin-top: 5px"),
               downloadButton("report_s_corr", "Save Report")      
      ),
      tabPanel("Multiple",
        div(style = "margin-top: -10px;"),
        #features input for correlation
        pickerInput(
          inputId = "feature_mcorr_variableSelect",
          label = "Select features", selected = "",
          choices = "... no features found",
          options = list(
            `actions-box` = TRUE, 
            size = 12, 'live-search'=TRUE
          ), 
          multiple = TRUE
        ),
        # grouping input
        switchInput(inputId = "input_mcorr_subgroups", "subgroups", labelWidth = "100px"),
        conditionalPanel(
          condition = "input.input_mcorr_subgroups==1",
          textInput(inputId = "groupVar_mcorr", "Enter grouping variable")
        ),
        conditionalPanel(
          condition = "input.input_mcorr_subgroups==1",
          textInput(inputId = "groupVar_mcorr_name", "Enter subgroup name")
        ),
        actionButton("m_correlationBtn", "Compute", class = "btn-primary", icon = icon("play")),
        div(style = "margin-top: 5px"),
        downloadButton("download_m_corrplot", "Save Plot"),
        div(style = "margin-top: 5px"),
        downloadButton("download_m_corrdf", "Save Data"),
        div(style = "margin-top: 5px"),
        downloadButton("report_m_corr", "Save Report")
      )
    )
  ),
  column(width = 8,
    shinydashboard::tabBox(width = NULL,
      tabPanel("Correlation Plot",
        fluid = TRUE,
        plotOutput(outputId = "corr_plot", width = "100%")
      ),
      tabPanel("Correlation Data",
        fluid = TRUE,
        DT::dataTableOutput("corr_table"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
      )
    )
  )
)