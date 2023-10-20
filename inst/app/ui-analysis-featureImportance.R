featureImportance <- fluidRow(
  column(
    width = 4,
    shinydashboard::tabBox(
      id = "fi_input", width = NULL,
      tabPanel(
        "Classification",
        div(style = "margin-top: -10px;"),
        # feature importance variables input
        pickerInput(
          inputId = "feature_c_fi_variableSelect",
          label = "Select features",
          choices = "... no features found",
          options = list(
            `actions-box` = TRUE,
            size = 12, "live-search" = TRUE
          ),
          multiple = TRUE
        ),
        # grouping variable input
        textInput("dependentVar_c_fi", "Enter dependent variable"),
        radioButtons("select_c_fimethod", "Select method", list("random forest", "recursive feature elimination")),
        conditionalPanel(
          condition = "input.select_c_fimethod=='recursive feature elimination'",
          sliderInput(
            inputId = "c_n_folds", "number of folds",
            value = 5, step = 1,
            min = 2, max = 10, round = T
          )
        ),
        conditionalPanel(
          condition = "input.select_c_fimethod=='recursive feature elimination'",
          sliderInput(
            inputId = "c_n_repeats", "number of repeats",
            value = 5, step = 1,
            min = 1, max = 10, round = T
          )
        ),
        actionButton("compute_c_FiBtn", "Compute", class = "btn-primary", icon = icon("play")),
        div(style = "margin-top: 5px"),
        downloadButton("download_c_fiplot", "Save Plot"),
        div(style = "margin-top: 5px"),
        downloadButton("report_c_fi", "Save Report")
      ),
      tabPanel(
        "Regression",
        div(style = "margin-top: -10px;"),
        # feature importance variables input
        pickerInput(
          inputId = "feature_r_fi_variableSelect",
          label = "Select features",
          choices = "... no features found",
          options = list(
            `actions-box` = TRUE,
            size = 12, "live-search" = TRUE
          ),
          multiple = TRUE
        ),
        # grouping variable input
        textInput("dependentVar_r_fi", "Enter dependent variable"),
        radioButtons("select_r_fimethod", "Select method", list("random forest", "recursive feature elimination")),
        conditionalPanel(
          condition = "input.select_r_fimethod=='recursive feature elimination'",
          sliderInput(
            inputId = "r_n_folds", "number of folds",
            value = 5, step = 1,
            min = 2, max = 10, round = T
          )
        ),
        conditionalPanel(
          condition = "input.select_r_fimethod=='recursive feature elimination'",
          sliderInput(
            inputId = "r_n_repeats", "number of repeats",
            value = 5, step = 1,
            min = 1, max = 10, round = T
          )
        ),
        actionButton("compute_r_FiBtn", "Compute", class = "btn-primary", icon = icon("play")),
        div(style = "margin-top: 5px"),
        downloadButton("download_r_fiplot", "Save Plot"),
        div(style = "margin-top: 5px"),
        downloadButton("report_r_fi", "Save Report")
      )
    )
  ),
  column(
    width = 8,
    shinydashboard::tabBox(
      width = NULL,
      tabPanel("Importance Summary",
        fluid = TRUE,
        verbatimTextOutput("importance_summary")
      ),
      tabPanel("Importance Plot",
        fluid = TRUE,
        plotOutput("importance_plot")
      )
    )
  )
)
