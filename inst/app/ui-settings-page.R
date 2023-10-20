settings_page <- fluidRow(
  column(
    width = 12, title = "Settings",
    box(
      width = NULL, title = "Settings",
      div(style = "margin-top: -10px;"),
      tags$h4("Processing Mode (animal / human):"),
      materialSwitch(inputId = "workflowHuman", label = "Human data workflow"),
      "If you want to analyse human data, select the toggle to change the processing mode - animal experiment data is enabled as default.",
      "The processing workflows differ based on the way FLASH processes whole-slide images from animal experiment or human data.",
      "For more information on how to structure the meta data for human or animal experiment data please visit the Help page within tRigon.", br(), br(),
      tags$h4("Keep feature calculations:"),
      materialSwitch(inputId = "keepCalculations", label = "Keep specimen level calculations"),
      "If you want to analyse features on specimen level it is best to do this with feature medians or means - as default they are excluded from the data.",
      "By enabling to keep these specimen-level calculations each feature will come with specimen-level median, mean and standard deviation after processing.",
      "Please note that enabling this will only work during processing and not after pathomics data was already processed."
    )
  )
)
