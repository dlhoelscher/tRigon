settings_page <- fluidRow(
  column(width = 12, title = "Settings",
    box(width = NULL, title = "Settings",
      div(style = "margin-top: -10px;"),
      tags$h4("Processing Mode (mouse / human):"),
      materialSwitch(inputId = "workflowHuman", label = "Human data workflow"),
      "If you want to analyse human data, select the toggle to change the processing mode - mouse data is enabled as default", br(),
      "mouse data: experiment data with supplied metadata file including an ID (as given in the feature files), position (the slide position on the WSI) and group (WT vs MT, etc.) column", br(),
      "human data: clinical data with the supplied metadata file including an ID (as given in the feature files), pseudonym (the sample identifier) and possible group (clinical data, diseases, etc.) column", br(),
      "The processing workflows differ based on the way FLASH processes whole-slide images from mouse or human data.", br(), br(),
      tags$h4("Keep feature calculations:"),
      materialSwitch(inputId = "keepCalculations", label = "Keep specimen level calculations"),
      "If you want to analyse features on specimen level it is best to do this with feature medians or means - as default they are excluded from the data.", br(),
      "By enabling to keep these specimen-level calculations each feature will come with specimen-level median, mean and standard deviation after processing.", br(),
      "Please note that enabling this will only work during processing and not after pathomics data was already processed."
    )
  )
)