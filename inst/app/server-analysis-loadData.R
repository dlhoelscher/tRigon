# loading data

# load data to df
loaded_df <- eventReactive(input$loaddataBtn, {
  if (is.null(input$preprocData)) {
    showNotification(ui = "Please select a file to upload", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(input$preprocData), "Please select a file to upload")
    )
  } else if (FALSE %in% (grepl(".csv", input$preprocData$datapath) | grepl(".xlsx", input$preprocData$datapath))) {
    showNotification(ui = "File format not supported", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(grepl(".csv", input$preprocData$datapath), "Selected file must be either a .csv or .xlsx file")
    )
  } else {
    withProgress(message = "Loading Data", value = 0, {
      req(input$preprocData)
      incProgress(1 / 3)
      if (grepl(".csv", input$preprocData$datapath)) {
        rvals$rval_loadeddf <- fread(input$preprocData$datapath)
      } else {
        rvals$rval_loadeddf <- read_excel(input$preprocData$datapath)
      }
      incProgress(2 / 3)
      names(rvals$rval_loadeddf) <- names(rvals$rval_loadeddf) %>%  make.names()
      names(rvals$rval_loadeddf) <- sub("%", "", names(rvals$rval_loadeddf))
      rvals$rval_loadeddf
    })
  }
})

# output loaded table
output$LoadedDataTable <- DT::renderDataTable(loaded_df())

#find features in loaded data
observeEvent(input$findfeaturesBtn_loadeddata, handlerExpr = {
  df_loaded <- rvals$rval_loadeddf
  if (is.null(dim(df_loaded))) {
    showNotification(ui = "Please load data before finding features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(df_loaded)), "Please load a data file before finding features")
    )}
  rvals$rval_chosendf <- "loaded"
  withProgress(message = "Finding Features", value = 0, {
    incProgress(3 / 6)
    Sys.sleep(1)
    updatePickerInput(session, "feature_ds_variableSelect",
                      choices = sort(names(loaded_df())),
                      selected = ""
    )
    updatePickerInput(session, "feature_p_variableSelect",
                      choices = sort(names(loaded_df())),
                      selected = ""
    )
    updatePickerInput(session, "feature_stats_variableSelect",
                      choices = sort(names(loaded_df())),
                      selected = ""
    )
    updatePickerInput(session, "feature_cluster_variableSelect",
                      choices = sort(names(loaded_df())),
                      selected = ""
    )
    updatePickerInput(session, "feature_c_fi_variableSelect",
                      choices = sort(names(loaded_df())),
                      selected = ""
    )
    updatePickerInput(session, "feature_r_fi_variableSelect",
                      choices = sort(names(loaded_df())),
                      selected = ""
    )
    updatePickerInput(session, "feature_scorr_variableSelect",
                      choices = sort(names(loaded_df())),
                      selected = ""
    )
    updatePickerInput(session, "feature_mcorr_variableSelect",
                      choices = sort(names(loaded_df())),
                      selected = ""
    )
  })
})

# download report for loaded data
output$report_loadeddata <- downloadHandler(
  filename = "report_data_loaded.html",
  content = function(file) {
    withProgress(
      message = paste0("Generating Report"),
      value = 0,
      {
        shiny::incProgress(1 / 5)
        src_img <- file.path(getwd(), "www/tRigon_logo.png")
        file.copy(src_img, "tRigon_logo.png")
        tempReport <- file.path(getwd(), "report_data_loaded.Rmd")
        file.copy("report_data_loaded.Rmd", tempReport, overwrite = FALSE)
        shiny::incProgress(2 / 5)
        # Set up parameters to pass to Rmd document
        params <- list(session_info = sessioninfo::session_info()$platform,
                       datapath_loadeddf = input$preprocData,
                       loaded_df = rvals$rval_loadeddf)
        shiny::incProgress(3 / 5)
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
        shiny::incProgress(4 / 5)
      }
    )
  }
)
