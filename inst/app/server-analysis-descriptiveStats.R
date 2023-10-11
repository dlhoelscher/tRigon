# descriptive statistics

# calculate summary statistics
ds_data <- reactive({
  withProgress(message = "Calculating", value = 0, {
    var = c(input$feature_ds_variableSelect, input$groupVar_ds)
    feature <- c(input$feature_ds_variableSelect)
    df_loaded <- rvals$rval_loadeddf
    df_processed <- rvals$rval_processeddf
    if (rvals$rval_chosendf == "processed") {
      if (!is.null(dim(df_processed))) {
        df_var <- df_processed
      }
    }
    if (rvals$rval_chosendf == "loaded") {
      if (!is.null(dim(df_loaded))) {
        df_var <- df_loaded
      }
    }
    if (!is.numeric(df_var[[feature]])) {
      showNotification(ui = "Please select a numeric input as feature", type = "error", duration = NULL, closeButton = TRUE)
      validate(
        need(is.numeric(df_var[[feature]]), "Please select a numeric input as feature")
      )
    }
    group_col <- input$groupVar_ds
    g <- factor(df_var[[group_col]])
    if (NA %in% g) {
      rvals$na_omit <- TRUE
      df_na <- df_var[which(is.na(df_var[[group_col]])), ]
      df_na <- df_na[which(!is.na(df_na[[feature]])), ]
      rvals$n_na <- count(df_na)
      g <- na.omit(g)
      df_var <- df_var[!is.na(df_var[[group_col]]),]
    } else {
      rvals$na_omit <- FALSE
      rvals$n_na <- 0
    }
    n_levels <- length(unique(df_var[[group_col]]))
    rvals$groups_lvl <- unique(df_var[[group_col]])
    if (is.numeric(df_var[[input$groupVar_ds]])) {
      showNotification(ui = "Warning: group var is numeric", type = "warning", duration = 5, closeButton = TRUE)
    }
    if (length(unique(df_var[[input$groupVar_ds]])) > 50) {
      showNotification(ui = "Please select fewer groups", type = "error", duration = NULL, closeButton = TRUE)
      validate(
        need(length(unique(df_var[[input$groupVar_ds]])) <= 50, paste0(length(unique(df_var[[input$groupVar_ds]])), " groups reported. Cannot process more than 50 unique groups."))
      )
    }
    summary_stats <- data.frame()
    i <- 1
    incProgress(i / n_levels)
    for (group_factor in levels(g)) {
      group_df <- df_var[which(df_var[[group_col]] == group_factor), ]
      temp_stats <- summary(group_df[[feature]], na.rm = TRUE)
      temp_stats$var <- var(group_df[[feature]], na.rm = TRUE)
      temp_stats$sd <- sd(group_df[[feature]], na.rm = TRUE)
      temp_stats$iqr <- IQR((group_df[[feature]]), na.rm = TRUE)
      n_datapoints <- group_df %>% filter(!is.na(group_df[[feature]])) %>% count()
      temp_stats$n_datapoints <- n_datapoints$n
      temp_stats$group_factor <- group_factor
      temp_stats <- as.data.frame(temp_stats) %>% dplyr::select(group_factor, Min., X1st.Qu., Median, Mean, X3rd.Qu., Max., var, sd, iqr, n_datapoints)
      summary_stats <- rbind(summary_stats, temp_stats)
      incProgress(i / n_levels)
      i <- i + 1
    }
    summary_stats$feature <- feature
    summary_stats <- summary_stats %>% dplyr::rename(group = "group_factor", min = "Min.", max = "Max.", median = "Median", mean = "Mean", Q1 = "X1st.Qu.", Q3 = "X3rd.Qu.")
    summary_stats <- summary_stats %>% dplyr::select(group, feature, median, mean, var, sd, min, max, Q1, Q3, iqr, n_datapoints)
    summary_stats[, c(3:11)] <- sapply(summary_stats[, c(3:11)], as.numeric)
    summary_stats <- summary_stats %>% mutate_if(is.numeric, ~ round(., 2))
    summary_stats <-  summary_stats %>% rename_at(1, ~ input$groupVar_ds)
    summary_stats
  })
  rvals$rval_dsdf <- summary_stats
})

# write descriptive statistics
output$dsTable <- DT::renderDataTable(ds_df())

ds_df <- eventReactive(input$ds_statisticsBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No preprocessed data loaded"),
      need(!is.null(dim(rvals$rval_processeddf)), "No processed data loaded")
    )
  }
  if (is.null(input$feature_ds_variableSelect)) {
    showNotification(ui = "Please select a feature", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(input$feature_ds_variableSelect), "Please select a feature for analysis")
    )
  }
  if (input$feature_ds_variableSelect == "... no features found") {
    showNotification(ui = "Please select a feature", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$feature_ds_variableSelect != "... no features found", "Please select a feature for analysis")
    )
  }
  if (input$groupVar_ds == "") {
    showNotification(ui = "Please specify a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_ds != "", "Please specify a group variable")
    )
  }
  if (!(input$groupVar_ds %in% names(rvals$rval_loadeddf) | input$groupVar_ds %in% names(rvals$rval_processeddf))) {
    showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_ds %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
      need(input$groupVar_ds %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
    )
  }
  if (input$groupVar_ds %in% input$feature_ds_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$groupVar_ds %in% input$feature_ds_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  } else {
    ds_data()
  }
})

# download descriptive statistics
output$download_ds <- downloadHandler(
  filename = function() {
    paste("feature_statistics_", input$feature_ds_variableSelect, ".xlsx", sep = "")
  },
  content = function(file) {
    withProgress(
      message = paste0("Saving Data"),
      value = 0,
      {
        shiny::incProgress(1 / 10)
        Sys.sleep(1)
        shiny::incProgress(5 / 10)
        writexl::write_xlsx(rvals$rval_dsdf, file)
      }
    )
  }
)

# download report for descriptive statistics
output$report_ds <- downloadHandler(
  filename = "report_descriptive_stats.html",
  content = function(file) {
    withProgress(
      message = paste0("Generating Report"),
      value = 0,
      {
        shiny::incProgress(1 / 5)
        src_img <- file.path(getwd(), "www/tRigon_logo.png")
        file.copy(src_img, "tRigon_logo.png")
        tempReport <- file.path(getwd(), "report_ds.Rmd")
        file.copy("report_ds.Rmd", tempReport, overwrite = FALSE)
        shiny::incProgress(2 / 5)
        # Set up parameters to pass to Rmd document
        params <- list(session_info = devtools::session_info()$platform,
                       feature_var = input$feature_ds_variableSelect,
                       group_var = input$groupVar_ds,
                       groups = rvals$groups_lvl,
                       ds_df = rvals$rval_dsdf,
                       na_omit = rvals$na_omit,
                       na_n = rvals$n_na)
        shiny::incProgress(3 / 5)
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
        shiny::incProgress(4 / 5)
      }
    )
  }
)