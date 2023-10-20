# feature importance calculation

# classification fi
df_c_fi <- eventReactive(input$compute_c_FiBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (input$dependentVar_c_fi == "") {
    showNotification(ui = "Please specify a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_c_fi != "", "Please specify a group variable")
    )
  }
  if (!(input$dependentVar_c_fi %in% names(rvals$rval_loadeddf) | input$dependentVar_c_fi %in% names(rvals$rval_processeddf))) {
    showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_c_fi %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
      need(input$dependentVar_c_fi %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
    )
  }
  if (length(input$feature_c_fi_variableSelect) < 4) {
    showNotification(ui = "Please select min. 4 features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$feature_c_fi_variableSelect > 3, paste("Please select at least 4 features for ", input$select_c_fimethod, sep = " ")),
    )
  }
  if (input$dependentVar_c_fi %in% input$feature_c_fi_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$dependentVar_c_fi %in% input$feature_c_fi_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  } else {
    calc_c_fi()
  }
})

# classification calculate feature importance object
calc_c_fi <- eventReactive(input$compute_c_FiBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (input$dependentVar_c_fi == "") {
    showNotification(ui = "Please specify a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_c_fi != "", "Please specify a group variable")
    )
  }
  if (!(input$dependentVar_c_fi %in% names(rvals$rval_loadeddf) | input$dependentVar_c_fi %in% names(rvals$rval_processeddf))) {
    showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_c_fi %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
      need(input$dependentVar_c_fi %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
    )
  }
  if (length(input$feature_c_fi_variableSelect) < 4) {
    showNotification(ui = "Please select min. 4 features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$feature_c_fi_variableSelect > 3, paste("Please select at least 4 features for ", input$select_c_fimethod, sep = "")),
    )
  }
  if (input$dependentVar_c_fi %in% input$feature_c_fi_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$dependentVar_c_fi %in% input$feature_c_fi_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  } else {
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
    group_col <- input$dependentVar_c_fi
    features <- input$feature_c_fi_variableSelect
    g <- factor(df_var[[group_col]])
    if (NA %in% g) {
      rvals$na_omit <- TRUE
      df_na <- df_var[which(is.na(df_var[[group_col]])), ]
      df_na <- as.data.frame(subset(df_na, select = c(features)))
      rvals$n_na <- count(df_na[complete.cases(df_na), ])
      g <- na.omit(g)
      df_var <- df_var[!is.na(df_var[[group_col]]), ]
    } else {
      rvals$na_omit <- FALSE
      rvals$n_na <- 0
    }
    rvals$groups_lvl <- unique(df_var[[group_col]])
    if (is.numeric(df_var[[input$dependentVar_c_fi]])) {
      showNotification(ui = "Numeric dependent var - consider regression", type = "warning", duration = 5, closeButton = TRUE)
    }
    if (length(unique(df_var[[input$dependentVar_c_fi]])) >= 20) {
      showNotification(ui = "Please select fewer groups or consider doing regression instead of classification", type = "error", duration = NULL, closeButton = TRUE)
      validate(
        need(length(unique(df_var[[input$dependentVar_c_fi]])) < 20, paste0(length(unique(df_var[[input$dependentVar_c_fi]])), " groups reported. Cannot process more than 20 unique groups for classification. Did you mean to do regression?"))
      )
    }
    df_var[[group_col]] <- as.character(df_var[[group_col]])
    if (input$select_c_fimethod == "random forest") {
      withProgress(
        message = paste0("Growing Trees"),
        value = 0,
        {
          incProgress(1 / 5)
          df_rf <- as.data.frame(subset(df_var, select = c(input$dependentVar_c_fi, features)))
          df_check <- df_rf %>% select(-input$dependentVar_c_fi)
          var_types <- sapply(df_check, is.numeric)
          if (FALSE %in% var_types) {
            showNotification(ui = "Please select only numeric inputs", type = "error", duration = NULL, closeButton = TRUE)
            nonnum_var <- names(which(FALSE == var_types))
            validate(
              need(!(FALSE %in% var_types), paste0("non-numeric feature selected: ", nonnum_var, sep = ""))
            )
          }
          rvals$data_warn <- FALSE
          rvals$data_warn_n <- 0
          df_check <- colSums(!is.na(df_check))
          if (length(unique(df_check)) != 1) {
            showNotification(ui = "Unequal vector length - data loss", type = "warning", duration = 3, closeButton = TRUE)
            rvals$data_warn <- TRUE
            df_check <- df_rf[rowSums(is.na(df_rf)) != ncol(df_rf), ]
            rvals$data_warn_n <- sum(!complete.cases(df_check))
          }
          incProgress(2 / 5)
          df_rf <- df_rf[complete.cases(df_rf), ]
          obs_groups <- df_rf %>%
            group_by_at(input$dependentVar_c_fi) %>%
            count()
          if (any(obs_groups$n == 1)) {
            showNotification(ui = "Warning: only one observation in group", type = "warning", duration = 3, closeButton = TRUE)
          }
          df_rf <- df_rf %>% mutate_if(is.numeric, scale)
          incProgress(3 / 5)
          df_rf <- df_rf %>% mutate(dependentVar_c_fiRF = factor(df_rf[[input$dependentVar_c_fi]]))
          incProgress(4 / 5)
          df_rf <- df_rf[, -1]
          fi <- randomForest(dependentVar_c_fiRF ~ ., data = df_rf, importance = TRUE, na.action = na.exclude)
        }
      )
    } else if (input$select_c_fimethod == "recursive feature elimination") {
      rfe_c_cont <- rfeControl(
        functions = rfFuncs,
        method = "repeatedcv",
        number = input$c_n_folds,
        repeats = input$c_n_repeats
      )
      n_c_fi <- length(input$feature_c_fi_variableSelect)
      withProgress(
        message = paste0("Computing RFE"),
        value = 0,
        {
          incProgress(1 / 3)
          fi <- caret::rfe(x = x_train_c_rfe(), y = y_train_c_rfe(), size = n_c_fi, rfeControl = rfe_c_cont)
        }
      )
    }
    fi
  }
})

# classification create feature importance plot object
output_c_fi_plot <- eventReactive(input$compute_c_FiBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (input$dependentVar_c_fi == "") {
    showNotification(ui = "Please specify a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_c_fi != "", "Please specify a group variable")
    )
  }
  if (!(input$dependentVar_c_fi %in% names(rvals$rval_loadeddf) | input$dependentVar_c_fi %in% names(rvals$rval_processeddf))) {
    showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_c_fi %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
      need(input$dependentVar_c_fi %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
    )
  }
  if (length(input$feature_c_fi_variableSelect) < 4) {
    showNotification(ui = "Please select min. 4 features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$feature_c_fi_variableSelect > 3, paste("Please select at least 4 features for", input$select_c_fimethod, sep = "")),
    )
  }
  if (input$dependentVar_c_fi %in% input$feature_c_fi_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$dependentVar_c_fi %in% input$feature_c_fi_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  } else {
    obj_fi <- df_c_fi()
    if (input$select_c_fimethod == "random forest") {
      withProgress(
        message = paste0("Rendering Plot"),
        value = 0,
        {
          incProgress(1 / 2)
          V <- as.data.frame(obj_fi$importance)
          rf_plot_accuracy <- ggplot2::ggplot(V, aes(y = reorder(rownames(V), MeanDecreaseAccuracy), x = MeanDecreaseAccuracy)) +
            geom_point(color = "black", size = 4, alpha = 1) +
            geom_segment(aes(y = rownames(V), yend = rownames(V), x = 0, xend = MeanDecreaseAccuracy), color = "black") +
            ylab("features") +
            xlab("Mean Decrease Accuracy") +
            theme_bw() +
            theme(legend.position = "none") +
            theme(axis.text = element_text(size = 7, family = "sans")) +
            theme(axis.title = element_text(size = 10, family = "sans")) +
            theme(plot.title = element_text(size = 12, family = "sans")) +
            theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
            theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
          rf_plot_accuracy <- rf_plot_accuracy + geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey")
          rf_plot_GINI <- ggplot2::ggplot(V, aes(y = reorder(rownames(V), MeanDecreaseGini), x = MeanDecreaseGini)) +
            geom_point(color = "black", size = 4, alpha = 1) +
            geom_segment(aes(y = rownames(V), yend = rownames(V), x = 0, xend = MeanDecreaseGini), color = "black") +
            ylab("features") +
            xlab("Mean Decrease Gini") +
            theme_bw() +
            theme(legend.position = "none") +
            theme(axis.text = element_text(size = 7, family = "sans")) +
            theme(axis.title = element_text(size = 10, family = "sans")) +
            theme(plot.title = element_text(size = 12, family = "sans")) +
            theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
            theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
          rf_plot_GINI <- rf_plot_GINI + geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey")
          rf_plots <- (rf_plot_accuracy + rf_plot_GINI)
          name <- paste("RandomForest_Classification_FeatureImportancePlot", ".png", sep = "")
          ggsave(name, rf_plots, path = tempdir())
        }
      )
    } else if (input$select_c_fimethod == "recursive feature elimination") {
      withProgress(
        message = paste0("Rendering Plot"),
        value = 0,
        {
          incProgress(1 / 2)
          varimp_data <- data.frame(
            feature = row.names(varImp(obj_fi))[1:length(predictors(obj_fi))],
            obj_fi[["fit"]][["importance"]]
          )
          rf_plot_accuracy <- ggplot(data = varimp_data, aes(y = reorder(rownames(varimp_data), MeanDecreaseAccuracy), x = MeanDecreaseAccuracy)) +
            geom_point(color = "black", size = 4, alpha = 1) +
            geom_segment(aes(y = feature, yend = feature, x = 0, xend = MeanDecreaseAccuracy), color = "black") +
            labs(y = "features", x = "Mean Decrease Accuracy") +
            geom_text(aes(label = round(MeanDecreaseAccuracy, 2)), vjust = 1.6, color = "white", size = 4) +
            theme_bw() +
            theme(legend.position = "none") +
            theme(axis.text = element_text(size = 7, family = "sans")) +
            theme(axis.title = element_text(size = 10, family = "sans")) +
            theme(plot.title = element_text(size = 12, family = "sans")) +
            theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
            theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
          rf_plot_accuracy <- rf_plot_accuracy + geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey")
          rf_plot_GINI <- ggplot(data = varimp_data, aes(y = reorder(rownames(varimp_data), MeanDecreaseGini), x = MeanDecreaseGini)) +
            geom_point(color = "black", size = 4, alpha = 1) +
            geom_segment(aes(y = feature, yend = feature, x = 0, xend = MeanDecreaseGini), color = "black") +
            labs(y = "features", x = "Mean Decrease Gini") +
            geom_text(aes(label = round(MeanDecreaseGini, 2)), vjust = 1.6, color = "white", size = 4) +
            theme_bw() +
            theme(legend.position = "none") +
            theme(axis.text = element_text(size = 7, family = "sans")) +
            theme(axis.title = element_text(size = 10, family = "sans")) +
            theme(plot.title = element_text(size = 12, family = "sans")) +
            theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
            theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
          rf_plot_GINI <- rf_plot_GINI + geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey")
          rf_plots <- (rf_plot_accuracy + rf_plot_GINI)
          name <- paste("RFE_Classification_FeatureImportancePlot", ".png", sep = "")
          ggsave(name, rf_plots, path = tempdir())
        }
      )
    }
    rvals$rval_fi_plot <- rf_plots
    rf_plots
  }
})

# output importance plot regression or classification
output$importance_plot <- renderPlot({
  if (input$fi_input == "Classification") {
    output_c_fi_plot()
  } else if (input$fi_input == "Regression") {
    output_r_fi_plot()
  }
})

# output summary regression or classification
output$importance_summary <- renderPrint({
  if (input$fi_input == "Classification") {
    output_c_fi()
  } else if (input$fi_input == "Regression") {
    output_r_fi()
  }
})

# write classification feature importance output
output_c_fi <- eventReactive(input$compute_c_FiBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (input$dependentVar_c_fi == "") {
    showNotification(ui = "Please specify a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_c_fi != "", "Please specify a group variable")
    )
  }
  if (!(input$dependentVar_c_fi %in% names(rvals$rval_loadeddf) | input$dependentVar_c_fi %in% names(rvals$rval_processeddf))) {
    showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_c_fi %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
      need(input$dependentVar_c_fi %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
    )
  }
  if (length(input$feature_c_fi_variableSelect) < 4) {
    showNotification(ui = "Please select min. 4 features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$feature_c_fi_variableSelect > 3, paste("Please select at least 4 features for", input$select_c_fimethod, sep = "")),
    )
  }
  if (input$dependentVar_c_fi %in% input$feature_c_fi_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$dependentVar_c_fi %in% input$feature_c_fi_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  } else {
    if (input$select_c_fimethod == "random forest") {
      a <- df_c_fi()
      b <- importance(a)
      out_fi <- list(a, b)
    } else if (input$select_c_fimethod == "recursive feature elimination") {
      a <- df_c_fi()
      b <- a[["fit"]][["importance"]]
      c <- rfe_c_test()
      out_fi <- list(a, b, c)
    }
    rvals$rval_fi_output <- out_fi
    out_fi
  }
})

# classification rfe
rfe_c_data <- reactive({
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
  features <- input$feature_c_fi_variableSelect
  df_rfe <- as.data.frame(subset(df_var, select = c(input$dependentVar_c_fi, features)))
  df_check <- df_rfe %>% select(-input$dependentVar_c_fi)
  var_types <- sapply(df_check, is.numeric)
  if (FALSE %in% var_types) {
    showNotification(ui = "Please select only numeric inputs", type = "error", duration = NULL, closeButton = TRUE)
    nonnum_var <- names(which(FALSE == var_types))
    validate(
      need(!(FALSE %in% var_types), paste0("non-numeric feature selected: ", nonnum_var, sep = ""))
    )
  }
  rvals$data_warn <- FALSE
  rvals$data_warn_n <- 0
  df_check <- colSums(!is.na(df_check))
  if (length(unique(df_check)) != 1) {
    showNotification(ui = "Unequal vector length - data loss", type = "warning", duration = 3, closeButton = TRUE)
    rvals$data_warn <- TRUE
    df_check <- df_rfe[rowSums(is.na(df_rfe)) != ncol(df_rfe), ]
    rvals$data_warn_n <- sum(!complete.cases(df_check))
  }
  obs_groups <- df_rfe %>%
    group_by_at(input$dependentVar_c_fi) %>%
    count()
  if (any(obs_groups$n == 1)) {
    showNotification(ui = "Too few observations per group", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(any(obs_groups$n == 1)), "Too few observations in subgroups to perform splitting for training/testing. Please choose another dependent variable or perform random forest instead.")
    )
  }
  df_rfe <- df_rfe[complete.cases(df_rfe), ]
  df_rfe_var <- df_rfe %>% select(-input$dependentVar_c_fi)
  df_rfe_var <- df_rfe_var %>% mutate_if(is.numeric, scale)
  df_rfe <- df_rfe %>% mutate(group = factor(df_rfe[[input$dependentVar_c_fi]]))
  df_rfe <- cbind(df_rfe_var, df_rfe$group)
  df_rfe <- df_rfe %>% rename(group = "df_rfe$group")
  df_rfe
})

# classifcation RFE data splits
y_c <- reactive({
  y_c <- rfe_c_data()
  y_c$group
})

x_c <- reactive({
  x_c_rfe <- rfe_c_data()
  x_c <- x_c_rfe[, names(x_c_rfe) != "group"]
  x_c
})

dataTrain_c_rfe <- reactive({
  caret::createDataPartition(y_c(), p = .80, list = FALSE)[, 1]
})

x_train_c_rfe <- reactive({
  x_c()[dataTrain_c_rfe(), ]
})

x_test_c_rfe <- reactive({
  x_c()[-dataTrain_c_rfe(), ]
})

y_train_c_rfe <- reactive({
  y_c()[dataTrain_c_rfe()]
})

y_test_c_rfe <- reactive({
  y_c()[-dataTrain_c_rfe()]
})

rfe_c_test <- reactive({
  postResample(predict(df_c_fi(), x_test_c_rfe()), y_test_c_rfe())
})

# download classification feature importance plot
output$download_c_fiplot <- downloadHandler(
  filename = function() {
    if (input$select_c_fimethod == "random forest") {
      paste("RandomForest_Classification_FeatureImportancePlot", ".png", sep = "")
    } else if (input$select_c_fimethod == "recursive feature elimination") {
      paste("RFE_Classification_FeatureImportancePlot", ".png", sep = "")
    }
  },
  content = function(file) {
    withProgress(
      message = paste0("Saving Plot"),
      value = 0,
      {
        shiny::incProgress(1 / 10)
        Sys.sleep(1)
        workingdir <- getwd()
        setwd(tempdir())
        if (input$select_c_fimethod == "random forest") {
          name <- paste("RandomForest_Classification_FeatureImportancePlot", ".png", sep = "")
        } else if (input$select_c_fimethod == "recursive feature elimination") {
          name <- paste("RFE_Classification_FeatureImportancePlot", ".png", sep = "")
        }
        shiny::incProgress(5 / 10)
        file.copy(name, file, overwrite = TRUE)
        setwd(workingdir)
      }
    )
  }
)

# download report for classification feature importance
output$report_c_fi <- downloadHandler(
  filename = "report_featureimportance.html",
  content = function(file) {
    withProgress(
      message = paste0("Generating Report"),
      value = 0,
      {
        shiny::incProgress(1 / 5)
        src_img <- file.path(getwd(), "www/tRigon_logo.png")
        file.copy(src_img, "tRigon_logo.png")
        tempReport <- file.path(getwd(), "report_fi.Rmd")
        file.copy("report_fi.Rmd", tempReport, overwrite = FALSE)
        shiny::incProgress(2 / 5)
        # Set up parameters to pass to Rmd document
        params <- list(
          session_info = sessioninfo::session_info()$platform,
          feature_vars = input$feature_c_fi_variableSelect,
          dependent_var = input$dependentVar_c_fi,
          groups = rvals$groups_lvl,
          warning_data = rvals$data_warn,
          warning_data_n = rvals$data_warn_n,
          fi_method = input$fi_input,
          fi_model = input$select_c_fimethod,
          folds_n = input$c_n_folds,
          repeats_n = input$c_n_repeats,
          fi_output = rvals$rval_fi_output,
          fi_plot = rvals$rval_fi_plot,
          na_omit = rvals$na_omit,
          na_n = rvals$n_na
        )
        shiny::incProgress(3 / 5)
        rmarkdown::render(tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
        shiny::incProgress(4 / 5)
      }
    )
  }
)

# regression fi
df_r_fi <- eventReactive(input$compute_r_FiBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (input$dependentVar_r_fi == "") {
    showNotification(ui = "Please specify a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_r_fi != "", "Please specify a group variable")
    )
  }
  if (!(input$dependentVar_r_fi %in% names(rvals$rval_loadeddf) | input$dependentVar_r_fi %in% names(rvals$rval_processeddf))) {
    showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_r_fi %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
      need(input$dependentVar_r_fi %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
    )
  }
  if (length(input$feature_r_fi_variableSelect) < 4) {
    showNotification(ui = "Please select min. 4 features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$feature_r_fi_variableSelect > 3, paste("Please select at least 4 features for ", input$select_r_fimethod, sep = " ")),
    )
  }
  if (input$dependentVar_r_fi %in% input$feature_r_fi_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$dependentVar_r_fi %in% input$feature_r_fi_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  } else {
    calc_r_fi()
  }
})

# regression calculate feature importance object
calc_r_fi <- eventReactive(input$compute_r_FiBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (input$dependentVar_r_fi == "") {
    showNotification(ui = "Please specify a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_r_fi != "", "Please specify a group variable")
    )
  }
  if (!(input$dependentVar_r_fi %in% names(rvals$rval_loadeddf) | input$dependentVar_r_fi %in% names(rvals$rval_processeddf))) {
    showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_r_fi %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
      need(input$dependentVar_r_fi %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
    )
  }
  if (length(input$feature_r_fi_variableSelect) < 4) {
    showNotification(ui = "Please select min. 4 features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$feature_r_fi_variableSelect > 3, paste("Please select at least 4 features for ", input$select_r_fimethod, sep = "")),
    )
  }
  if (input$dependentVar_r_fi %in% input$feature_r_fi_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$dependentVar_r_fi %in% input$feature_r_fi_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  } else {
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
    group_col <- input$dependentVar_r_fi
    features <- input$feature_r_fi_variableSelect
    g <- df_var[[group_col]]
    if (NA %in% g) {
      rvals$na_omit <- TRUE
      df_na <- df_var[which(is.na(df_var[[group_col]])), ]
      df_na <- as.data.frame(subset(df_na, select = c(features)))
      rvals$n_na <- count(df_na[complete.cases(df_na), ])
      g <- na.omit(g)
      df_var <- df_var[!is.na(df_var[[group_col]]), ]
    } else {
      rvals$na_omit <- FALSE
      rvals$n_na <- 0
    }
    rvals$groups_lvl <- unique(df_var[[group_col]])
    if (!(is.numeric(df_var[[input$dependentVar_r_fi]]))) {
      showNotification(ui = "Non-numeric dependent variable", type = "error", duration = NULL, closeButton = TRUE)
      validate(
        need(is.numeric(df_var[[input$dependentVar_r_fi]]), "Dependant variable is non-numeric. Did you mean to do classification?")
      )
    }
    if (input$select_r_fimethod == "random forest") {
      withProgress(
        message = paste0("Growing Trees"),
        value = 0,
        {
          incProgress(1 / 5)
          features <- input$feature_r_fi_variableSelect
          df_rf <- as.data.frame(subset(df_var, select = c(input$dependentVar_r_fi, features)))
          df_check <- df_rf %>% select(-input$dependentVar_r_fi)
          var_types <- sapply(df_check, is.numeric)
          if (FALSE %in% var_types) {
            showNotification(ui = "Please select only numeric inputs", type = "error", duration = NULL, closeButton = TRUE)
            nonnum_var <- names(which(FALSE == var_types))
            validate(
              need(!(FALSE %in% var_types), paste0("non-numeric feature selected: ", nonnum_var, sep = ""))
            )
          }
          rvals$data_warn <- FALSE
          rvals$data_warn_n <- 0
          df_check <- colSums(!is.na(df_check))
          if (length(unique(df_check)) != 1) {
            showNotification(ui = "Unequal vector length - data loss", type = "warning", duration = 3, closeButton = TRUE)
            rvals$data_warn <- TRUE
            df_check <- df_rf[rowSums(is.na(df_rf)) != ncol(df_rf), ]
            rvals$data_warn_n <- sum(!complete.cases(df_check))
          }
          incProgress(2 / 5)
          df_rf <- df_rf[complete.cases(df_rf), ]
          df_scale <- df_rf %>% select(-input$dependentVar_r_fi)
          df_scale <- df_scale %>% mutate_if(is.numeric, scale)
          df_rf <- cbind(df_rf[[input$dependentVar_r_fi]], df_scale)
          incProgress(3 / 5)
          colnames(df_rf)[1] <- input$dependentVar_r_fi
          incProgress(4 / 5)
          y <- df_rf[, 1]
          df_rf <- df_rf %>% select(-input$dependentVar_r_fi)
          fi <- randomForest(y ~ ., data = df_rf, importance = TRUE, na.action = na.exclude)
        }
      )
    } else if (input$select_r_fimethod == "recursive feature elimination") {
      rfe_r_cont <- rfeControl(
        functions = rfFuncs,
        method = "repeatedcv",
        number = input$r_n_folds,
        repeats = input$r_n_repeats
      )
      n_r_fi <- length(input$feature_r_fi_variableSelect)
      withProgress(
        message = paste0("Computing RFE"),
        value = 0,
        {
          incProgress(1 / 3)
          fi <- caret::rfe(x = x_train_r_rfe(), y = y_train_r_rfe(), size = n_r_fi, rfeControl = rfe_r_cont)
        }
      )
    }
    fi
  }
})

# regression create feature importance plot object
output_r_fi_plot <- eventReactive(input$compute_r_FiBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (input$dependentVar_r_fi == "") {
    showNotification(ui = "Please specify a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_r_fi != "", "Please specify a group variable")
    )
  }
  if (!(input$dependentVar_r_fi %in% names(rvals$rval_loadeddf) | input$dependentVar_r_fi %in% names(rvals$rval_processeddf))) {
    showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_r_fi %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
      need(input$dependentVar_r_fi %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
    )
  }
  if (length(input$feature_r_fi_variableSelect) < 4) {
    showNotification(ui = "Please select min. 4 features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$feature_r_fi_variableSelect > 3, paste("Please select at least 4 features for", input$select_r_fimethod, sep = "")),
    )
  }
  if (input$dependentVar_r_fi %in% input$feature_r_fi_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$dependentVar_r_fi %in% input$feature_r_fi_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  } else {
    obj_fi <- df_r_fi()
    if (input$select_r_fimethod == "random forest") {
      withProgress(
        message = paste0("Rendering Plot"),
        value = 0,
        {
          incProgress(1 / 2)
          V <- as.data.frame(obj_fi$importance)
          V <- V %>% rename(IncMSE = "%IncMSE")
          rf_plot_MSE <- ggplot2::ggplot(V, aes(y = reorder(rownames(V), IncMSE), x = IncMSE)) +
            geom_point(color = "black", size = 4, alpha = 1) +
            geom_segment(aes(y = rownames(V), yend = rownames(V), x = 0, xend = IncMSE), color = "black") +
            ylab("feature") +
            xlab("Mean Square Error (%inc)") +
            theme_bw() +
            theme(legend.position = "none") +
            theme(axis.text = element_text(size = 7, family = "sans")) +
            theme(axis.title = element_text(size = 10, family = "sans")) +
            theme(plot.title = element_text(size = 12, family = "sans")) +
            theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
            theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
          rf_plot_MSE <- rf_plot_MSE + geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey")
          rf_plot_NodePurity <- ggplot2::ggplot(V, aes(y = reorder(rownames(V), IncNodePurity), x = IncNodePurity)) +
            geom_point(color = "black", size = 4, alpha = 1) +
            geom_segment(aes(y = rownames(V), yend = rownames(V), x = 0, xend = IncNodePurity), color = "black") +
            ylab("feature") +
            xlab("Increase Node Purity") +
            theme_bw() +
            theme(legend.position = "none") +
            theme(axis.text = element_text(size = 7, family = "sans")) +
            theme(axis.title = element_text(size = 10, family = "sans")) +
            theme(plot.title = element_text(size = 12, family = "sans")) +
            theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
            theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
          rf_plot_NodePurity <- rf_plot_NodePurity + geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey")
          rf_plots <- (rf_plot_MSE + rf_plot_NodePurity)
          name <- paste("RandomForest_Regression_FeatureImportancePlot", ".png", sep = "")
          ggsave(name, rf_plots, path = tempdir())
        }
      )
    } else if (input$select_r_fimethod == "recursive feature elimination") {
      withProgress(
        message = paste0("Rendering Plot"),
        value = 0,
        {
          incProgress(1 / 2)
          varimp_data <- data.frame(
            feature = row.names(varImp(obj_fi))[1:length(predictors(obj_fi))],
            obj_fi[["fit"]][["importance"]]
          )
          varimp_data <- varimp_data %>% rename(IncMSE = "X.IncMSE")
          rf_plot_MSE <- ggplot(data = varimp_data, aes(y = reorder(rownames(varimp_data), IncMSE), x = IncMSE)) +
            geom_point(color = "black", size = 4, alpha = 1) +
            geom_segment(aes(y = feature, yend = feature, x = 0, xend = IncMSE), color = "black") +
            labs(y = "features", x = "Mean Square Error (%inc)") +
            geom_text(aes(label = round(IncMSE, 2)), vjust = 1.6, color = "white", size = 4) +
            theme_bw() +
            theme(legend.position = "none") +
            theme(axis.text = element_text(size = 7, family = "sans")) +
            theme(axis.title = element_text(size = 10, family = "sans")) +
            theme(plot.title = element_text(size = 12, family = "sans")) +
            theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
            theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
          rf_plot_MSE <- rf_plot_MSE + geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey")
          rf_plot_NodePurity <- ggplot(data = varimp_data, aes(y = reorder(rownames(varimp_data), IncNodePurity), x = IncNodePurity)) +
            geom_point(color = "black", size = 4, alpha = 1) +
            geom_segment(aes(y = feature, yend = feature, x = 0, xend = IncNodePurity), color = "black") +
            labs(y = "features", x = "Increase Node Purity") +
            geom_text(aes(label = round(IncNodePurity, 2)), vjust = 1.6, color = "white", size = 4) +
            theme_bw() +
            theme(legend.position = "none") +
            theme(axis.text = element_text(size = 7, family = "sans")) +
            theme(axis.title = element_text(size = 10, family = "sans")) +
            theme(plot.title = element_text(size = 12, family = "sans")) +
            theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
            theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
          rf_plot_NodePurity <- rf_plot_NodePurity + geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey")
          rf_plots <- (rf_plot_MSE + rf_plot_NodePurity)
          name <- paste("RFE_Regression_FeatureImportancePlot", ".png", sep = "")
          ggsave(name, rf_plots, path = tempdir())
        }
      )
    }
    rvals$rval_fi_plot <- rf_plots
    rf_plots
  }
})

# write regression feature importance output
output_r_fi <- eventReactive(input$compute_r_FiBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (input$dependentVar_r_fi == "") {
    showNotification(ui = "Please specify a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_r_fi != "", "Please specify a group variable")
    )
  }
  if (!(input$dependentVar_r_fi %in% names(rvals$rval_loadeddf) | input$dependentVar_r_fi %in% names(rvals$rval_processeddf))) {
    showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$dependentVar_r_fi %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
      need(input$dependentVar_r_fi %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
    )
  }
  if (length(input$feature_r_fi_variableSelect) < 4) {
    showNotification(ui = "Please select min. 4 features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$feature_r_fi_variableSelect > 3, paste("Please select at least 4 features for", input$select_r_fimethod, sep = "")),
    )
  }
  if (input$dependentVar_r_fi %in% input$feature_r_fi_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$dependentVar_r_fi %in% input$feature_r_fi_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  } else {
    if (input$select_r_fimethod == "random forest") {
      a <- df_r_fi()
      b <- importance(a)
      out_fi <- list(a, b)
    } else if (input$select_r_fimethod == "recursive feature elimination") {
      a <- df_r_fi()
      b <- a[["fit"]][["importance"]]
      c <- rfe_r_test()
      out_fi <- list(a, b, c)
    }
    rvals$rval_fi_output <- out_fi
    out_fi
  }
})

# regression rfe
rfe_r_data <- reactive({
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
  features <- input$feature_r_fi_variableSelect
  df_rfe <- as.data.frame(subset(df_var, select = c(input$dependentVar_r_fi, features)))
  df_check <- df_rfe %>% select(-input$dependentVar_r_fi)
  var_types <- sapply(df_check, is.numeric)
  if (FALSE %in% var_types) {
    showNotification(ui = "Please select only numeric inputs", type = "error", duration = NULL, closeButton = TRUE)
    nonnum_var <- names(which(FALSE == var_types))
    validate(
      need(!(FALSE %in% var_types), paste0("non-numeric feature selected: ", nonnum_var, sep = ""))
    )
  }
  rvals$data_warn <- FALSE
  rvals$data_warn_n <- 0
  df_check <- colSums(!is.na(df_check))
  if (length(unique(df_check)) != 1) {
    showNotification(ui = "Unequal vector length - data loss", type = "warning", duration = 3, closeButton = TRUE)
    rvals$data_warn <- TRUE
    df_check <- df_rfe[rowSums(is.na(df_rfe)) != ncol(df_rfe), ]
    rvals$data_warn_n <- sum(!complete.cases(df_check))
  }
  df_rfe <- df_rfe[complete.cases(df_rfe), ]
  df_scale <- df_rfe[, -c(1)]
  df_scale <- df_scale %>% mutate_if(is.numeric, scale)
  df_rfe <- cbind(df_rfe[, c(1)], df_scale)
  colnames(df_rfe)[1] <- input$dependentVar_r_fi
  df_rfe
})

# regression RFE data splits
y_r <- reactive({
  y_r <- rfe_r_data()
  y_r[, c(1)]
})

x_r <- reactive({
  x_r_rfe <- rfe_r_data()
  x_r <- x_r_rfe %>% select(-input$dependentVar_r_fi)
  x_r
})

dataTrain_r_rfe <- reactive({
  createDataPartition(y_r(), p = .80, list = FALSE)[, 1]
})

x_train_r_rfe <- reactive({
  x_r()[dataTrain_r_rfe(), ]
})

x_test_r_rfe <- reactive({
  x_r()[-dataTrain_r_rfe(), ]
})

y_train_r_rfe <- reactive({
  y_r()[dataTrain_r_rfe()]
})

y_test_r_rfe <- reactive({
  y_r()[-dataTrain_r_rfe()]
})

rfe_r_test <- reactive({
  postResample(predict(df_r_fi(), x_test_r_rfe()), y_test_r_rfe())
})

# download regression feature importance plot
output$download_r_fiplot <- downloadHandler(
  filename = function() {
    if (input$select_r_fimethod == "random forest") {
      paste("RandomForest_Regression_FeatureImportancePlot", ".png", sep = "")
    } else if (input$select_r_fimethod == "recursive feature elimination") {
      paste("RFE_Regression_FeatureImportancePlot", ".png", sep = "")
    }
  },
  content = function(file) {
    withProgress(
      message = paste0("Saving Plot"),
      value = 0,
      {
        shiny::incProgress(1 / 10)
        Sys.sleep(1)
        workingdir <- getwd()
        setwd(tempdir())
        if (input$select_r_fimethod == "random forest") {
          name <- paste("RandomForest_Regression_FeatureImportancePlot", ".png", sep = "")
        } else if (input$select_r_fimethod == "recursive feature elimination") {
          name <- paste("RFE_Regression_FeatureImportancePlot", ".png", sep = "")
        }
        shiny::incProgress(5 / 10)
        file.copy(name, file, overwrite = TRUE)
        setwd(workingdir)
      }
    )
  }
)

# download report for regression feature importance
output$report_r_fi <- downloadHandler(
  filename = "report_featureimportance.html",
  content = function(file) {
    withProgress(
      message = paste0("Generating Report"),
      value = 0,
      {
        shiny::incProgress(1 / 5)
        src_img <- file.path(getwd(), "www/tRigon_logo.png")
        file.copy(src_img, "tRigon_logo.png")
        tempReport <- file.path(getwd(), "report_fi.Rmd")
        file.copy("report_fi.Rmd", tempReport, overwrite = FALSE)
        shiny::incProgress(2 / 5)
        # Set up parameters to pass to Rmd document
        params <- list(
          session_info = sessioninfo::session_info()$platform,
          feature_vars = input$feature_c_fi_variableSelect,
          dependent_var = input$dependentVar_c_fi,
          groups = rvals$groups_lvl,
          warning_data = rvals$data_warn,
          warning_data_n = rvals$data_warn_n,
          fi_method = input$fi_input,
          fi_model = input$select_c_fimethod,
          folds_n = input$c_n_folds,
          repeats_n = input$c_n_repeats,
          fi_output = rvals$rval_fi_output,
          fi_plot = rvals$rval_fi_plot,
          na_omit = rvals$na_omit,
          na_n = rvals$n_na
        )
        shiny::incProgress(3 / 5)
        rmarkdown::render(tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
        shiny::incProgress(4 / 5)
      }
    )
  }
)
