# correlation

#perform simple correlation and create result df
scorr_data <- reactive({
  withProgress(
    message = paste0("Rendering Plot"),
    value = 0,
    {
      incProgress(1 / 2)
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
      if (input$input_scorr_subgroups) {
        group_name <- input$groupVar_scorr_name
        df_var <- subset(df_var, select = c(input$groupVar_scorr, input$feature_scorr_variableSelect))
        df_check <- df_var %>% select(-input$groupVar_scorr)
        var_types <- sapply(df_check, is.numeric)
        df_corr <- filter(df_var, df_var[, 1] == group_name)
        df_corr <- df_corr %>% dplyr::select(input$feature_scorr_variableSelect)
        if (FALSE %in% var_types) {
          showNotification(ui = "Please select only numeric inputs", type = "error", duration = NULL, closeButton = TRUE)
          nonnum_var <- names(which(FALSE == var_types))
          validate(
            need(!(FALSE %in% var_types), paste0("non-numeric feature selected: ", nonnum_var, sep=""))
          )
        }
        rvals$data_warn <- FALSE
        rvals$data_warn_n <- 0
        df_check <- colSums(!is.na(df_corr))
        if (df_check[1] != df_check[2]) {
          showNotification(ui = "Unequal vector length - data loss", type = "warning", duration = 3, closeButton = TRUE)
          rvals$data_warn <- TRUE
          df_check <- df_corr[rowSums(is.na(df_corr)) != ncol(df_corr), ]
          rvals$data_warn_n <- sum(!complete.cases(df_corr))
        }
        df_corr <- df_corr[complete.cases(df_corr), ]
        obs_groups <- df_corr %>% count()
        if (any(obs_groups$n < 2)) {
          showNotification(ui = "Too few observations per group", type = "error", duration = NULL, closeButton = TRUE)
          validate(
            need((any(obs_groups$n > 1)), "Too few observations in subgroups to perform correlation analysis.")
          )
        }
        cor_obj <- cor.test(df_corr[[input$feature_scorr_variableSelect[1]]], df_corr[[input$feature_scorr_variableSelect[2]]], na.rm=TRUE)
        df_corr$R <- cor_obj[["estimate"]][["cor"]]
        df_corr$R[duplicated(df_corr$R)] <- NA
        df_corr$p_value <- cor_obj[["p.value"]]
        df_corr$p_value[duplicated(df_corr$p_value)] <- NA
      } else {
        df_corr <- df_var %>% dplyr::select(input$feature_scorr_variableSelect)
        var_types <- sapply(df_corr, is.numeric)
        if (FALSE %in% var_types) {
          showNotification(ui = "Please select only numeric inputs", type = "error", duration = NULL, closeButton = TRUE)
          nonnum_var <- names(which(FALSE == var_types))
          validate(
            need(!(FALSE %in% var_types), paste0("non-numeric feature selected: ", nonnum_var, sep=""))
          )
        }
        rvals$data_warn <- FALSE
        rvals$data_warn_n <- 0
        df_check <- colSums(!is.na(df_corr))
        if (df_check[1] != df_check[2]) {
          showNotification(ui = "Unequal vector length - data loss", type = "warning", duration = 3, closeButton = TRUE)
          rvals$data_warn <- TRUE
          df_check <- df_corr[rowSums(is.na(df_corr)) != ncol(df_corr), ]
          rvals$data_warn_n <- sum(!complete.cases(df_corr))
        }
        df_corr <- df_corr[complete.cases(df_corr), ]
        obs_groups <- df_corr %>% count()
        if (any(obs_groups$n < 2)) {
          showNotification(ui = "Too few observations per group", type = "error", duration = NULL, closeButton = TRUE)
          validate(
            need((any(obs_groups$n > 1)), "Too few observations in subgroups to perform correlation analysis.")
          )
        }
        cor_obj <- cor.test(df_corr[[input$feature_scorr_variableSelect[1]]], df_corr[[input$feature_scorr_variableSelect[2]]], na.rm=TRUE)
        df_corr$R <- cor_obj[["estimate"]][["cor"]]
        df_corr$R[duplicated(df_corr$R)] <- NA
        df_corr$p_value <- cor_obj[["p.value"]]
        df_corr$p_value[duplicated(df_corr$p_value)] <- NA
      }
      rvals$rval_corrdf <- df_corr
    }
  )
})

# render simple correlation df
scorr_df <- eventReactive(input$s_correlationBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (is.null(input$feature_scorr_variableSelect)) {
    showNotification(ui = "Please select two features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(input$feature_scorr_variableSelect), "Please select two features for simple correlation analysis")
    )
  }
  if (input$groupVar_scorr == "" & input$input_scorr_subgroups == TRUE) {
    showNotification(ui = "Please select a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_scorr != "", "Please select a group variable")
    )
  }
  if (input$groupVar_scorr_name == "" & input$input_scorr_subgroups == TRUE) {
    showNotification(ui = "Please select a subgroup name", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_scorr_name != "", "Please select a subgroup name")
    )
  }
  if (length(input$feature_scorr_variableSelect) > 2) {
    showNotification(ui = "Please select less features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(length(input$feature_scorr_variableSelect) < 3, "Please select only two features for simple correlation - if you want to analyse more features consider multiple correlation")
    )
  }
  if (input$input_scorr_subgroups == TRUE) {
    if (!(input$groupVar_scorr %in% names(rvals$rval_loadeddf) | input$groupVar_scorr %in% names(rvals$rval_processeddf))) {
      showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
      validate(
        need(input$groupVar_scorr %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
        need(input$groupVar_scorr %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
      )
    }
    if (!(any(input$groupVar_scorr_name == rvals$rval_loadeddf[[input$groupVar_scorr]])) | any(input$groupVar_scorr_name == rvals$rval_processeddf[[input$groupVar_scorr]])) {
      showNotification(ui = "Subgroup name not found", type = "error", duration = NULL, closeButton = TRUE)
      validate(
        need(any(input$groupVar_scorr_name == rvals$rval_loadeddf[[input$groupVar_scorr]]), "Subgroup name not found in loaded dataframe"),
        need(any(input$groupVar_scorr_name == rvals$rval_processeddf[[input$groupVar_scorr]]), "Subgroup name not found in processed dataframe")
      )
    }
    if (input$groupVar_scorr %in% input$feature_scorr_variableSelect) {
      showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
      validate(
        need(!(input$groupVar_scorr %in% input$feature_scorr_variableSelect), "Group variable and feature variable cannot be the same."),
      )
    }
  }
  scorr_data()
})

# create simple correlation plot
scorr_plot <- eventReactive(input$s_correlationBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (is.null(input$feature_scorr_variableSelect)) {
    showNotification(ui = "Please select a feature", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(input$feature_scorr_variableSelect), "Please select a feature for correlation analysis")
    )
  }
  if (input$groupVar_scorr == "" & input$input_scorr_subgroups == TRUE) {
    showNotification(ui = "Please select a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_scorr != "", "Please select a group variable")
    )
  }
  if (input$groupVar_scorr_name == "" & input$input_scorr_subgroups == TRUE) {
    showNotification(ui = "Please select a subgroup name", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_scorr_name != "", "Please select a subgroup name")
    )
  }
  if (length(input$feature_scorr_variableSelect) < 2) {
    showNotification(ui = "Please select more features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(length(input$feature_scorr_variableSelect) > 1, "Please select more than one feature for correlation")
    )
  } else {
    df_corrplot <- scorr_df()
    feature_x <- colnames(df_corrplot)[1]
    feature_y <- colnames(df_corrplot)[2]
    if (df_corrplot[1, 3] > 0) {
      label_y <- (min(df_corrplot[, 2]) - (max(df_corrplot[, 2]) - min(df_corrplot[, 2]))*0.1)
    } else {
      label_y <- (max(df_corrplot[, 2]) + (max(df_corrplot[, 2]) - min(df_corrplot[, 2]))*0.1)
    }
    s_corrp <- ggplot(df_corrplot, aes_string(x = feature_x, y = feature_y)) +
      geom_point(color = "black") +
      geom_smooth(method = lm, se = TRUE) +
      theme_bw() +
      xlab(feature_x) + ylab (feature_y) +
      theme(legend.position = "none") +
      ggtitle("") +
      stat_cor(label.x.npc = "left", label.y = label_y, size= 5) +
      theme(axis.text = element_text(size = 9, family = "sans")) +
      theme(axis.title = element_text(size = 11, family = "sans")) +
      theme(plot.title = element_text(size = 12, family = "sans")) +
      theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
      theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
    name <- paste("simplecorrelation_plot.png", sep = "")
    ggsave(name, s_corrp, path = tempdir())
    rvals$rval_corrplot <- s_corrp
    s_corrp
  }
})

# output plot simple or multiple correlation
output$corr_plot <- renderPlot({
  if (input$corr_input == "Simple") {
    scorr_plot()
  } else if (input$corr_input == "Multiple") {
    mcorr_plot()
  }
})

# output df simple or multiple correlation
output$corr_table <- DT::renderDataTable({
  if (input$corr_input == "Simple") {
    scorr_df()
  } else if (input$corr_input == "Multiple") {
    mcorr_df()
  }
})

# download simple correlation data
output$download_s_corrdf <- downloadHandler(
  filename = function() {
    paste("simplecorrelation_data", ".xlsx", sep = "")
  },
  content = function(file) {
    withProgress(
      message = paste0("Saving Data"),
      value = 0,
      {
        shiny::incProgress(1 / 10)
        Sys.sleep(1)
        shiny::incProgress(5 / 10)
        download_statsdf_corr <- as.data.frame(cbind(" " = rownames(rvals$rval_corrdf), rvals$rval_corrdf))
        writexl::write_xlsx(download_statsdf_corr, file)
      }
    )
  }
)

# download simple correlation plot
output$download_s_corrplot <- downloadHandler(
  filename = function() {
    paste("simplecorrelation_plot.png", sep = "")
  },
  content = function(file) {
    withProgress(
      message = paste0("Saving Plot"),
      value = 0,
      {
        shiny::incProgress(1 / 10)
        Sys.sleep(1)
        workingdir = getwd()
        setwd(tempdir())
        name <- paste("simplecorrelation_plot.png", sep = "")
        shiny::incProgress(5 / 10)
        file.copy(name, file, overwrite = TRUE)
        setwd(workingdir)
      }
    )
  }
)

# download simple correlation report
output$report_s_corr <- downloadHandler(
  filename = "report_correlation.html",
  content = function(file) {
    withProgress(
      message = paste0("Generating Report"),
      value = 0,
      {
        shiny::incProgress(1 / 5)
        src_img <- file.path(getwd(), "www/tRigon_logo.png")
        file.copy(src_img, "tRigon_logo.png")
        tempReport <- file.path(getwd(), "report_corr.Rmd")
        file.copy("report_corr.Rmd", tempReport, overwrite = FALSE)
        shiny::incProgress(2 / 5)
        # Set up parameters to pass to Rmd document
        params <- list(session_info = devtools::session_info()$platform,
                       feature_vars = input$feature_scorr_variableSelect,
                       subgroups_en = input$input_scorr_subgroups,
                       corr_method = input$corr_input,
                       warning_data = rvals$data_warn,
                       warning_data_n = rvals$data_warn_n,
                       group_corr = input$groupVar_scorr,
                       subgroup_corr = input$groupVar_scorr_name,
                       corr_df = rvals$rval_corrdf,
                       corr_plot = rvals$rval_corrplot)
        shiny::incProgress(3 / 5)
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
        shiny::incProgress(4 / 5)
      }
    )
  }
)

# multiple correlation

#perform simple correlation and create result df
mcorr_data <- reactive({
  withProgress(
    message = paste0("Rendering Plot"),
    value = 0,
    {
      incProgress(1 / 2)
      df_loaded <- rvals$rval_loadeddf
      df_processed <- rvals$rval_processeddf
      if (!is.null(dim(df_processed))) {
        df_var <- df_processed
      }
      if (!is.null(dim(df_loaded))) {
        df_var <- df_loaded
      }
      if (input$input_mcorr_subgroups) {
        group_name <- input$groupVar_mcorr_name
        df_var <- subset(df_var, select = c(input$groupVar_mcorr, input$feature_mcorr_variableSelect))
        df_check <- df_var %>% select(-input$groupVar_mcorr)
        var_types <- sapply(df_check, is.numeric)
        if (FALSE %in% var_types) {
          showNotification(ui = "Please select only numeric inputs", type = "error", duration = NULL, closeButton = TRUE)
          nonnum_var <- names(which(FALSE == var_types))
          validate(
            need(!(FALSE %in% var_types), paste0("non-numeric feature selected: ", nonnum_var, sep=""))
          )
        }
        df_corr <- filter(df_var, df_var[, 1] == group_name)
        df_corr <- df_corr %>% dplyr::select(input$feature_mcorr_variableSelect)
        df_check <- colSums(!is.na(df_corr))
        rvals$data_warn <- FALSE
        rvals$data_warn_n <- 0
        if (length(unique(df_check)) != 1) {
          showNotification(ui = "Unequal vector length - data loss", type = "warning", duration = 3, closeButton = TRUE)
          rvals$data_warn <- TRUE
          df_check <- df_corr[rowSums(is.na(df_corr)) != ncol(df_corr), ]
          rvals$data_warn_n <- sum(!complete.cases(df_corr))
        }
        obs_groups <- df_corr %>% count()
        if (any(obs_groups$n < 2)) {
          showNotification(ui = "Too few observations per group", type = "error", duration = NULL, closeButton = TRUE)
          validate(
            need(!(any(obs_groups$n > 1)), "Too few observations in subgroups to perform correlation analysis.")
          )
        }
        corr_matrix <- cor(as.matrix(df_corr))
        corr_matrix <- round(corr_matrix$r, 2)
      } else {
        df_corr <- df_var %>% dplyr::select(input$feature_mcorr_variableSelect)
        var_types <- sapply(df_corr, is.numeric)
        if (FALSE %in% var_types) {
          showNotification(ui = "Please select only numeric inputs", type = "error", duration = NULL, closeButton = TRUE)
          nonnum_var <- names(which(FALSE == var_types))
          validate(
            need(!(FALSE %in% var_types), paste0("non-numeric feature selected: ", nonnum_var, sep=""))
          )
        }
        df_check <- colSums(!is.na(df_corr))
        rvals$data_warn <- FALSE
        rvals$data_warn_n <- 0
        if (length(unique(df_check)) != 1) {
          showNotification(ui = "Unequal vector length - data loss", type = "warning", duration = 3, closeButton = TRUE)
          rvals$data_warn <- TRUE
          df_check <- df_corr[rowSums(is.na(df_corr)) != ncol(df_corr), ]
          rvals$data_warn_n <- sum(!complete.cases(df_corr))
        }
        obs_groups <- df_corr %>% count()
        if (any(obs_groups$n < 2)) {
          showNotification(ui = "Too few observations per group", type = "error", duration = NULL, closeButton = TRUE)
          validate(
            need(!(any(obs_groups$n > 1)), "Too few observations in subgroups to perform correlation analysis.")
          )
        }
        corr_matrix <- cor(as.matrix(df_corr), use = "p", method = "pearson")
        corr_matrix <- round(corr_matrix, 2)
      }
      rvals$rval_corrdf <- corr_matrix
    }
  )
})

# render multiple correlation df
mcorr_df <- eventReactive(input$m_correlationBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (is.null(input$feature_mcorr_variableSelect)) {
    showNotification(ui = "Please select features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(input$feature_mcorr_variableSelect), "Please select features for correlation analysis")
    )
  }
  if (input$groupVar_mcorr == "" & input$input_mcorr_subgroups == TRUE) {
    showNotification(ui = "Please select a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_mcorr != "", "Please select a group variable")
    )
  }
  if (input$groupVar_mcorr_name == "" & input$input_mcorr_subgroups == TRUE) {
    showNotification(ui = "Please select a subgroup name", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_mcorr_name != "", "Please select a subgroup name")
    )
  }
  if (length(input$feature_mcorr_variableSelect) <= 2) {
    showNotification(ui = "Please select more features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(length(input$feature_mcorr_variableSelect) > 2, "Please select more than two feature for  multiple correlation - for two features consider simple correlation")
    )
  }
  if (input$input_mcorr_subgroups == TRUE) {
    if (!(input$groupVar_mcorr %in% names(rvals$rval_loadeddf) | input$groupVar_mcorr %in% names(rvals$rval_processeddf))) {
      showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
      validate(
        need(input$groupVar_mcorr %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
        need(input$groupVar_mcorr %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
      )
    }
    if (!(any(input$groupVar_mcorr_name == rvals$rval_loadeddf[[input$groupVar_mcorr]])) | any(input$groupVar_mcorr_name == rvals$rval_processeddf[[input$groupVar_mcorr]])) {
      showNotification(ui = "Subgroup name not found", type = "error", duration = NULL, closeButton = TRUE)
      validate(
        need(any(input$groupVar_mcorr_name == rvals$rval_loadeddf[[input$groupVar_mcorr]]), "Subgroup name not found in loaded dataframe"),
        need(any(input$groupVar_mcorr_name == rvals$rval_processeddf[[input$groupVar_mcorr]]), "Subgroup name not found in processed dataframe")
      )
    }
    if (input$groupVar_mcorr %in% input$feature_mcorr_variableSelect) {
      showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
      validate(
        need(!(input$groupVar_mcorr %in% input$feature_mcorr_variableSelect), "Group variable and feature variable cannot be the same."),
      )
    }
  }
  mcorr_data()
})

# create multiple correlation plot
mcorr_plot <- eventReactive(input$m_correlationBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (is.null(input$feature_mcorr_variableSelect)) {
    showNotification(ui = "Please select a feature", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(input$feature_mcorr_variableSelect), "Please select a feature for correlation analysis")
    )
  }
  if (input$groupVar_mcorr == "" & input$input_mcorr_subgroups == TRUE) {
    showNotification(ui = "Please select a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_mcorr != "", "Please select a group variable")
    )
  }
  if (input$groupVar_mcorr_name == "" & input$input_mcorr_subgroups == TRUE) {
    showNotification(ui = "Please select a subgroup name", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_mcorr_name != "", "Please select a subgroup name")
    )
  }
  if (length(input$feature_mcorr_variableSelect) < 2) {
    showNotification(ui = "Please select more features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(length(input$feature_mcorr_variableSelect) > 1, "Please select more than one feature for correlation")
    )
  } else {
    df_corrplot <- mcorr_df()
    n_features <- length(input$feature_mcorr_variableSelect)
    m_corrp <- ggcorrplot(df_corrplot, method = "square", type = "lower", lab = TRUE, outline.color = "white") +
      theme_bw() + theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x=element_text(angle = 45, hjust = 1),
        legend.position = "none")
    name <- paste("correlation_matrix", "_", length(input$feature_mcorr_variableSelect), "features", ".png", sep = "")
    ggsave(name, m_corrp, path = tempdir())
    rvals$rval_corrplot <- m_corrp
    m_corrp
  }
})

# download multiple correlation data
output$download_m_corrdf <- downloadHandler(
  filename = function() {
    paste("multiplecorrelation", "_", length(input$feature_mcorr_variableSelect), "features", ".xlsx", sep = "")
  },
  content = function(file) {
    withProgress(
      message = paste0("Saving Data"),
      value = 0,
      {
        shiny::incProgress(1 / 10)
        Sys.sleep(1)
        shiny::incProgress(5 / 10)
        download_statsdf_corr <- as.data.frame(cbind(" " = rownames(rvals$rval_corrdf), rvals$rval_corrdf))
        writexl::write_xlsx(download_statsdf_corr, file)
      }
    )
  }
)

# download multiple correlation plot
output$download_m_corrplot <- downloadHandler(
  filename = function() {
    paste("correlation_matrix", "_", length(input$feature_mcorr_variableSelect), "features", ".png", sep = "")
  },
  content = function(file) {
    withProgress(
      message = paste0("Saving Plot"),
      value = 0,
      {
        shiny::incProgress(1 / 10)
        Sys.sleep(1)
        workingdir = getwd()
        setwd(tempdir())
        name <- paste("correlation_matrix", "_", length(input$feature_mcorr_variableSelect), "features", ".png", sep = "")
        shiny::incProgress(5 / 10)
        file.copy(name, file, overwrite = TRUE)
        setwd(workingdir)
      }
    )
  }
)

# download multiple correlation report
output$report_m_corr <- downloadHandler(
  filename = "report_correlation.html",
  content = function(file) {
    withProgress(
      message = paste0("Generating Report"),
      value = 0,
      {
        shiny::incProgress(1 / 5)
        src_img <- file.path(getwd(), "www/tRigon_logo.png")
        file.copy(src_img, "tRigon_logo.png")
        tempReport <- file.path(getwd(), "report_corr.Rmd")
        file.copy("report_corr.Rmd", tempReport, overwrite = FALSE)
        shiny::incProgress(2 / 5)
        # Set up parameters to pass to Rmd document
        params <- list(session_info = devtools::session_info()$platform,
                       feature_vars = input$feature_mcorr_variableSelect,
                       subgroups_en = input$input_mcorr_subgroups,
                       corr_method = input$corr_input,
                       warning_data = rvals$data_warn,
                       warning_data_n = rvals$data_warn_n,
                       group_corr = input$groupVar_mcorr,
                       subgroup_corr = input$groupVar_mcorr_name,
                       corr_df = rvals$rval_corrdf,
                       corr_plot = rvals$rval_corrplot)
        shiny::incProgress(3 / 5)
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
        shiny::incProgress(4 / 5)
      }
    )
  }
)
