# plots

#create input data and choose plot
plot_data <- reactive({
  withProgress(message = "Plotting", value = 0, {
    incProgress(1 / 6)
    logscale <- input$logscale_switch
    var <- c(input$feature_p_variableSelect, input$groupVar_p)
    feature <- c(input$feature_p_variableSelect)
    group_col <- input$groupVar_p
    plot <- input$p_PlotSelect
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
    if (is.numeric(df_var[[input$groupVar_p]])) {
      showNotification(ui = "Warning: group var is numeric", type = "warning", duration = 5, closeButton = TRUE)
    }
    if (length(unique(df_var[[input$groupVar_p]])) >= 20) {
      showNotification(ui = "Please select fewer groups", type = "error", duration = NULL, closeButton = TRUE)
      validate(
        need(length(unique(df_var[[input$groupVar_p]])) < 20, paste0(length(unique(df_var[[input$groupVar_p]])), " groups reported. Cannot process more than 20 unique groups."))
      )
    }
    obs_groups <- df_var %>%
      group_by_at(group_col) %>% count()
    if (any(obs_groups$n == 1)) {
      showNotification(ui = "Warning: only one observation in group", type = "warning", duration = 3, closeButton = TRUE)
    }
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
    rvals$groups_lvl <- unique(df_var[[group_col]])
    if (logscale) {
      if (plot == "Violin plot") {
        df_var <- subset(df_var, df_var[[feature]] > 0)
        n_groups <- length(unique(df_var[[group_col]]))
        if (n_groups > 12) {
          group_colors = sample(col_vector, n_groups)
        } else {
          group_colors = my_palette
        }
        df_var[[group_col]] <- as.factor(df_var[[group_col]])
        n <- nrow(df_var[!is.na(df_var[[feature]]), ])
        x <- df_var %>%
          filter(!is.na(df_var[[feature]])) %>%
          group_by_at(group_col) %>%
          count()
        label <- paste0(x[[group_col]], "\n n = ", x$n)
        p <- ggplot(df_var, aes_string(x = group_col, y = feature, fill = group_col)) +
          geom_violin(adjust = 2, trim = FALSE) +
          theme_bw() +
          scale_y_log10() +
          ylab(feature) +
          theme(legend.position = "none") +
          ggtitle("") +
          scale_fill_manual(values = group_colors) +
          scale_x_discrete(group_col, labels = c(label)) +
          theme(axis.text = element_text(size = 9, family = "sans")) +
          theme(axis.title = element_text(size = 11, family = "sans")) +
          theme(plot.title = element_text(size = 12, family = "sans")) +
          theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
          theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
      }
      incProgress(2 / 6)
      if (plot == "Boxplot") {
        df_var <- subset(df_var, df_var[[feature]] > 0)
        n_groups <- length(unique(df_var[[group_col]]))
        if (n_groups > 12) {
          group_colors = sample(col_vector, n_groups)
        } else {
          group_colors = my_palette
        }
        df_var[[group_col]] <- as.factor(df_var[[group_col]])
        n <- nrow(df_var[!is.na(df_var[[feature]]), ])
        x <- df_var %>%
          filter(!is.na(df_var[[feature]])) %>%
          group_by_at(group_col) %>%
          count()
        label <- paste0(x[[group_col]], "\n n = ", x$n)
        p <- ggplot(df_var, aes_string(x = group_col, y = feature, fill = group_col)) +
          geom_boxplot(width = 0.4, color = "black") +
          theme_bw() +
          scale_y_log10() +
          ylab(feature) +
          theme(legend.position = "none") +
          ggtitle("") +
          scale_fill_manual(values = group_colors) +
          scale_x_discrete(group_col, labels = c(label)) +
          theme(axis.text = element_text(size = 9, family = "sans")) +
          theme(axis.title = element_text(size = 11, family = "sans")) +
          theme(plot.title = element_text(size = 12, family = "sans")) +
          theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
          theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
      }
      incProgress(3 / 6)
      if (plot == "Violin with Boxplot") {
        df_var <- subset(df_var, df_var[[feature]] > 0)
        n_groups <- length(unique(df_var[[group_col]]))
        if (n_groups > 12) {
          group_colors = sample(col_vector, n_groups)
        } else {
          group_colors = my_palette
        }
        df_var[[group_col]] <- as.factor(df_var[[group_col]])
        n <- nrow(df_var[!is.na(df_var[[feature]]), ])
        x <- df_var %>%
          filter(!is.na(df_var[[feature]])) %>%
          group_by_at(group_col) %>%
          count()
        label <- paste0(x[[group_col]], "\n n = ", x$n)
        p <- ggplot(df_var, aes_string(x = group_col, y = feature, fill = group_col)) +
          geom_violin(adjust = 2, trim = FALSE) +
          geom_boxplot(width = 0.1, color = "black") +
          theme_bw() +
          scale_y_log10() +
          ylab(feature) +
          theme(legend.position = "none") +
          ggtitle("") +
          scale_fill_manual(values = group_colors) +
          scale_x_discrete(group_col, labels = c(label)) +
          theme(axis.text = element_text(size = 9, family = "sans")) +
          theme(axis.title = element_text(size = 11, family = "sans")) +
          theme(plot.title = element_text(size = 12, family = "sans")) +
          theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
          theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
      }
      incProgress(4 / 6)
      if (plot == "Ridgeline") {
        df_var <- subset(df_var, df_var[[feature]] > 0)
        n_groups <- length(unique(df_var[[group_col]]))
        if (n_groups > 12) {
          group_colors = sample(col_vector, n_groups)
        } else {
          group_colors = my_palette
        }
        df_var[[group_col]] <- as.factor(df_var[[group_col]])
        n <- nrow(df_var[!is.na(df_var[[feature]]), ])
        x <- df_var %>%
          filter(!is.na(df_var[[feature]])) %>%
          group_by_at(group_col) %>%
          count()
        label <- paste0(x[[group_col]], "\n n = ", x$n)
        p <- ggplot(df_var, aes_string(x = feature, y = group_col, fill = group_col)) +
          geom_density_ridges() +
          theme_ridges() +
          theme_bw() +
          scale_x_log10() +
          xlab(feature) +
          theme(legend.position = "none") +
          ggtitle("") +
          scale_fill_manual(values = group_colors) +
          scale_y_discrete(group_col, labels = c(label)) +
          theme(axis.text = element_text(size = 9, family = "sans")) +
          theme(axis.title = element_text(size = 11, family = "sans")) +
          theme(plot.title = element_text(size = 12, family = "sans")) +
          theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
          theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
      }
      incProgress(5 / 6)
    } else {
      if (plot == "Violin plot") {
        n_groups <- length(unique(df_var[[group_col]]))
        if (n_groups > 12) {
          group_colors = sample(col_vector, n_groups)
        } else {
          group_colors = my_palette
        }
        df_var[[group_col]] <- as.factor(df_var[[group_col]])
        n <- nrow(df_var[!is.na(df_var[[feature]]), ])
        x <- df_var %>%
          filter(!is.na(df_var[[feature]])) %>%
          group_by_at(group_col) %>%
          count()
        label <- paste0(x[[group_col]], "\n n = ", x$n)
        p <- ggplot(df_var, aes_string(x = group_col, y = feature, fill = group_col)) +
          geom_violin(adjust = 2, trim = FALSE) +
          theme_bw() +
          ylab(feature) +
          theme(legend.position = "none") +
          ggtitle("") +
          scale_fill_manual(values = group_colors) +
          scale_x_discrete(group_col, labels = c(label)) +
          theme(axis.text = element_text(size = 9, family = "sans")) +
          theme(axis.title = element_text(size = 11, family = "sans")) +
          theme(plot.title = element_text(size = 12, family = "sans")) +
          theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
          theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
      }
      incProgress(2 / 6)
      if (plot == "Boxplot") {
        n_groups <- length(unique(df_var[[group_col]]))
        if (n_groups > 12) {
          group_colors = sample(col_vector, n_groups)
        } else {
          group_colors = my_palette
        }
        df_var[[group_col]] <- as.factor(df_var[[group_col]])
        n <- nrow(df_var[!is.na(df_var[[feature]]), ])
        x <- df_var %>%
          filter(!is.na(df_var[[feature]])) %>%
          group_by_at(group_col) %>%
          count()
        label <- paste0(x[[group_col]], "\n n = ", x$n)
        p <- ggplot(df_var, aes_string(x = group_col, y = feature, fill = group_col)) +
          geom_boxplot(width = 0.4, color = "black") +
          theme_bw() +
          ylab(feature) +
          theme(legend.position = "none") +
          ggtitle("") +
          scale_fill_manual(values = group_colors) +
          scale_x_discrete(group_col, labels = c(label)) +
          theme(axis.text = element_text(size = 9, family = "sans")) +
          theme(axis.title = element_text(size = 11, family = "sans")) +
          theme(plot.title = element_text(size = 12, family = "sans")) +
          theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
          theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
      }
      incProgress(3 / 6)
      if (plot == "Violin with Boxplot") {
        n_groups <- length(unique(df_var[[group_col]]))
        if (n_groups > 12) {
          group_colors = sample(col_vector, n_groups)
        } else {
          group_colors = my_palette
        }
        df_var[[group_col]] <- as.factor(df_var[[group_col]])
        n <- nrow(df_var[!is.na(df_var[[feature]]), ])
        x <- df_var %>%
          filter(!is.na(df_var[[feature]])) %>%
          group_by_at(group_col) %>%
          count()
        label <- paste0(x[[group_col]], "\n n = ", x$n)
        p <- ggplot(df_var, aes_string(x = group_col, y = feature, fill = group_col)) +
          geom_violin(adjust = 2, trim = FALSE) +
          geom_boxplot(width = 0.1, color = "black") +
          theme_bw() +
          ylab(feature) +
          theme(legend.position = "none") +
          ggtitle("") +
          scale_fill_manual(values = group_colors) +
          scale_x_discrete(group_col, labels = c(label)) +
          theme(axis.text = element_text(size = 9, family = "sans")) +
          theme(axis.title = element_text(size = 11, family = "sans")) +
          theme(plot.title = element_text(size = 12, family = "sans")) +
          theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
          theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
      }
      incProgress(4 / 6)
      if (plot == "Ridgeline") {
        n_groups <- length(unique(df_var[[group_col]]))
        if (n_groups > 12) {
          group_colors = sample(col_vector, n_groups)
        } else {
          group_colors = my_palette
        }
        df_var[[group_col]] <- as.factor(df_var[[group_col]])
        n <- nrow(df_var[!is.na(df_var[[feature]]), ])
        x <- df_var %>%
          filter(!is.na(df_var[[feature]])) %>%
          group_by_at(group_col) %>%
          count()
        label <- paste0(x[[group_col]], "\n n = ", x$n)
        p <- ggplot(df_var, aes_string(x = feature, y = group_col, fill = group_col)) +
          geom_density_ridges() +
          theme_ridges() +
          theme_bw() +
          xlab(feature) +
          theme(legend.position = "none") +
          ggtitle("") +
          scale_fill_manual(values = group_colors) +
          scale_y_discrete(group_col, labels = c(label)) +
          theme(axis.text = element_text(size = 9, family = "sans")) +
          theme(axis.title = element_text(size = 11, family = "sans")) +
          theme(plot.title = element_text(size = 12, family = "sans")) +
          theme(panel.border = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.ticks = element_line(color = "black", size = 0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
          theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
      }
      incProgress(5 / 6)
    }
    rvals$rval_plot <- p
    p
  })
})

# render plot
output$Feature_Plot <- renderPlot({
  plot_df()
})

plot_df <- eventReactive(input$plotBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (is.null(input$feature_p_variableSelect)) {
    showNotification(ui = "Please select a feature", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(input$feature_p_variableSelect), "Please select a feature for analysis")
    )
  }
  if (input$feature_p_variableSelect == "... no features found") {
    showNotification(ui = "Please select a feature", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$feature_p_variableSelect != "... no features found", "Please select a feature for analysis")
    )
  }
  if (input$groupVar_p == "") {
    showNotification(ui = "Please specify a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_p != "", "Please specify a group variable")
    )
  }
  if (!(input$groupVar_p %in% names(rvals$rval_loadeddf) | input$groupVar_p %in% names(rvals$rval_processeddf))) {
    showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_p %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
      need(input$groupVar_p %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
    )
  }
  if (input$p_PlotSelect == "...") {
    showNotification(ui = "Please specify a plot type", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$p_PlotSelect != "...", "Please specify a plot type")
    )
  }
  if (input$groupVar_p %in% input$feature_p_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$groupVar_p %in% input$feature_p_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  } else {
    name <- paste(input$p_PlotSelect, "_", input$feature_p_variableSelect, ".png", sep = "")
    ggsave(name, plot_data(), path = tempdir())
    plot_data()
  }
})

# download plot
output$download_plot <- downloadHandler(
  filename = function() {
    paste(input$p_PlotSelect, "_", input$feature_p_variableSelect, ".png", sep = "")
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
        name <- paste(input$p_PlotSelect, "_", input$feature_p_variableSelect, ".png", sep = "")
        shiny::incProgress(5 / 10)
        file.copy(name, file, overwrite = TRUE)
        setwd(workingdir)
      }
    )
  }
)

# download report for plot
output$report_plot <- downloadHandler(
  filename = "report_plot.html",
  content = function(file) {
    withProgress(
      message = paste0("Generating Report"),
      value = 0,
      {
        shiny::incProgress(1 / 5)
        src_img <- file.path(getwd(), "www/tRigon_logo.png")
        file.copy(src_img, "tRigon_logo.png")
        tempReport <- file.path(getwd(), "report_plot.Rmd")
        file.copy("report_plot.Rmd", tempReport, overwrite = FALSE)
        shiny::incProgress(2 / 5)
        # Set up parameters to pass to Rmd document
        params <- list(session_info = sessioninfo::session_info()$platform,
                       feature_var = input$feature_p_variableSelect,
                       group_var = input$groupVar_p,
                       groups = rvals$groups_lvl,
                       plot_selection = input$p_PlotSelect,
                       scale = input$logscale_switch,
                       plot = rvals$rval_plot,
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
