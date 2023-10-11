# clustering

# perform k-means clustering
clustering <- shiny::reactive({
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
  if (input$cluster_groups_en) {
    group_col <- input$groupVar_cluster
    rvals$groups_lvl <- unique(df_var[[group_col]])
  }
  withProgress(
    message = paste0("Computing Clusters"),
    value = 0,
    {
      incProgress(1 / 3)
      if (input$cluster_groups_en) {
        df_cluster <- df_var %>% select(input$groupVar_cluster, input$feature_cluster_variableSelect)
        df_check <- df_cluster %>% select(-input$groupVar_cluster)
      } else {
        df_cluster <- df_var %>% select(input$feature_cluster_variableSelect)
        df_check <- df_cluster
      }
      var_types <- sapply(df_check, is.numeric)
      if (FALSE %in% var_types) {
        showNotification(ui = "Please select only numeric inputs", type = "error", duration = NULL, closeButton = TRUE)
        nonnum_var <- names(which(FALSE == var_types))
        validate(
          need(!(FALSE %in% var_types), paste0("non-numeric feature selected: ", nonnum_var, sep=""))
        )
      }
      rvals$data_warn <- FALSE
      rvals$data_warn_n <- 0
      df_check <- colSums(!is.na(df_check))
      if (length(unique(df_check)) != 1) {
        showNotification(ui = "Unequal vector length - data loss", type = "warning", duration = 3, closeButton = TRUE)
        rvals$data_warn <- TRUE
        df_check <- df_cluster[rowSums(is.na(df_cluster)) != ncol(df_cluster), ]
        rvals$data_warn_n <- sum(!complete.cases(df_check))
      }
      df_cluster <- df_cluster[complete.cases(df_cluster), ]
      if (input$cluster_groups_en) {
        if (is.numeric(df_cluster[[input$groupVar_cluster]])) {
          showNotification(ui = "Warning: group var is numeric", type = "warning", duration = 5, closeButton = TRUE)
        }
        if (length(unique(df_cluster[[input$groupVar_cluster]])) > input$cluster_n) {
          showNotification(ui = "Please select fewer groups", type = "error", duration = NULL, closeButton = TRUE)
          validate(
            need(length(unique(df_cluster[[input$groupVar_cluster]])) <= input$cluster_n, "Levels of group var cannot be larger than the specified n of clusters.")
          )
        }
      }
      if (input$cluster_groups_en) {
        df_cluster[[group_col]] <- as.character(df_cluster[[group_col]])
        data_cluster  <- scale(df_cluster %>% select(input$feature_cluster_variableSelect))
        data_cluster <- cbind(df_cluster[[group_col]], data_cluster)
      } else {
        data_cluster  <- scale(df_cluster)
      }
      rvals$rval_data_cluster <- data_cluster
      c <- kmeans(subset(data_cluster, select = input$feature_cluster_variableSelect), centers = input$cluster_n)
    }
  )
  rvals$rval_output_cluster <- c
  c
})

clustering_alg <- shiny::eventReactive(input$clusterBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (is.null(input$feature_cluster_variableSelect)) {
    showNotification(ui = "Please select features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(input$feature_cluster_variableSelect), "Please select features for k-means Clustering")
    )
  }
  if (input$groupVar_cluster == "" & input$cluster_groups_en == TRUE) {
    showNotification(ui = "Please select a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_cluster != "", "Please select a group variable")
    )
  }
  if (length(input$feature_cluster_variableSelect) < 3) {
    showNotification(ui = "Please select more features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(length(input$feature_cluster_variableSelect) > 1, "Please select at least three features for clustering")
    )
  }
  if (input$cluster_groups_en == TRUE) {
    if (!(input$groupVar_cluster %in% names(rvals$rval_loadeddf) | input$groupVar_cluster %in% names(rvals$rval_processeddf))) {
      showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
      validate(
        need(input$groupVar_cluster %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
        need(input$groupVar_cluster %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
      )
    }
  }
  if (input$groupVar_cluster %in% input$feature_cluster_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$groupVar_cluster %in% input$feature_cluster_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  }
  clustering()
})

# write output of k-means clustering
output$cluster_output <- shiny::renderPrint({
  clustering_alg()
})

# create plot based on k-means clustering object
output_cluster_p <- shiny::eventReactive(input$clusterBtn, {
  my_palette <- c("#a82203", "#208cc0", "#f1af3a", "#cf5e4e", "#637b31", "#003967", "#60D394", "#37123C", "#172A3A", "#8D5A97", "#01FDF6", "grey")
  withProgress(
    message = paste0("Rendering Plot"),
    value = 0,
    {
      incProgress(1 / 2)
      clustering()
      if (input$cluster_groups_en) {
        cluster_viz <- as.data.frame(rvals$rval_data_cluster[, -c(1)])
        cluster_viz <- cluster_viz %>% dplyr::mutate_all(as.numeric)
        cluster_viz <- cbind(rvals$rval_data_cluster[, c(1)], cluster_viz)
        cluster_viz <- cluster_viz %>% rename_at(1, ~ input$groupVar_cluster)
        cluster_viz[[input$groupVar_cluster]] <- as.character(cluster_viz[[input$groupVar_cluster]])
        cp <- fviz_cluster(clustering_alg(), data = cluster_viz[, -c(1)], geom = "point", palette = my_palette, ggtheme = theme_bw(), shape = 16) + geom_point(aes(shape = cluster_viz[, c(1)])) + scale_shape_manual(values=seq(0,15), name = input$groupVar_cluster)
        name <- paste("k-means_", input$cluster_n, "_clusters.png", sep = "")
        ggsave(name, cp, path = tempdir())
      } else {
        cluster_viz <- as.data.frame(rvals$rval_data_cluster[, -c(1)])
        cluster_viz <- cluster_viz %>% dplyr::mutate_all(as.numeric)
        cp <- fviz_cluster(clustering_alg(), data = cluster_viz, geom = "point", palette = my_palette, ggtheme = theme_bw(), shape = 16)
        name <- paste("k-means_", input$cluster_n, "_clusters.png", sep = "")
        ggsave(name, cp, path = tempdir())
      }
    }
  )
  rvals$rval_plot_cluster <- cp
  cp
})

# render cluster plot
output$cluster_plot <- renderPlot({
  output_cluster_p()
})

# download cluster plot
output$download_clusterplot <- downloadHandler(
  filename = function() {
    paste("k-means_", input$cluster_n, "_clusters.png", sep = "")
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
        name <- paste("k-means_", input$cluster_n, "_clusters.png", sep = "")
        shiny::incProgress(5 / 10)
        file.copy(name, file, overwrite = TRUE)
        setwd(workingdir)
      }
    )
  }
)

# download report for clustering
output$report_clustering <- downloadHandler(
  filename = "report_clustering.html",
  content = function(file) {
    withProgress(
      message = paste0("Generating Report"),
      value = 0,
      {
        shiny::incProgress(1 / 5)
        src_img <- file.path(getwd(), "www/tRigon_logo.png")
        file.copy(src_img, "tRigon_logo.png")
        tempReport <- file.path(getwd(), "report_cluster.Rmd")
        file.copy("report_cluster.Rmd", tempReport, overwrite = FALSE)
        shiny::incProgress(2 / 5)
        # Set up parameters to pass to Rmd document
        params <- list(session_info = sessioninfo::session_info()$platform,
                       feature_vars = input$feature_cluster_variableSelect,
                       clusters_n = input$cluster_n,
                       groups_en = input$cluster_groups_en,
                       group_var = input$groupVar_cluster,
                       groups = rvals$groups_lvl,
                       warning_data = rvals$data_warn,
                       warning_data_n = rvals$data_warn_n,
                       cluster_output = rvals$rval_output_cluster,
                       cluster_plot = rvals$rval_plot_cluster)
        shiny::incProgress(3 / 5)
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
        shiny::incProgress(4 / 5)
      }
    )
  }
)
