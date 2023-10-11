# statistical tests

#perform statistical test
stats_data <- reactive({
  withProgress(message = "Calculating", value = 0, {
    suppressWarnings({
      incProgress(1 / 5)
      var <- c(input$feature_stats_variableSelect, input$groupVar_stats)
      feature <- c(input$feature_stats_variableSelect)
      group_col <- input$groupVar_stats
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
      if (is.numeric(df_var[[input$groupVar_stats]])) {
        showNotification(ui = "Warning: group var is numeric", type = "warning", duration = 5, closeButton = TRUE)
      }
      if (length(unique(df_var[[input$groupVar_stats]])) >= 50) {
        showNotification(ui = "Please select fewer groups", type = "error", duration = NULL, closeButton = TRUE)
        validate(
          need(length(unique(df_var[[input$groupVar_stats]])) < 50, paste0(length(unique(df_var[[input$groupVar_stats]])), " groups reported. Cannot process more than 50 unique groups."))
        )
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
      if (input$select_test == "pairwise Wilcoxon-Rank") {
        wilcox <- pairwise.wilcox.test(df_var[[feature]][complete.cases(df_var[[feature]], df_var[[group_col]])], df_var[[group_col]][complete.cases(df_var[[group_col]], df_var[[feature]])], p.adjust.method = "bonferroni", alternative = "two.sided")
        s <- wilcox
        wilcox$data.name <- "df"
        wilcox_df <- as.data.frame(wilcox$p.value)
        s_df <- wilcox_df
        s_df[s_df < 2e-16] <- "<2e-16"
      }
      incProgress(2 / 5)
      if (input$select_test == "bootstrapped CI") {
        compare.levels <- function(i, j) {
          xi <- df_var[[feature]][as.integer(g) == i]
          xj <- df_var[[feature]][as.integer(g) == j]
          sink("NUL")
          ind <- c(rep(1, length(xi)), rep(2, length(xj)))
          boot.func <- function(x, idx) {
            d1 <- x[idx[ind == 1]]
            d2 <- x[idx[ind == 2]]
            fval <- func(d1, ...) - func(d2, ...)
            b <- two.boot(d1, d2, FUN, R = M, student = FALSE,M = NULL)
            fval <- c(fval, var(b$t))
            fval
          }
          b <- boot(c(xi, xj), statistic = boot.func, strata = ind)
          b$student <- student
          structure(b, class = "simpleboot")
          browser()
          #temp_booty <- two.boot(xi, xj, median, student = FALSE, 100, na.rm = TRUE)
          t <- boot.ci(temp_booty, type = c("basic"))
          temp_boot <- data.frame(0)
          temp_boot$diff <- median(xi, na.rm = TRUE) - median(xj, na.rm = TRUE)
          temp_boot$lower_CI <- t$basic[4]
          temp_boot$upper_CI <- t$basic[5]
          if (is.null(temp_boot$upper_CI)) {
            temp_boot$upper_CI <- temp_boot$diff
          }
          if (is.null(temp_boot$lower_CI)) {
            temp_boot$lower_CI <- temp_boot$diff
          }
          sink()
          temp_boot <- lapply(temp_boot, round, 2)
          out <- paste(temp_boot$diff, " [", temp_boot$lower_CI, ",", temp_boot$upper_CI, "]", sep = "")
        }
        boot.tbl <- function(compare.levels, level.names, p.adjust.method) {
          ix <- setNames(seq_along(level.names), level.names)
          pp <- outer(
            ix[-1L], ix[-length(ix)],
            function(ivec, jvec) {
              sapply(
                seq_along(ivec),
                function(k) {
                  i <- ivec[k]
                  j <- jvec[k]
                  if (i > j) {
                    compare.levels(i, j)
                  } else {
                    NA_real_
                  }
                }
              )
            }
          )
          il.tri <- lower.tri(pp, TRUE)
          pp
        }
        table_boot <- boot.tbl(compare.levels, levels(g))
        s <- list("difference in feature median with 100-times bootstrapped 95% CI", table_boot)
        s_df <- table_boot
      }
      incProgress(3 / 5)
      if (input$select_test == "Kruskal-Wallis") {
        s <- kruskal.test(df_var[[feature]] ~ df_var[[group_col]])
        s$data.name <- paste("df", input$feature_stats_variableSelect, "~", input$groupVar_stats, sep = " ")
        s_df <- data.frame(0)
        s_df$comparison <- input$groupVar_stats
        s_df$pvalue <- s$p.value
        s_df <- s_df %>% select(comparison, pvalue)
        s_df[s_df < 2e-16] <- "<2e-16"
      }
      incProgress(4 / 5)
      rvals$rval_calc <- s
      rvals$rval_calcdf <- s_df
    })
  })
  s
})

stats_alg <- eventReactive(input$statisticsBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (is.null(input$feature_stats_variableSelect)) {
    showNotification(ui = "Please select a feature", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(input$feature_stats_variableSelect), "Please select a feature for analysis")
    )
  }
  if (input$feature_stats_variableSelect == "... no features found") {
    showNotification(ui = "Please select a feature", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$feature_stats_variableSelect != "... no features found", "Please select a feature for analysis")
    )
  }
  if (input$groupVar_stats == "") {
    showNotification(ui = "Please specify a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_stats != "", "Please specify a group variable")
    )
  }
  if (!(input$groupVar_stats %in% names(rvals$rval_loadeddf) | input$groupVar_stats %in% names(rvals$rval_processeddf))) {
    showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_stats %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
      need(input$groupVar_stats %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
    )
  }
  if (input$groupVar_stats %in% input$feature_stats_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$groupVar_stats %in% input$feature_stats_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  } else {
    stats_data()
  }
})

# render test output to df
stats_algdf <- eventReactive(input$statisticsBtn, {
  if (is.null(dim(rvals$rval_loadeddf)) & is.null(dim(rvals$rval_processeddf))) {
    showNotification(ui = "Please load/process data first", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(rvals$rval_loadeddf)), "No processed data found"),
      need(!is.null(dim(rvals$rval_processeddf)), "No loaded data found")
    )
  }
  if (is.null(input$feature_stats_variableSelect)) {
    showNotification(ui = "Please select a feature", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(input$feature_stats_variableSelect), "Please select a feature for analysis")
    )
  }
  if (input$feature_stats_variableSelect == "... no features found") {
    showNotification(ui = "Please select a feature", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$feature_stats_variableSelect != "... no features found", "Please select a feature for analysis")
    )
  }
  if (input$groupVar_stats == "") {
    showNotification(ui = "Please specify a group variable", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_stats != "", "Please specify a group variable")
    )
  }
  if (!(input$groupVar_stats %in% names(rvals$rval_loadeddf) | input$groupVar_stats %in% names(rvals$rval_processeddf))) {
    showNotification(ui = "Column name not found", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(input$groupVar_stats %in% names(rvals$rval_loadeddf), "Column name not found in loaded dataframe"),
      need(input$groupVar_stats %in% names(rvals$rval_processeddf), "Column name not found in processed dataframe")
    )
  }
  if (input$groupVar_stats %in% input$feature_stats_variableSelect) {
    showNotification(ui = "Group and feature var overlap", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!(input$groupVar_stats %in% input$feature_stats_variableSelect), "Group variable and feature variable cannot be the same."),
    )
  } else {
    stats_data()
    stats_df <- rvals$rval_calcdf
  }
})

# write statistics output
output$stats_output <- renderPrint({
  stats_alg()
})

# write statistics output to df
output$stats_table <- DT::renderDataTable(stats_algdf())

# download stats data
output$download_statsdf <- downloadHandler(
  filename = function() {
    paste("stats_", input$select_test, "_", input$feature_stats_variableSelect, ".xlsx", sep = "")
  },
  content = function(file) {
    withProgress(
      message = paste0("Saving Data"),
      value = 0,
      {
        shiny::incProgress(1 / 10)
        Sys.sleep(1)
        shiny::incProgress(5 / 10)
        download_statsdf_c <- cbind(" " = rownames(rvals$rval_calcdf), rvals$rval_calcdf)
        writexl::write_xlsx(download_statsdf_c, file)
      }
    )
  }
)

# download report for statistical tests
output$report_stats <- downloadHandler(
  filename = "report_stats.html",
  content = function(file) {
    withProgress(
      message = paste0("Generating Report"),
      value = 0,
      {
        shiny::incProgress(1 / 5)
        src_img <- file.path(getwd(), "www/tRigon_logo.png")
        file.copy(src_img, "tRigon_logo.png")
        tempReport <- file.path(getwd(), "report_stats.Rmd")
        file.copy("report_stats.Rmd", tempReport, overwrite = FALSE)
        shiny::incProgress(2 / 5)
        # Set up parameters to pass to Rmd document
        params <- list(session_info = devtools::session_info()$platform,
                       feature_var = input$feature_stats_variableSelect,
                       group_var = input$groupVar_stats,
                       groups = rvals$groups_lvl,
                       test_selection = input$select_test,
                       stats_output = rvals$rval_calc,
                       stats_df = rvals$rval_calcdf,
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
