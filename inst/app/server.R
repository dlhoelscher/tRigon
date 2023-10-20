library(dplyr)
library(ggplot2)
library(data.table)
library(DT)
library(readxl)
library(writexl)
library(ggridges)
library(RColorBrewer)
library(simpleboot)
library(boot)
library(factoextra)
library(ggpubr)
library(ggcorrplot)
library(randomForest)
library(caret)
library(patchwork)
library(markdown)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 10000000000 * 1024^2)
  set.seed(10)

  # reactive values
  rvals <- reactiveValues(
    rval_loadeddf = NULL,
    rval_metadatadf = NULL,
    rval_processeddf = NULL,
    rval_chosendf = NULL,
    rval_nfiles = NULL,
    rval_ID_metadata = NULL,
    rval_ID_feature = NULL,
    rval_IDmatches_df = NULL,
    na_omit = NULL,
    n_na = NULL,
    groups_lvl = NULL,
    rval_dsdf = NULL,
    rval_plot = NULL,
    rval_calc = NULL,
    rval_calcdf = NULL,
    rval_data_cluster = NULL,
    rval_output_cluster = NULL,
    rval_plot_cluster = NULL,
    rval_fi_output = NULL,
    rval_fi_plot = NULL,
    rval_corrdf = NULL,
    rval_corrplot = NULL,
    data_warn_n = NULL,
    data_warn = NULL
  )

  # define color palette
  qual_col_pals <- brewer.pal.info[brewer.pal.info$category == "qual", ]
  col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  my_palette <- c("#a82203", "#208cc0", "#f1af3a", "#cf5e4e", "#637b31", "#003967", "#60D394", "#37123C", "#172A3A", "#8D5A97", "#01FDF6", "grey")

  # load dependencies
  source("server-analysis-processData.R", local = TRUE)
  source("server-analysis-loadData.R", local = TRUE)
  source("server-analysis-descriptiveStats.R", local = TRUE)
  source("server-analysis-plots.R", local = TRUE)
  source("server-analysis-testingStats.R", local = TRUE)
  source("server-analysis-clustering.R", local = TRUE)
  source("server-analysis-featureImportance.R", local = TRUE)
  source("server-analysis-correlation.R", local = TRUE)
}
