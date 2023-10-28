#' @title Run tRigon application
#' @description Run tRigon application
#' @author David Hoelscher
#' @rawNamespace import(shinydashboard, except = c(dashboardPage, dashboardSidebar))
#' @rawNamespace import(shiny, except = renderDataTable)
#' @import boot caret dplyr ggcorrplot ggplot2 markdown patchwork RColorBrewer readxl shinyWidgets writexl
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom simpleboot two.boot
#' @importFrom factoextra fviz
#' @importFrom DT renderDataTable
#' @importFrom data.table fread
#' @importFrom data.table rbindlist
#' @importFrom randomForest randomForest
#' @importFrom shinydashboardPlus dashboardPage
#' @importFrom shinydashboardPlus dashboardSidebar
#' @importFrom ggpubr stat_cor
#' @importFrom sessioninfo session_info
#' @export
#' @return app
#' @examples
#' if(interactive()){
#' run_tRigon()
#' }

run_tRigon <- function() {
  appDir <- system.file("app", package = "tRigon")
  if (appDir == "") {
    stop("Could not find Shiny application directory. Try re-installing `tRigon`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
