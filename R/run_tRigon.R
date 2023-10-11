#' @title Run tRigon application
#' @description Run tRigon application
#' @author David Hoelscher
#' @import boot caret data.table dplyr DT ggcorrplot ggplot2 ggpubr markdown patchwork randomForest RColorBrewer readxl shiny shinydashboard shinydashboardPlus shinyWidgets writexl
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom simpleboot two.boot
#' @importFrom factoextra fviz
#' @export
#' @return app
#' @examples
#' if(interactive()){}

run_tRigon <- function() {
  appDir <- system.file("app", package = "tRigon")
  if (appDir == "") {
    stop("Could not find Shiny application directory. Try re-installing `tRigon`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
