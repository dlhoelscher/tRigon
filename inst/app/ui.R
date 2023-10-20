library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

ui <- shinydashboardPlus::dashboardPage(

  ## header
  header = dashboardHeader(
    tags$li(
      class = "dropdown",
      tags$style(".main-header .navbar {margin-left: 200px; !important}
                  .main-header .logo {width: 200px; !important}")
    ),
    title = tagList(
      span(class = "logo-lg", strong("tRigon v0.3", style = "position: relative; right: 20px")),
      img(class = "logo-mini", src = "tRigon_icon.png", width = "230%", style = "position: relative; bottom: 1px; position: relative; left: 3px")
    )
  ),

  ## sidebar
  sidebar = shinydashboardPlus::dashboardSidebar(
    id = "sidebarmenu", width = 200,
    sidebarMenu(
      menuItem(text = strong("Home", style = "position: relative; left: 3px"), tabName = "home", icon = icon("home")),
      menuItem(
        text = strong("Analysis", style = "position: relative; left: 9px"), tabName = "analysis", startExpanded = FALSE, icon = icon("calculator", style = "position: relative; left: 2px"),
        menuSubItem("Processing", tabName = "processData", icon = icon("angle-right")),
        menuSubItem("Load Data", tabName = "loadData", icon = icon("angle-right")),
        menuSubItem("Feature Summary", tabName = "descriptiveStats", icon = icon("angle-right")),
        menuSubItem("Plots", tabName = "plots", icon = icon("angle-right")),
        menuSubItem("Statistical Tests", tabName = "testingStats", icon = icon("angle-right")),
        menuSubItem("Clustering", tabName = "clustering", icon = icon("angle-right")),
        menuSubItem("Feature Importance", tabName = "featureImportance", icon = icon("angle-right")),
        menuSubItem("Correlation", tabName = "correlation", icon = icon("angle-right"))
      ),
      menuItem(text = strong("Settings", style = "position: relative; left: 5px"), tabName = "settings", icon = icon("cog")),
      menuItem(text = strong("Help", style = "position: relative; left: 10px"), tabName = "help", icon = icon("question", style = "position: relative; left: 2px")),
      menuItem(text = strong("Contact", style = "position: relative; left: 5px"), tabName = "contact", icon = icon("address-book"))
    )
  ),

  ## body
  body = dashboardBody(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme.css"),
    tags$style(HTML("table.dataTable tr.selected td,table.dataTable td.selected {background-color: #3EB489 !important;}")),
    tags$style(HTML("table.dataTable.stripe>tbody>tr.odd.selected>*, table.dataTable.display>tbody>tr.odd.selected>* {box-shadow: none !important}")),
    tags$style(HTML("table.dataTable.stripe>tbody>tr.even.selected>*, table.dataTable.display>tbody>tr.even.selected>* {box-shadow: none !important}")),
    tags$style(HTML("table.dataTable.hover>tbody>tr.selected:hover>*, table.dataTable.display>tbody>tr.selected:hover>* {box-shadow: none !important}")),
    tags$style(HTML(".dataTables_wrapper .dataTables_processing {display: none !important;}")),
    tags$style(HTML(".progress-bar {background-color: #3EB489}")),
    tags$style(HTML(".selected {background-color: #3EB489 !important;}")),
    tags$style(HTML(".dropdown-item.active {background-color: #3EB489 !important;}")),
    tags$style(HTML(".irs--shiny .irs-bar {border-top: 1px solid #3EB489; border-bottom: 1px solid #3EB489; background: #3EB489}")),
    tags$style(HTML(".irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {background-color: #3EB489}")),
    tags$style(HTML(".bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-primary, .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-primary {background: #3EB489}")),
    tags$style(HTML(".bootstrap-switch .bootstrap-switch-handle-on, .bootstrap-switch .bootstrap-switch-handle-off, .bootstrap-switch .bootstrap-switch-label {padding-right: 6px; padding-left: 6px}")),
    tags$style(HTML(".btn-primary:focus {background-color: #3EB489}")),
    tags$style(HTML(".btn-primary:active {background-color: #3EB489 !important}")),
    tags$style(HTML(".btn-primary:active {border-color: #FFFFF !important}")),
    tags$style(HTML(".btn-primary:focus {border-color: #3EB489}")),
    tags$style(HTML(".btn-primary:hover {border-color: #37a17b}")),
    tags$style(HTML(".label-default.bg-default {background-color: #3EB489 !important}")),
    tags$style(HTML(".sidebar-menu li>a>.fa-angle-left {position: absolute; top: 53%;}")),
    tags$style(HTML(".sidebar-menu .fas.pull-right.fa-angle-down {position: absolute; top: 35% !important; left: 85%}")),
    tags$style(HTML(".sidebar-mini:not(.sidebar-mini-expand-feature).sidebar-collapse .sidebar-menu>li:hover>a>span:not(.pull-right), .sidebar-mini:not(.sidebar-mini-expand-feature).sidebar-collapse .sidebar-menu>li:hover>.treeview-menu {display: block !important;position: absolute;width: 170px;left: 50px;}")),
    tabItems(
      tabItem(tabName = "home", source("ui-home-page.R", local = TRUE)$value),
      tabItem(tabName = "processData", source("ui-analysis-processData.R", local = TRUE)$value),
      tabItem(tabName = "loadData", source("ui-analysis-loadData.R", local = TRUE)$value),
      tabItem(tabName = "descriptiveStats", source("ui-analysis-descriptiveStats.R", local = TRUE)$value),
      tabItem(tabName = "plots", source("ui-analysis-plots.R", local = TRUE)$value),
      tabItem(tabName = "testingStats", source("ui-analysis-testingStats.R", local = TRUE)$value),
      tabItem(tabName = "clustering", source("ui-analysis-clustering.R", local = TRUE)$value),
      tabItem(tabName = "featureImportance", source("ui-analysis-featureImportance.R", local = TRUE)$value),
      tabItem(tabName = "correlation", source("ui-analysis-correlation.R", local = TRUE)$value),
      tabItem(tabName = "settings", source("ui-settings-page.R", local = TRUE)$value),
      tabItem(tabName = "help", source("ui-help-page.R", local = TRUE)$value),
      tabItem(tabName = "contact", source("ui-contact-page.R", local = TRUE)$value)
    )
  ),

  ## footer
  footer = dashboardFooter(
    left = "LaBOORatory of Nephropathology, RWTH Aachen University",
    right = "Copyright (C) 2023, code licensed under GPL-3"
  )
)
