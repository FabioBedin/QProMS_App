box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput, icon, fluidRow],
  bs4Dash[...],
)

box::use(
  app/view/tab1_upload_data_page,
  app/view/tab2_wrangling_data_page,
  app/view/tab3_missing_data_page,
  app/view/tab4_correlation_page,
  app/view/tab5_pca_page,
)

box::use(
  app/logic/R6Class_QProMS,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    title = "QProMS",
    fullscreen = TRUE,
    header = dashboardHeader(
      title =  dashboardBrand(
        title = "QProMS",
        color = "primary",
        href = NULL,
        image = NULL
      )
    ),
    sidebar = dashboardSidebar(
      tags$br(),
      # sidebarHeader("Workflow"),
      # tags$br(),
      sidebarMenu(
        id = "sidebarMenu",
        menuItem(
          text = "Upload Data",
          tabName = "upload_data",
          icon = icon("upload")
        ),
        menuItem(
          text = "Filter Data",
          tabName = "wrangling_data",
          icon = icon("filter")
        ),
        menuItem(
          text = "Missing Data",
          tabName = "missing_data",
          icon = icon("magnifying-glass")
        ),
        menuItem(
          text = "Correlation",
          tabName = "correlation",
          icon = icon("link")
        ),
        menuItem(
          text = "PCA",
          tabName = "pca",
          icon = icon("slack")
        ),
        menuItem(
          text = "Statistics",
          tabName = "statistics",
          icon = icon("circle-half-stroke")
        ),
        menuItem(
          text = "Function Analysis",
          tabName = "function_analysis",
          icon = icon("chart-column")
        )
      )
    ),
    controlbar = dashboardControlbar(),
    footer = dashboardFooter(),
    body = dashboardBody(
      tabItems(
        tab1_upload_data_page$ui(ns("tab1_upload_data_page")),
        tab2_wrangling_data_page$ui(ns("tab2_wrangling_data_page")),
        tab3_missing_data_page$ui(ns("tab3_missing_data_page")),
        tab4_correlation_page$ui(ns("tab4_correlation_page")),
        tab5_pca_page$ui(ns("tab5_pca_page"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    options(shiny.maxRequestSize=10000*1024^2)
    
    object <- R6Class_QProMS$QProMS$new()
    
    tab1_upload_data_page$server("tab1_upload_data_page", r6 = object)
    tab2_wrangling_data_page$server("tab2_wrangling_data_page", r6 = object)
    tab3_missing_data_page$server("tab3_missing_data_page", r6 = object)
    tab4_correlation_page$server("tab4_correlation_page")
    tab5_pca_page$server("tab5_pca_page")
  })
}
