box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput, icon, fluidRow],
  bs4Dash[...],
  fresh[create_theme, bs4dash_vars, bs4dash_yiq, bs4dash_layout, bs4dash_sidebar_light, bs4dash_status, bs4dash_color, use_theme, bs4dash_button]
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

# QProMS_theme <- create_theme(
#   theme = "default",
#   # bs4dash_vars(
#   #   navbar_light_bg = "#222629",
#   #   navbar_light_active_color = "#222629",
#   #   navbar_light_hover_color = "#222629",
#   #   navbar_dark_color = "#222629",
#   #   navbar_dark_active_color = "#222629",
#   #   navbar_dark_hover_color = "#222629"
#   # ),
#   bs4dash_sidebar_light(
#     bg = "#222629",
#     hover_bg = "#474B4F",
#     color = "#bec5cb",
#     hover_color = "#FFF",
#     active_color = "#FFF",
#     submenu_bg = "#222629",
#     submenu_color = "#bec5cb",
#     submenu_hover_color = "#FFF",
#     submenu_hover_bg = "#474B4F",
#     submenu_active_color = "#FFF",
#     submenu_active_bg = "#474B4F",
#     header_color = "#bec5cb"
#   ),
#   # bs4dash_layout(
#   #   main_bg = "#474B4F",
#   # ),
#   bs4dash_yiq(
#     contrasted_threshold = 150,
#     text_dark = "#222629",
#     text_light = "#FFF"
#   ),
#   bs4dash_status(
#     primary = "#61892F"
#   ),
#   bs4dash_button(
#     default_background_color = "#61892F",
#     default_color = "#FFF",
#     default_border_color = "#61892F"
#   )
# )

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
      # use_theme(QProMS_theme),
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
    tab4_correlation_page$server("tab4_correlation_page", r6 = object)
    tab5_pca_page$server("tab5_pca_page", r6 = object)
  })
}
