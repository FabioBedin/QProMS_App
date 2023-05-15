box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput, icon, fluidRow, observeEvent, removeUI],
  bs4Dash[...],
  waiter[useWaiter, useWaitress, spin_5],
  fresh[create_theme, bs4dash_vars, bs4dash_yiq, bs4dash_layout, bs4dash_sidebar_light, bs4dash_status, bs4dash_color, use_theme, bs4dash_button]
)

box::use(
  app/view/tab1_upload_data_page,
  app/view/tab2_wrangling_data_page,
  app/view/tab3_missing_data_page,
  app/view/tab4_correlation_page,
  app/view/tab5_pca_page,
  app/view/tab6_statistics_univariate_page,
  app/view/tab7_statistics_multivariate_page,
  app/view/tab8_functional_analysis_ora_page,
  app/view/tab10_network_analysis_page,
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
    title = "QProMS beta",
    fullscreen = TRUE,
    preloader = list(html = spin_5(), color = "#adb5bd"),
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
        id = ns("sidebarMenu"),
        menuItem(
          text = "Upload Data",
          tabName = "upload_data",
          icon = icon("upload")
        ),
        menuItemOutput(ns("wrangling_data_blocked")),
        menuItemOutput(ns("wrangling_data")),
        menuItemOutput(ns("missing_data_blocked")),
        menuItemOutput(ns("missing_data")),
        menuItemOutput(ns("correlation_blocked")),
        menuItemOutput(ns("correlation")),
        menuItemOutput(ns("pca_blocked")),
        menuItemOutput(ns("pca")),
        menuItemOutput(ns("statistics_blocked")),
        menuItemOutput(ns("statistics")),
        menuItemOutput(ns("network_analysis_blocked")),
        menuItemOutput(ns("network_analysis")),
        menuItemOutput(ns("function_analysis_blocked")),
        menuItemOutput(ns("function_analysis"))
      )
    ),
    controlbar = dashboardControlbar(),
    footer = dashboardFooter(),
    body = dashboardBody(
      useWaiter(),
      useWaitress(),
      # use_theme(QProMS_theme),
      tabItems(
        tab1_upload_data_page$ui(ns("tab1_upload_data_page")),
        tab2_wrangling_data_page$ui(ns("tab2_wrangling_data_page")),
        tab3_missing_data_page$ui(ns("tab3_missing_data_page")),
        tab4_correlation_page$ui(ns("tab4_correlation_page")),
        tab5_pca_page$ui(ns("tab5_pca_page")),
        tab6_statistics_univariate_page$ui(ns("tab6_statistics_univariate_page")),
        tab7_statistics_multivariate_page$ui(ns("tab7_statistics_multivariate_page")),
        tab8_functional_analysis_ora_page$ui(ns("tab8_functional_analysis_ora_page")),
        tab10_network_analysis_page$ui(ns("tab10_network_analysis_page"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    options(shiny.maxRequestSize=10000*1024^2)
    
    object <- R6Class_QProMS$QProMS$new()
    
    output$wrangling_data_blocked <- renderMenu({ menuItem("Filter Data", icon = icon("lock"), tabName = "") })
    output$missing_data_blocked    <- renderMenu({ menuItem("Missing Data", icon = icon("lock"), tabName = "") })
    output$correlation_blocked  <- renderMenu({ menuItem("Correlation", icon = icon("lock"), tabName = "") })
    output$pca_blocked <- renderMenu({ menuItem("PCA", icon = icon("lock"), tabName = "") })
    output$statistics_blocked <- renderMenu({ menuItem("Statistics", icon = icon("lock"), tabName = "") })
    output$function_analysis_blocked <- renderMenu({ menuItem("Functional Analysis", icon = icon("lock"), tabName = "") })
    output$network_analysis_blocked <- renderMenu({ menuItem("Network Analysis", icon = icon("lock"), tabName = "") })
    
    unlock_pages <- tab1_upload_data_page$server("tab1_upload_data_page", r6 = object)
    tab2_wrangling_data_page$server("tab2_wrangling_data_page", r6 = object)
    tab3_missing_data_page$server("tab3_missing_data_page", r6 = object)
    tab4_correlation_page$server("tab4_correlation_page", r6 = object)
    tab5_pca_page$server("tab5_pca_page", r6 = object)
    tab6_statistics_univariate_page$server("tab6_statistics_univariate_page", r6 = object)
    tab7_statistics_multivariate_page$server("tab7_statistics_multivariate_page", r6 = object)
    tab8_functional_analysis_ora_page$server("tab8_functional_analysis_ora_page", r6 = object)
    tab10_network_analysis_page$server("tab10_network_analysis_page", r6 = object)
    
    
    observeEvent(unlock_pages(), {
      
      output$wrangling_data <- renderMenu({ menuItem("Filter Data", icon = icon("filter"), tabName = "wrangling_data") })
      removeUI(selector = "#app-wrangling_data_blocked")
 
      output$missing_data <- renderMenu({ menuItem("Missing Data", icon = icon("magnifying-glass"), tabName = "missing_data") })
      removeUI(selector = "#app-missing_data_blocked")

      output$correlation  <- renderMenu({ menuItem("Correlation", icon = icon("link"), tabName = "correlation") })
      removeUI(selector = "#app-correlation_blocked")

      output$pca <- renderMenu({ menuItem("PCA", icon = icon("slack"), tabName = "pca") })
      removeUI(selector = "#app-pca_blocked")

      output$statistics <- renderMenu({
        menuItem(
          "Statistics",
          icon = icon("circle-half-stroke"),
          menuSubItem(
            text = "Univariate",
            tabName = "statistics_uni",
            icon = icon("cube")
          ),
          menuSubItem(
            text = "Multivariate",
            tabName = "statistics_multi",
            icon = icon("cubes")
          )
        )
      })
      removeUI(selector = "#app-statistics_blocked")

      output$function_analysis <- renderMenu({ menuItem("Functional Analysis", icon = icon("chart-column"), tabName = "functional_analysis_ora") })
      removeUI(selector = "#app-function_analysis_blocked")
      
      output$network_analysis <- renderMenu({ menuItem("Network Analysis", icon = icon("code-fork"), tabName = "network") })
      removeUI(selector = "#app-network_analysis_blocked")
      
    })
    
  })
}
