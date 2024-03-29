box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput, icon, fluidRow, observeEvent, removeUI],
  bs4Dash[...],
  waiter[useWaiter, useWaitress, spin_5],
  shiny.emptystate[use_empty_state],
  fresh[create_theme, bs4dash_vars, bs4dash_yiq, bs4dash_layout, bs4dash_sidebar_light, bs4dash_status, bs4dash_color, use_theme, bs4dash_button, bs_vars_button]
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
  app/view/tab9_functional_analysis_gsea_page,
  app/view/tab10_network_analysis_page,
  app/view/tab11_download_table_page,
  app/view/tab12_report_page,
  app/view/tab13_protein_rank_page,
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
      minified = FALSE,
      tags$br(),
      sidebarMenu(
        id = ns("sidebarMenu"),
        menuItem(
          text = "Upload Data",
          tabName = "upload_data",
          icon = icon("upload")
        ),
        tags$div(id = 'analysis', "Analysis", style = "
             letter-spacing: 3.2px;
             line-height: 42px;
             text-transform: uppercase;
             text-align: center;
             margin: 10px 0px;
             background-color: #ffffff1a;
             border-radius: 0.25rem;
             color: #fff;"),
        # bs4SidebarHeader("Workflow"),
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
        menuItemOutput(ns("function_analysis")),
        tags$div(id = 'tools', "Save results", style = "
             letter-spacing: 3.2px;
             line-height: 42px;
             text-transform: uppercase;
             text-align: center;
             margin: 10px 0px;
             background-color: #ffffff1a;
             border-radius: 0.25rem;
             color: #fff;"),
        menuItemOutput(ns("download_table_blocked")),
        menuItemOutput(ns("download_table")),
        menuItemOutput(ns("report_blocked")),
        menuItemOutput(ns("report"))
      )
    ),
    controlbar = NULL,
    footer = dashboardFooter(),
    body = dashboardBody(
      useWaiter(),
      use_empty_state(),
      useWaitress(color = "#adb5bd"),
      # use_theme(QProMS_theme),
      tabItems(
        tab1_upload_data_page$ui(ns("tab1_upload_data_page")),
        tab2_wrangling_data_page$ui(ns("tab2_wrangling_data_page")),
        tab3_missing_data_page$ui(ns("tab3_missing_data_page")),
        tab4_correlation_page$ui(ns("tab4_correlation_page")),
        tab13_protein_rank_page$ui(ns("tab13_protein_rank_page")),
        tab5_pca_page$ui(ns("tab5_pca_page")),
        tab6_statistics_univariate_page$ui(ns("tab6_statistics_univariate_page")),
        tab7_statistics_multivariate_page$ui(ns("tab7_statistics_multivariate_page")),
        tab8_functional_analysis_ora_page$ui(ns("tab8_functional_analysis_ora_page")),
        tab9_functional_analysis_gsea_page$ui(ns("tab9_functional_analysis_gsea_page")),
        tab10_network_analysis_page$ui(ns("tab10_network_analysis_page")),
        tab11_download_table_page$ui(ns("tab11_download_table_page")),
        tab12_report_page$ui(ns("tab12_report_page"))
      )
    ),
    dark = NULL
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
    output$download_table_blocked <- renderMenu({ menuItem("Download Tables", icon = icon("lock"), tabName = "") })
    output$report_blocked <- renderMenu({ menuItem("Generate Report", icon = icon("lock"), tabName = "") })
    
    unlock_pages <- tab1_upload_data_page$server("tab1_upload_data_page", r6 = object)
    tab2_wrangling_data_page$server("tab2_wrangling_data_page", r6 = object)
    tab3_missing_data_page$server("tab3_missing_data_page", r6 = object)
    tab4_correlation_page$server("tab4_correlation_page", r6 = object)
    tab13_protein_rank_page$server("tab13_protein_rank_page", r6 = object)
    tab5_pca_page$server("tab5_pca_page", r6 = object)
    tab6_statistics_univariate_page$server("tab6_statistics_univariate_page", r6 = object)
    tab7_statistics_multivariate_page$server("tab7_statistics_multivariate_page", r6 = object)
    tab8_functional_analysis_ora_page$server("tab8_functional_analysis_ora_page", r6 = object)
    tab9_functional_analysis_gsea_page$server("tab9_functional_analysis_gsea_page", r6 = object)
    tab10_network_analysis_page$server("tab10_network_analysis_page", r6 = object)
    tab11_download_table_page$server("tab11_download_table_page", r6 = object)
    tab12_report_page$server("tab12_report_page", r6 = object)
    
    
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
          ),
          menuSubItem(
            text = "Protein Rank",
            tabName = "protein_rank",
            icon = icon("up-long")
          )
        )
      })
      removeUI(selector = "#app-statistics_blocked")

      # output$function_analysis <- renderMenu({ menuItem("Functional Analysis", icon = icon("chart-column"), tabName = "functional_analysis_ora") })
      output$function_analysis <- renderMenu({
        menuItem(
          "Functional Analysis",
          icon = icon("chart-column"),
          menuSubItem(
            text = "ORA",
            tabName = "functional_analysis_ora",
            icon = icon("bars-progress")
          ),
          menuSubItem(
            text = "GSEA",
            tabName = "functional_analysis_gsea",
            icon = icon("server")
          )
        )
      })
      removeUI(selector = "#app-function_analysis_blocked")
      
      output$network_analysis <- renderMenu({ menuItem("Network Analysis", icon = icon("code-fork"), tabName = "network") })
      removeUI(selector = "#app-network_analysis_blocked")
      
      output$download_table <- renderMenu({ menuItem("Download Tables", icon = icon("download"), tabName = "download") })
      removeUI(selector = "#app-download_table_blocked")
      
      output$report <- renderMenu({ menuItem("Generate Report", icon = icon("file"), tabName = "report") })
      removeUI(selector = "#app-report_blocked")
      
    })
    
  })
}
