box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, div, h4, p],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox],
  echarts4r[echarts4rOutput, renderEcharts4r],
  shinyWidgets[actionBttn]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "pca",
    fluidRow(
      valueBoxOutput(ns("pc_1"), width = 4),
      valueBoxOutput(ns("pc_2"), width = 4),
      valueBoxOutput(ns("pc_3"), width = 4)
    ),
    fluidRow(
      box(
        title = "2D PCA",
        status = "primary",
        width = 6,
        height = 700,
        maximizable = TRUE,
        echarts4rOutput(ns("pca_2d_plot"), height = "650")
      ),
      box(
        title = "3D PCA",
        status = "primary",
        width = 6,
        height = 700,
        maximizable = TRUE,
        echarts4rOutput(ns("pca_3d_plot"), height = "650")
      )
    )
  )
  
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    output$pca_2d_plot <- renderEcharts4r({
      
      r6$plot_pca(view_3d = FALSE) 
      
    })
    
    output$pca_3d_plot <- renderEcharts4r({
      
      r6$plot_pca(view_3d = TRUE) 
      
    })
    
    output$pc_1 <- renderValueBox({
      
      value <- paste0(r6$pcs[1], " %")
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("thermometer-3", verify_fa = FALSE),
        color = "primary",
        footer = p("PC1", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    output$pc_2 <- renderValueBox({
      
      value <- paste0(r6$pcs[2], " %")
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("thermometer-2", verify_fa = FALSE),
        color = "primary",
        footer = p("PC2", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    output$pc_3 <- renderValueBox({
      
      value <- paste0(r6$pcs[3], " %")
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("thermometer-1", verify_fa = FALSE),
        color = "primary",
        footer = p("PC3", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
  })
}