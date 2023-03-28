box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, div, h4, p, plotOutput, renderPlot, observeEvent, req],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox],
  echarts4r[echarts4rOutput, renderEcharts4r],
  shinyWidgets[actionBttn],
  gargoyle[init, watch, trigger],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "statistics",
    fluidRow(
      valueBoxOutput(ns("tested_cond"), width = 2),
      valueBoxOutput(ns("significant"), width = 2),
      valueBoxOutput(ns("up_reg"), width = 2),
      valueBoxOutput(ns("down_reg"), width = 2),
      valueBoxOutput(ns("fdr_thr"), width = 2),
      valueBoxOutput(ns("fc_thr"), width = 2)
    ),
    fluidRow(
      box(
        title = "Volcano plot",
        status = "primary",
        width = 8,
        height = 700,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = ns("correlation_sidebar"),
          startOpen = TRUE,
          div(
            style = "padding-right: 0.5rem",
            h3("Correlation methods"),
            selectInput(
              inputId = ns("correlation_input"),
              label = NULL,
              choices = c("Pearson" = "pearson", "Kendall" = "kendall", "Spearman" = "spearman"),
              selected = "pearson"
            ),
            actionBttn(
              inputId = ns("update"),
              label = "Update", 
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          )
        ),
        echarts4rOutput(ns("correlation_interactive_plot"), height = "650")
      ),
      box(
        title = "Result table",
        status = "primary",
        width = 4,
        height = 700,
        maximizable = TRUE,
        # collapsed = TRUE,
        echarts4rOutput(ns("scatter_plot"), height = "650")
        # plotOutput(ns("correlation_static_plot"), height = "650")
      )
    )
  )

}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    init("plot", "boxes")
    
    output$tested_cond <- renderValueBox({
      
      watch("boxes")
      
      value <- "A_vs_B"
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("database"),
        color = "primary",
        footer = p("Primary tested condition", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$significant <- renderValueBox({
      
      watch("boxes")
      
      value <- 23
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("link"),
        color = "primary",
        footer = p("Significant", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$up_reg <- renderValueBox({
      
      watch("boxes")
      
      value <- 21
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("database"),
        color = "primary",
        footer = p("Up-regulated", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$down_reg <- renderValueBox({
      
      watch("boxes")
      
      value <- 23
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("link"),
        color = "primary",
        footer = p("Down-regulated", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$fdr_thr <- renderValueBox({
      
      watch("boxes")
      
      value <- 0.05
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("database"),
        color = "primary",
        footer = p("FDR", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$fc_thr <- renderValueBox({
      
      watch("boxes")
      
      value <- 1
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("link"),
        color = "primary",
        footer = p("Fold change", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })

  })
}
