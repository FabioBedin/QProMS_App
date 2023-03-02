box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, div, h4, p, plotOutput, renderPlot, observeEvent, req],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox],
  echarts4r[echarts4rOutput, renderEcharts4r],
  shinyWidgets[actionBttn],
  stringr[str_to_title],
  gargoyle[init, watch, trigger],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "correlation",
    fluidRow(
      valueBoxOutput(ns("n_samples"), width = 6),
      valueBoxOutput(ns("corr_method"), width = 6)
    ),
    fluidRow(
      box(
        title = "Correlation plot",
        status = "primary",
        width = 6,
        height = 700,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = ns("correlation_sidebar"),
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
        title = "Multi scatter plot",
        status = "primary",
        width = 6,
        height = 700,
        maximizable = TRUE,
        collapsed = TRUE,
        plotOutput(ns("correlation_static_plot"), height = "650")
      )
    )
  )
  
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    init("plot", "boxes")
    
    output$n_samples <- renderValueBox({
      
      watch("boxes")
      
      value <- length(unique(r6$expdesign$label))
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("database"),
        color = "primary",
        footer = p("Samples", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$corr_method <- renderValueBox({
      
      watch("boxes")
      
      value <- str_to_title(r6$cor_method)
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("link"),
        color = "primary",
        footer = p("Correlation method", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$correlation_interactive_plot <- renderEcharts4r({
      
      watch("plot")
      
      r6$plot_correlation_interactive(cor_method = r6$cor_method) 
      
    })
    
    output$correlation_static_plot <- renderPlot({
      
      watch("plot")
      
      r6$plot_correlation_static(cor_method = r6$cor_method)
      
    })
    
    observeEvent(input$update ,{
      
      req(input$correlation_input)
      
      r6$cor_method <- input$correlation_input
      
      trigger("plot")
      trigger("boxes")
      
    })
    
  })
}