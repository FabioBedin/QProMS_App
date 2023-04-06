box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, div, h4, p, plotOutput, renderPlot, observeEvent, req, reactiveVal, uiOutput, renderUI, isolate],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, boxLabel],
  echarts4r[echarts4rOutput, renderEcharts4r, e_show_loading],
  shinyWidgets[actionBttn, pickerInput],
  stringr[str_to_title],
  # waiter[Waiter, spin_5],
  dplyr[`%>%`, select, distinct, pull],
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
        title = "Scatter plot",
        status = "primary",
        width = 6,
        height = 700,
        maximizable = TRUE,
        echarts4rOutput(ns("scatter_plot"), height = "650")
        # plotOutput(ns("correlation_static_plot"), height = "650")
      )
    ),
    fluidRow(
      box(
        title = "Multi scatter plot",
        id = ns("multi"),
        status = "primary",
        width = 12,
        maximizable = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        label = boxLabel("Take time!", "warning", "Time consuming operation"),
        uiOutput(ns("ui_primary_input")),
        uiOutput(ns("multi_scatter"))
      )
    )
  )
  
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    init("plot", "boxes")
    
    # w <- Waiter$new(id = "multi")
    
    output$ui_primary_input <- renderUI({
      
      test <- r6$expdesign %>% 
        select(condition) %>% 
        distinct() %>% 
        pull()
      
      test <- c(test, "all")
      
      pickerInput(
        inputId = session$ns("primary_input"),
        label = "Select condition",
        choices = test, 
        multiple = FALSE,
        options = list(size = 5)
      )
      
      
    })
    
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
    
    x_scatter <- reactiveVal(value = NULL)
    y_scatter <- reactiveVal(value = NULL)
    corr_value <- reactiveVal(value = NULL)
    
    observeEvent(input$correlation_interactive_plot_clicked_data, {
      x_scatter(input$correlation_interactive_plot_clicked_data$value[1])
      y_scatter(input$correlation_interactive_plot_clicked_data$value[2])
      corr_value(input$correlation_interactive_plot_clicked_data$value[3])
    })
    
    # output$correlation_static_plot <- renderPlot({
    #   
    #   watch("plot")
    #   
    #   r6$plot_correlation_static(cor_method = r6$cor_method)
    #   
    # })
    
    output$scatter_plot <- renderEcharts4r({
      
      watch("plot")
      
      if(is.null(x_scatter()) | is.null(y_scatter())){
        x_scatter(r6$expdesign$label[1])
        y_scatter(r6$expdesign$label[2])
      }
      
      r6$plot_correlation_scatter(x = x_scatter(), y = y_scatter(), value = corr_value()) %>% 
        e_show_loading(text = "Loading...", color = "#35608D")
      
    })
    
    observeEvent(input$update ,{
      
      req(input$correlation_input)
      
      r6$cor_method <- input$correlation_input
      
      trigger("plot")
      trigger("boxes")
      
    })
    
    # output$multi_scatter <- renderUI({
    #   
    #   req(input$primary_input)
    #   
    #   w$show()
    #   
    #   r6$plot_multi_scatter(isolate({input$primary_input}))
    #   
    #   w$hide()
    #   
    # }) 
    
  
    observeEvent(input$primary_input, {
      
      # w$show()
      
      output$multi_scatter <- renderUI({
        
        req(input$primary_input)
        
        r6$plot_multi_scatter(isolate({input$primary_input}))
        
      })
      
      # w$hide()

    })
    
    
    
    
    
  })
}