box::use(
  shiny[moduleServer, NS, fluidRow, column, icon, h3, selectInput, div, h4, p, plotOutput, renderPlot, observeEvent, req, reactiveVal, uiOutput, renderUI, isolate, reactive],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, boxLabel, accordion, accordionItem, updateAccordion],
  echarts4r[echarts4rOutput, renderEcharts4r, e_show_loading],
  shinyWidgets[actionBttn, pickerInput],
  stringr[str_to_title],
  reactable[reactableOutput, renderReactable, reactable, colDef, getReactableState],
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
      column(
        width = 11,
        accordion(
          id = ns("advance_params"),
          accordionItem(
            title = "Parameters",
            status = "primary",
            collapsed = TRUE,
            solidHeader = TRUE,
            div(
              style = "display: flex; justify-content: center; gap: 5rem; align-items: start;",
              div(
                style = "width: 100%; flex: 1 1 0;",
                selectInput(
                  inputId = ns("correlation_input"),
                  label = "Correlation method",
                  choices = c("Pearson" = "pearson", "Kendall" = "kendall", "Spearman" = "spearman"),
                  selected = "pearson"
                ),
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
              )
            )
          ),
          width = 12
        )
      ),
      column(
        width = 1,
        div(
          style = "margin-top: 2.5px;",
          actionBttn(
            inputId = ns("update"),
            label = "Update", 
            style = "material-flat",
            color = "success",
            size = "md",
            block = TRUE
          ) 
        )
      )
    ),
    fluidRow(
      box(
        title = "Correlation plot",
        status = "primary",
        width = 6,
        height = 700,
        maximizable = TRUE,
        echarts4rOutput(ns("correlation_interactive_plot"), height = "650")
      ),
      box(
        title = "Scatter plot",
        status = "primary",
        width = 6,
        height = 700,
        maximizable = TRUE,
        echarts4rOutput(ns("scatter_plot"), height = "650")
      )
    ),
    fluidRow(
      box(
        title = "Imputed Table",
        status = "primary",
        width = 12,
        maximizable = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        reactableOutput(ns("table"))
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
        selected = NULL,
        choices = test, 
        multiple = FALSE,
        width = 300,
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
      
      if(r6$imp_methods == "none"){
        data <- r6$normalized_data
      }else{
        data <- r6$imputed_data
      }
      table <- r6$print_table(data, df = TRUE)
      highlights <- table[gene_selected(), ] %>% 
        pull(gene_names)
      
      r6$plot_correlation_scatter(
        x = x_scatter(),
        y = y_scatter(),
        value = corr_value(),
        highlights_names = highlights
      ) %>% 
        e_show_loading(text = "Loading...", color = "#35608D")
      
    })
    
    observeEvent(input$update ,{
      
      req(input$correlation_input)
      
      r6$cor_method <- input$correlation_input
      
      updateAccordion(id = "advance_params", selected = NULL)
      
      trigger("plot")
      trigger("boxes")
      
    })
    
    output$table <- renderReactable({
      
      watch("plot")
      
      if(r6$imp_methods == "none"){
        data <- r6$normalized_data
      }else{
        data <- r6$imputed_data
      }
      
      table <- r6$print_table(data, df = TRUE)
      
      reactable(
        table,
        searchable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        height = "auto",
        selection = "multiple",
        onClick = "select",
        defaultColDef = colDef(align = "center", minWidth = 200),
        columns = list(
          gene_names = colDef(
            name = "Gene names",
            sticky = "left",
            style = list(borderRight  = "1px solid #eee")
          )
        )
      )
      
    })
    
    gene_selected <- reactive(getReactableState("table", "selected"))
    
  
    observeEvent(input$primary_input, {
      
      output$multi_scatter <- renderUI({
        
        req(input$primary_input)
        
        if(r6$imp_methods == "none"){
          data <- r6$normalized_data
        }else{
          data <- r6$imputed_data
        }
        table <- r6$print_table(data, df = TRUE)
        highlights <- table[gene_selected(), ] %>% 
          pull(gene_names)
        
        r6$plot_multi_scatter(isolate({input$primary_input}), highlights_names = highlights)
        
      })
      

    })
    
    
    
    
    
  })
}