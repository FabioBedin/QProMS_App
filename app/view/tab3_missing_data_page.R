box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, sliderInput, updateSelectInput, updateSliderInput, div, observeEvent, observe, req, h4, p, uiOutput, renderUI, column],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, boxLabel, bs4Callout, accordion, accordionItem, updateAccordion],
  shinyWidgets[actionBttn],
  echarts4r[echarts4rOutput, renderEcharts4r],
  gargoyle[init, watch, trigger],
  reactable[reactableOutput, renderReactable]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "missing_data",
    fluidRow(
      valueBoxOutput(ns("n_proteins"), width = 3),
      valueBoxOutput(ns("total_missing_data"), width = 3),
      valueBoxOutput(ns("missing_mar"), width = 3),
      valueBoxOutput(ns("missing_mnar"), width = 3)
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
                  inputId = ns("imputation_input"),
                  label = "imputation strategy",
                  choices = c("Mixed" = "mixed", "Perseus" = "perseus", "None" = "none"),
                  selected = "mixed"
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                sliderInput(
                  inputId = ns("shift_slider"),
                  label = "Down shift",
                  min = 1.6,
                  max = 2,
                  value = 1.8,
                  step = 0.1
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                sliderInput(
                  inputId = ns("scale_slider"),
                  label = "Scale",
                  min = 0.1,
                  max = 0.5,
                  value = 0.3,
                  step = 0.1
                )
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
        title = "Missing data Counts",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        echarts4rOutput(ns("missing_data_counts_plot"), height = "450")
      ),
      box(
        title = "Missing data distribution",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        echarts4rOutput(ns("missval_distribution_plot"), height = "450")
      ),
      box(
        title = "Effect of imputation: Before",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        echarts4rOutput(ns("pre_imputation_plot"), height = "450")
      ),
      box(
        title = "Effect of imputation: After",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        # sidebar = boxSidebar(),
        echarts4rOutput(ns("post_imputation_plot"), height = "450")
      )
    ),
    fluidRow(
      box(
        title = "Distribution for each sample",
        status = "primary",
        width = 12,
        maximizable = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        label = boxLabel("Take time!", "warning", "Time consuming operation"),
        uiOutput(ns("multiple_distribution"))
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
    )
  )
  
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    init("plot", "boxes")
    
    output$n_proteins <- renderValueBox({
      
      
      watch("boxes")
      
      value <- length(unique(r6$filtered_data$gene_names))
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("database"),
        color = "primary",
        footer = p("Total N° of proteins", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$total_missing_data <- renderValueBox({
      
      watch("boxes")
      
      value <- r6$total_missing_data(raw = FALSE)
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("eye-slash"),
        color = "primary",
        footer = p("Total missing values", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$missing_mar <- renderValueBox({
      
      watch("boxes")
      
      value <- r6$missing_data_type(type = "MAR")
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("random", verify_fa = FALSE),
        color = "primary",
        footer = p("Missing at random (MAR)", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$missing_mnar <- renderValueBox({
      
      watch("boxes")
      
      value <- r6$missing_data_type(type = "MNAR")
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("low-vision", verify_fa = FALSE),
        color = "primary",
        footer = p("Missing not at random (MNAR)", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$missing_data_counts_plot <- renderEcharts4r({
      
      watch("plot")
      
      r6$plot_missing_data() 
      
    })
    
    output$missval_distribution_plot <- renderEcharts4r({
      
      watch("plot")
      
      r6$plot_missval_distribution() 
      
    })
    
    output$pre_imputation_plot <- renderEcharts4r({
      
      watch("plot")
      
      r6$plot_imputation(data = r6$normalized_data, imp_visualization = FALSE) 
      
    })
    
    output$post_imputation_plot <- renderEcharts4r({
      
      watch("plot")
      
      if(r6$imp_methods == "none"){
        data <- r6$normalized_data
        imp = FALSE
      }else{
        data <- r6$imputed_data
        imp = TRUE
      }
      
      r6$plot_imputation(data = data, imp_visualization = imp) 
      
    })
    
    observe({
      
      watch("params")
      
      updateSelectInput(inputId = "imputation_input", selected = r6$imp_methods)
      updateSliderInput(inputId = "shift_slider", value = r6$imp_shift)
      updateSliderInput(inputId = "scale_slider", value = r6$imp_scale)
      
      
    })
    
    observeEvent(input$update ,{
      
      req(input$imputation_input)
      req(input$shift_slider)
      req(input$scale_slider)
      
      r6$imp_methods <- input$imputation_input
      r6$imp_shift <- input$shift_slider
      r6$imp_scale <- input$scale_slider
      
      r6$imputation(
        imp_methods = r6$imp_methods,
        shift = r6$imp_shift,
        scale = r6$imp_scale,
        unique_visual = FALSE
      )
      
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
      
      r6$print_table(data)
      
    })
    
    output$multiple_distribution <- renderUI({
      
      watch("plot")
      
      r6$plot_multiple_distribution()
      
    })
    
  })
}