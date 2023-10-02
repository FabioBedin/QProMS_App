box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, updateSelectInput, updateSliderInput, sliderInput, br, div, observeEvent, observe, req, checkboxInput, h4, h5, p, column, reactive],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, bs4Callout, accordion, accordionItem, updateAccordion],
  shinyWidgets[actionBttn, prettyCheckbox, updatePrettyCheckbox],
  echarts4r[echarts4rOutput, renderEcharts4r],
  gargoyle[init, watch, trigger],
  reactable[reactableOutput, renderReactable]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "wrangling_data",
    fluidRow(
      valueBoxOutput(ns("n_proteins"), width = 3),
      valueBoxOutput(ns("total_missing_data"), width = 3),
      valueBoxOutput(ns("max_intensity"), width = 3),
      valueBoxOutput(ns("min_intensity"), width = 3)
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
                    inputId = ns("valid_values_input"),
                    label = "Valid values filters",
                    choices = c("In at least one group" = "alog", "In each group" = "each_grp", "In total" = "total"),
                    selected = "alog"
                  ),
                  sliderInput(
                    inputId = ns("valid_values_slider"),
                    label = "Percentage",
                    min = 0,
                    max = 100,
                    value = 75,
                    step = 1
                  )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                  selectInput(
                    inputId = ns("normalization_input"),
                    label = "Normalization",
                    choices = c("None", "VSN"),
                    selected = "None"
                  )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                selectInput(
                  inputId = ns("peptides_input"),
                  label = "MaxQuant peptides column",
                  choices = c("Peptides" = "peptides", "Unique peptides" = "unique", "Razor and unique peptides" = "razor"),
                  selected = "peptides"
                ),
                sliderInput(
                  inputId = ns("peptides_slider"),
                  label = "Minimum number of peptides",
                  min = 0,
                  max = 10,
                  value = 2,
                  step = 1
                ),
                h5("Remove:"),
                div(
                  style = "display: flex; gap: 0.5rem; align-items: center;",
                  div(
                    style = "width: 100%; flex: 1 1 0;",
                    checkboxInput(
                      inputId = ns("rev"),
                      label = "Reverse",
                      value = TRUE
                    )
                  ),
                  div(
                    style = "width: 100%; flex: 1 1 0;",
                    checkboxInput(
                      inputId = ns("cont"),
                      label = "Contaminant",
                      value = TRUE
                    )
                  ),
                  div(
                    style = "width: 100%; flex: 1 1 0;",
                    checkboxInput(
                      inputId = ns("oibs"),
                      label = "Only identify by site",
                      value = TRUE
                    )
                  )
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
            inputId = ns("update_parameters"),
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
        title = "Protein Counts",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        # sidebar = boxSidebar(),
        echarts4rOutput(ns("protein_counts_plot"), height = "450")
      ),
      box(
        title = "Upset Plot",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        # sidebar = boxSidebar(),
        echarts4rOutput(ns("valid_values_plot"), height = "450")
      ),
      box(
        title = "Intensity distribution",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        # sidebar = boxSidebar(),
        echarts4rOutput(ns("distribution_plot"), height = "450")
      ),
      box(
        title = "Coefficient of variation (CV)",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        echarts4rOutput(ns("cv_plot"), height = "450")
      )
    ),
    fluidRow(
      box(
        title = "Filtred Table",
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
    
    output$max_intensity <- renderValueBox({
      
      watch("boxes")
      
      value <- round(max(r6$filtered_data$intensity, na.rm = TRUE), 2)
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("arrow-up"),
        color = "primary",
        footer = p("Max intensity value", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$min_intensity <- renderValueBox({
      
      watch("boxes")
      
      value <- round(min(r6$filtered_data$intensity, na.rm = TRUE), 2)
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("arrow-down"),
        color = "primary",
        footer = p("Min intensity value", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$protein_counts_plot <- renderEcharts4r({
      
      watch("plot")
      
      r6$plot_protein_counts() 
      
    })
    
    output$valid_values_plot <- renderEcharts4r({
      
      watch("plot")
      
      r6$plot_protein_coverage() 
      
    })
    
    output$distribution_plot <- renderEcharts4r({
      
      watch("plot")
      
      r6$plot_distribution() 
      
    })
    
    output$cv_plot <- renderEcharts4r({
      
      watch("plot")
      
      r6$plot_cv() 
      
    })
    
    observe({
      
      watch("params")
      
      updateSelectInput(inputId = "valid_values_input", selected = r6$valid_val_filter)
      updateSliderInput(inputId = "valid_values_slider", value = r6$valid_val_thr*100)
      updateSelectInput(inputId = "normalization_input", selected = r6$norm_methods)
      updateSelectInput(inputId = "peptides_input", selected = r6$pep_filter)
      updateSliderInput(inputId = "peptides_slider", value = r6$pep_thr)
      updatePrettyCheckbox(inputId = "rev", value = r6$rev)
      updatePrettyCheckbox(inputId = "cont", value = r6$cont)
      updatePrettyCheckbox(inputId = "oibs", value = r6$oib)

    })
    
    observeEvent(input$update_parameters ,{
      
      req(input$valid_values_input)
      req(input$valid_values_slider)
      req(input$normalization_input)

      r6$valid_val_filter <- input$valid_values_input
      r6$valid_val_thr <- as.numeric(input$valid_values_slider) / 100
      r6$norm_methods <- input$normalization_input
      
      r6$pep_filter <- input$peptides_input
      r6$pep_thr <- input$peptides_slider
      r6$rev <- input$rev
      r6$cont <- input$cont
      r6$oibs <- input$oibs

      r6$data_wrangling(
        valid_val_filter = r6$valid_val_filter,
        valid_val_thr = r6$valid_val_thr,
        pep_filter = r6$pep_filter,
        pep_thr = r6$pep_thr,
        rev = r6$rev,
        cont = r6$cont,
        oibs = r6$oibs
      )
      
      r6$normalization(norm_methods = r6$norm_methods)
      
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
      
      if(r6$norm_methods == "none"){
        data <- r6$filtered_data
      }else{
        data <- r6$normalized_data
      }
      
      r6$print_table(data)
      
    })
    
  })
}