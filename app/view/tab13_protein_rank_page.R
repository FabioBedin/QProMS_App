box::use(
  shiny[moduleServer, NS, fluidRow, column, icon, h3, selectInput, div, h4, p, plotOutput, sliderInput, renderPlot, observeEvent, req, reactiveVal, uiOutput, renderUI, isolate, reactive, observe, updateSelectInput, updateSliderInput],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, boxLabel, accordion, accordionItem],
  echarts4r[echarts4rOutput, renderEcharts4r, e_show_loading],
  shinyWidgets[actionBttn, prettyCheckbox, updatePrettyCheckbox],
  stringr[str_to_title],
  reactable[reactableOutput, renderReactable, reactable, colDef, getReactableState],
  dplyr[`%>%`, select, distinct, pull, mutate],
  gargoyle[init, watch, trigger],
  shinyGizmo[conditionalJS, jsCalls],
  waiter[Waiter, spin_5],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "protein_rank",
    fluidRow(
      valueBoxOutput(ns("n_highlights"), width = 4),
      valueBoxOutput(ns("max_intensity"), width = 4),
      valueBoxOutput(ns("min_intensity"), width = 4)
    ),
    fluidRow(
      column(
        width = 11,
        accordion(
          id = ns("advance_params"),
          accordionItem(
            title = "Parameters",
            status = "primary",
            collapsed = FALSE,
            solidHeader = TRUE,
            div(
              style = "display: flex; justify-content: center; gap: 5rem; align-items: start;",
              div(
                style = "width: 100%; flex: 2 1 0;",
                div(
                  style = "display: flex; justify-content: center; align-items: center; gap: 20px",
                  div(
                    style = "width: 100%; flex: 3 1 0;",
                    selectInput(
                      inputId = ns("target"),
                      label = "Genes from",
                      choices = NULL,
                      selected = NULL, 
                      width = "auto"
                    )
                  ),
                  div(
                    style = "width: 100%; flex: 1 1 0; text-align: center; margin-top: 1.5rem;",
                    prettyCheckbox(
                      inputId = ns("by_cond_input"),
                      label = "Use mean of condition", 
                      value = FALSE,
                      shape = "curve", 
                      width = "auto"
                    )
                  )
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                selectInput(
                  inputId = ns("selections"),
                  label = "Protein selection",
                  choices = c("From top" = "top", "From bottom" = "bot", "Manual" = "manual"),
                  selected = "top", 
                  width = "auto"
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                conditionalJS(
                  sliderInput(
                    inputId = ns("top_n_slider"),
                    label = "n % of proteins",
                    min = 0,
                    max = 50,
                    value = 10,
                    step = 1
                  ),
                  condition = "input.selections != 'manual'",
                  jsCall = jsCalls$show(),
                  ns = ns
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
            label = "Start", 
            style = "material-flat",
            color = "success",
            size = "md",
            block = TRUE,
            width = "auto"
          ) 
        )
      )
    ),
    fluidRow(
      box(
        title = "Protein Rank plot",
        status = "primary",
        width = 6,
        height = 700,
        maximizable = TRUE,
        echarts4rOutput(ns("protein_rank_plot"), height = "650")
      ),
      box(
        title = "Table",
        status = "primary",
        width = 6,
        height = 700,
        maximizable = TRUE,
        conditionalJS(
          div(
            style = "position: absolute; z-index: 999; left: 30px",
            actionBttn(
              inputId = ns("save_selected"),
              label = "Save selected proteins",
              style = "bordered",
              color = "primary",
              size = "sm",
              block = TRUE
            )
          ),
          condition = "input.selections == 'manual'",
          jsCall = jsCalls$show(),
          ns = ns
        ),
        label = boxLabel("Auto save!", "info", "The top n % selected protein are saved for downstream analysis. You can use it for network and functional analysis."),
        reactableOutput(ns("table"))
      )
    )
  )
  
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    w <- Waiter$new(html = spin_5(), color = "#adb5bd")
    
    init("check", "rank")
    
    output$n_highlights <- renderValueBox({

      watch("rank")
      
      if(is.null(r6$rank_data)){
        value <- "Undefinded"
      }else{
        total <- nrow(r6$rank_data)
        
        value <- paste0(length(r6$protein_rank_list), " out of ", total)
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("database"),
        color = "primary",
        footer = p("Number of selected proteins", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )

    })

    output$max_intensity <- renderValueBox({

      watch("rank")
      
      if(is.null(r6$rank_data)){
        value <- "Undefinded"
      }else{
        value <- round(max(r6$rank_data$intensity, na.rm = TRUE), 2)
      }

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

      watch("rank")
      
      if(is.null(r6$rank_data)){
        value <- "Undefinded"
      }else{
        value <- round(min(r6$rank_data$intensity, na.rm = TRUE), 2)
      }

      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("arrow-down"),
        color = "primary",
        footer = p("Min intensity value", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )

    })
    
    observe({
      
      watch("ui_element")
      watch("check")
      
      if(!is.null(r6$expdesign)) {
        if(r6$protein_rank_by_cond){
          scelte <- r6$expdesign %>% 
            select(condition) %>% 
            distinct() %>% 
            pull()
        } else {
          scelte <- r6$expdesign %>% 
            select(label) %>% 
            distinct() %>% 
            pull()
        }
        
        updateSelectInput(inputId = "target", choices = scelte, selected = scelte[1])
      }
      
    })
    
    observeEvent(input$by_cond_input, {
      
      r6$protein_rank_by_cond <- input$by_cond_input
      
      trigger("check")
      
    })
    
    observe({
      
      watch("params")
      
      updateSelectInput(inputId = "target", selected = r6$protein_rank_target)
      updatePrettyCheckbox(inputId = "by_cond_input", value = r6$protein_rank_by_cond)
      updateSliderInput(inputId = "top_n_slider", value =  r6$protein_rank_top_n * 100)
      
    })
    
    
    observeEvent(input$update ,{
      
      w$show()

      req(input$target)
      req(input$top_n_slider)
      
      r6$protein_rank_target <- input$target
      r6$protein_rank_by_cond <- input$by_cond_input
      r6$protein_rank_selection <- input$selections
      r6$protein_rank_top_n <- as.numeric(input$top_n_slider) / 100
      
      r6$rank_protein(
        target = r6$protein_rank_target,
        by_condition = r6$protein_rank_by_cond,
        selection = r6$protein_rank_selection,
        n_perc = r6$protein_rank_top_n
      )

      trigger("rank")
      
      w$hide()

    })

    output$table <- renderReactable({

      watch("rank")
      
      req(input$update)
      
      data <- r6$rank_data %>% 
        mutate(intensity = round(intensity, 2))

      reactable(
        data,
        searchable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        height = "auto",
        selection = "multiple",
        defaultPageSize = 12,
        onClick = "select",
        defaultSelected = 1,
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
    
    output$protein_rank_plot <- renderEcharts4r({
      
      watch("rank")
      
      req(input$update)
      
      highlights <- r6$rank_data[gene_selected(),] %>%
        pull(gene_names)
      
      r6$plot_protein_rank(highlights_names = highlights)
      
    })
    
    observeEvent(input$save_selected, {
      highlights <- r6$rank_data[gene_selected(), ] %>%
        pull(gene_names)
      
      r6$protein_rank_list <- highlights
      
    })
    
    
  })
}