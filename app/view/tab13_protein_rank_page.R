box::use(
  shiny[moduleServer, NS, fluidRow, column, icon, h3, selectInput, div, h4, p, plotOutput, sliderInput, renderPlot, observeEvent, req, reactiveVal, uiOutput, renderUI, isolate, reactive],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, boxLabel, accordion, accordionItem],
  echarts4r[echarts4rOutput, renderEcharts4r, e_show_loading],
  shinyWidgets[actionBttn, prettyCheckbox],
  stringr[str_to_title],
  reactable[reactableOutput, renderReactable, reactable, colDef, getReactableState],
  dplyr[`%>%`, select, distinct, pull],
  gargoyle[init, watch, trigger],
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
                style = "width: 100%; flex: 1 1 0;",
                div(
                  style = "display: flex; justify-content: center; align-items: center; gap: 20px",
                  div(
                    style = "width: 100%; flex: 3 1 0;",
                    uiOutput(ns("target_ui"))
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
                sliderInput(
                  inputId = ns("top_n_slider"),
                  label = "Select top n % proteins",
                  min = 0,
                  max = 50,
                  value = 10,
                  step = 1
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
        reactableOutput(ns("table"))
      )
    )
  )
  
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    init("plot", "boxes", "check")
    
    output$n_highlights <- renderValueBox({

      watch("boxes")

      total <- nrow(r6$imputed_data)

      value <- paste0(length(r6$protein_rank_list), " out of ", total)
      
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

      watch("boxes")

      value <- round(max(r6$imputed_data$intensity, na.rm = TRUE), 2)

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

      value <- round(min(r6$imputed_data$intensity, na.rm = TRUE), 2)

      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("arrow-down"),
        color = "primary",
        footer = p("Min intensity value", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )

    })
    
    observeEvent(input$by_cond_input, {
      
      r6$protein_rank_by_cond <- input$by_cond_input
      
      trigger("check")
      
    })
    
    output$target_ui <- renderUI({
      
      watch("check")
      
      if(r6$protein_rank_by_cond){
        scelte <- r6$expdesign %>% 
          select(condition) %>% 
          distinct() %>% 
          pull()
        
        sel <- scelte[1]
      } else {
        scelte <- r6$expdesign %>% 
          select(label) %>% 
          distinct() %>% 
          pull()
        
        sel <- scelte[1]
      }
      
      
      selectInput(
        inputId = session$ns("target"),
        label = "Genes from",
        choices = scelte,
        selected = sel, 
        width = "auto"
      )
      
    })
    
    
    observeEvent(input$update ,{

      req(input$target)
      req(input$top_n_slider)
      
      r6$protein_rank_by_cond <- input$by_cond_input
      r6$protein_rank_target <- input$target
      r6$protein_rank_top_n <- as.numeric(input$top_n_slider) / 100

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
        defaultPageSize = 12,
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
    
    output$protein_rank_plot <- renderEcharts4r({
      
      watch("plot")
      
      if(is.null(r6$protein_rank_target)){
        r6$plot_protein_rank(
          target = r6$expdesign$label[1],
          by_condition = FALSE,
          top_n = 0.1,
          highlights_names = NULL
        )
      } else {
        r6$plot_protein_rank(
          target = r6$protein_rank_target,
          by_condition = r6$protein_rank_by_cond,
          top_n = r6$protein_rank_top_n,
          highlights_names = NULL
        )
      }
      
      
      
    })
    
    
  })
}