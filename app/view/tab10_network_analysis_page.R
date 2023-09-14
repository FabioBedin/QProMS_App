box::use(
  shiny[moduleServer, NS, fluidRow, div, column, br, h4, h1, p, observeEvent, observe, uiOutput, renderUI, selectInput, updateSelectInput, updateSliderInput, req, isolate, sliderInput, reactive, icon, reactiveVal],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, bs4Callout, boxLabel, toast, accordion, accordionItem, updateAccordion],
  shinyWidgets[actionBttn, pickerInput, prettyCheckbox, updatePrettyCheckbox, updatePickerInput],
  gargoyle[init, watch, trigger],
  shinyGizmo[conditionalJS, jsCalls],
  echarts4r[echarts4rOutput, renderEcharts4r],
  dplyr[filter, `%>%`, pull, distinct, case_when, if_all, ends_with],
  waiter[Waiter, spin_5],
  tibble[rowid_to_column],
  reactable[reactableOutput, renderReactable, reactable, colDef, getReactableState],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "network",
    fluidRow(
      valueBoxOutput(ns("n_nodes"), width = 6),
      valueBoxOutput(ns("n_edges"), width = 6)
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
                selectInput(
                  inputId = ns("from_statistic_input"),
                  label = "Genes from",
                  choices = c(
                    "Univariate" = "univariate",
                    "Multivariate" = "multivariate",
                    "Top rank proteins" = "top_rank"
                  ),
                  selected = "univariate", 
                  width = "auto"
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                conditionalJS(
                  selectInput(
                    inputId = ns("test_uni_input"),
                    label = "Contrasts",
                    choices = NULL,
                    selected = NULL, 
                    width = "auto"
                  ),
                  condition = "input.from_statistic_input == 'univariate'",
                  jsCall = jsCalls$show(),
                  ns = ns
                ),
                conditionalJS(
                  selectInput(
                    inputId = ns("ui_direction_input"),
                    label = "Directions",
                    choices = c("Up" = "up", "Down" = "down", "Both" = "both"),
                    selected = "up", 
                    width = "auto"
                  ),
                  condition = "input.from_statistic_input == 'univariate'",
                  jsCall = jsCalls$show(),
                  ns = ns
                ),
                conditionalJS(
                  pickerInput(
                    inputId = ns("clusters_input"),
                    label = "Clusters",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(
                      `live-search` = TRUE,
                      title = "None",
                      `selected-text-format` = "count > 2",
                      size = 5)
                  ),
                  condition = "input.from_statistic_input == 'multivariate'",
                  jsCall = jsCalls$show(),
                  ns = ns
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                pickerInput(
                  inputId = ns("db_source"),
                  label = "Database",
                  choices = c("String" = "string", "Corum" = "corum"),
                  selected = "string",
                  multiple = TRUE,
                  options = list(title = "None")
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
            inputId = ns("generate_network"),
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
    fluidRow(column(
      7,
      box(
        title = "Network",
        status = "primary",
        width = 12,
        height = 1000,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = ns("network_sidebar"),
          div(
            style = "padding-right: 0.5rem",
            h4("Network parameters"),
            sliderInput(
              inputId = ns("score_thr"),
              label = "Score threshold",
              min = 0,
              max = 0.9,
              value = 0.4,
              step = 0.1,
              width = "100%"
            ), 
            h4("Layout parameters"),
            div(
              style = "display: flex; justify-content: center; align-items: center; gap: 20px",
              div(
                style = "width: 100%; flex: 2 1 0;",
                selectInput(
                  inputId = ns("layout"),
                  label = NULL,
                  choices = c("force", "circular"),
                  selected = "force",
                  width = "100%"
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                prettyCheckbox(
                  inputId = ns("isolate_nodes_input"),
                  label = "Keep isolate nodes", 
                  value = FALSE,
                  shape = "curve", 
                  width = "auto"
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                prettyCheckbox(
                  inputId = ns("names_input"),
                  label = "Show names", 
                  value = TRUE,
                  shape = "curve", 
                  width = "auto"
                )
              )
            ),
            h4("Subset network"),
            div(
              style = "width: 100%; flex: 1 1 0;",
              prettyCheckbox(
                inputId = ns("keep_selected"),
                label = "Display network with only selected nodes",
                value = FALSE,
                shape = "curve",
                width = "auto"
              )
            ),
            div(
              style = "display: flex; justify-content: center; align-items: center; gap: 20px",

              
            ),
            br(),
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
        echarts4rOutput(ns("network_plot"), height = "975")
      )
    ),
    column(5,
           fluidRow(
             box(
               title = "Nodes table",
               status = "primary",
               width = 12,
               height = 466,
               maximizable = TRUE,
               div(
                 style = "position: absolute; z-index: 999; left: 30px",
                 actionBttn(
                   inputId = ns("save_selected"),
                   label = "Save selected nodes for ORA",
                   style = "bordered",
                   color = "primary",
                   size = "sm"
                 )
               ),
               reactableOutput(ns("table"))
             )
           ),
           fluidRow(
             box(
               title = "Edges table",
               status = "primary",
               width = 12,
               height = 466,
               maximizable = TRUE,
               label = boxLabel("Score info", "info", "The STRINGscore is derived from a combination of both the experimental score and the database score.\nThe CORUMscore indicates the total number of complexes that involve the interaction between two given nodes."),
               reactableOutput(ns("table_edges"))
             )
           )
          )
    )
  )
  
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    w <- Waiter$new(html = spin_5(), color = "#adb5bd")
    
    init("ppi_network")
    
    observe({
      
      watch("stat")
      
      updateSelectInput(
        inputId = "test_uni_input",
        selected = r6$primary_condition,
        choices = c(r6$primary_condition, r6$additional_condition)
      )
      
    })
    
    observe({
      
      watch("anova")
      
      if (!is.null(r6$anova_table)) {
        cluster_names <- r6$anova_table %>% 
          filter(significant) %>% 
          distinct(cluster) %>% 
          pull(cluster)
        
        updatePickerInput(
          session = session,
          inputId = "clusters_input",
          selected = cluster_names[1],
          choices = cluster_names
        )
      }
      
    })
    
    output$n_nodes <- renderValueBox({
      
      watch("ppi_network")
      
      if(is.null(nodes_box())){
      value <- "Undefinded"
      }else{
        value <- nodes_box()
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("adjust", verify_fa = FALSE),
        color = "primary",
        footer = p("N° of nodes", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
    })
    
    output$n_edges <- renderValueBox({
      
      watch("ppi_network")
      
      if(is.null(edges_box())){
        value <- "Undefinded"
      }else{
        value <- edges_box()
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("adjust", verify_fa = FALSE),
        color = "primary",
        footer = p("N° of edges", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
    })
    
    observe({
      
      watch("params")
      
      updatePickerInput(session = session, inputId = "db_source", selected = r6$pdb_database)
      updateSliderInput(inputId = "score_thr", value = r6$network_score_thr)
      
    })
    
    observeEvent(input$generate_network, {
      
      req(input$from_statistic_input)
      req(input$db_source)
      
      r6$network_from_statistic <- input$from_statistic_input
      r6$pdb_database <- input$db_source
      r6$network_uni_direction <- input$ui_direction_input
      
      if (r6$network_from_statistic == "univariate") {
        
        if(is.null(r6$stat_table)) {
          input_error <- "You need to perform Univariate statistics in order to run this analysis."
        } else {
          input_error <- case_when(
            nrow(filter(
              r6$stat_table, if_all(ends_with("significant"))
            )) == 0 ~ "There are no significant, no network analysis will be performed",
            TRUE ~ ""
          )
        }
        
        r6$network_focus_uni <- input$test_uni_input
        focus_net <- r6$network_focus_uni
        
      } else if (r6$network_from_statistic == "multivariate") {
        
        if(is.null(r6$anova_table)) {
          input_error <- "You need to perform Multivariate statistics in order to run this analysis."
        } else {
          input_error <- case_when(
            nrow(filter(r6$anova_table, significant)) == 0 ~ "There are no significant, no network analysis will be performed",
            TRUE ~ ""
          )
        }
        
        r6$network_focus_multi <- input$clusters_input
        focus_net <- r6$network_focus_multi
          
      } else {
        focus_net <- NULL
        if(is.null(r6$protein_rank_list) | length(r6$protein_rank_list) == 0) {
          input_error <- "You need to select at least some proteins in order to run this analysis."
        } else {
          input_error <- ""
        }
      }
      
      if (input_error != "") {
        toast(
          title = "No significant",
          body = input_error,
          options = list(
            class = "bg-danger",
            autohide = TRUE,
            delay = 5000,
            icon = icon("exclamation-circle", verify_fa = FALSE)
          )
        )
        return() 
      }
      
      w$show()
      
      r6$make_nodes(list_from = r6$network_from_statistic, focus = focus_net, direction = r6$network_uni_direction)
      r6$make_edges(source = r6$pdb_database)
      
      trigger("ppi_network") 
      
      updateAccordion(id = "advance_params", selected = NULL)
      
      w$hide()
      
    })
    
    observe({
      
      watch("ppi_network")
      
      updatePrettyCheckbox(
        session = session,
        inputId = "keep_selected",
        value = FALSE
      )
      
    })
    
    observeEvent(input$update, {
      
      req(input$generate_network)
      
      trigger("ppi_network")
      
    })
    
    gene_selected <- reactive(getReactableState("table", "selected"))
    
    nodes_box <- reactiveVal(NULL)
    edges_box <- reactiveVal(NULL)
    
    output$table <- renderReactable({
      
      watch("ppi_network")
      
      req(input$generate_network)
      
      if (!is.null(r6$nodes_table)) {
        
        r6$network_score_thr <- isolate(input$score_thr)
        
        nodes <- r6$print_nodes(
          isolate_nodes = isolate(input$isolate_nodes_input),
          score_thr = r6$network_score_thr
        )
        
        nodes_box(nrow(nodes))
        
        reactable(
          nodes,
          searchable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          wrap = FALSE,
          paginateSubRows = TRUE,
          height = "auto",
          defaultPageSize = 7,
          selection = "multiple",
          # defaultSelected = selected_rowid,
          onClick = "select",
          columns = list(
            gene_names = colDef(name = "Node"),
            category = colDef(name = "Category"),
            p_val = colDef(align = "center", name = "-log(p.value)"),
            p_adj = colDef(align = "center", name = "-log(p.adj)")
          )
        )
        
      }
      
     
      
    })
    
    output$table_edges <- renderReactable({
      
      watch("ppi_network")
      
      req(input$generate_network)
      
      if (!is.null(r6$nodes_table)) {
        
        r6$network_score_thr <- isolate(input$score_thr)
        
        nodes <- r6$print_nodes(
          isolate_nodes = isolate(input$isolate_nodes_input),
          score_thr = r6$network_score_thr
        )
        
        highlights <- nodes[gene_selected(), ] %>% 
          pull(gene_names)
        
        edges <- r6$print_edges(score_thr = r6$network_score_thr, selected_nodes = highlights)
        
        edges_box(nrow(edges))
        
        reactable(
          edges,
          searchable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          wrap = FALSE,
          height = "auto",
          defaultPageSize = 7,
          columns = list(
            target = colDef(aggregate = "unique", minWidth = 100, name = "Target"),
            source = colDef(name = "Source"),
            database = colDef(name = "Database"),
            complex = colDef(minWidth = 300, name = "Complex"),
            score = colDef(align = "center", name = "Score")
          )
        )
      }
      
      
    })
    
    output$network_plot <- renderEcharts4r({
      
      watch("ppi_network")
      
      req(input$generate_network)
      
      r6$network_score_thr <- isolate(input$score_thr)
      
      if (!is.null(r6$nodes_table)) {
        nodes <- r6$print_nodes(
          isolate_nodes = isolate(input$isolate_nodes_input),
          score_thr = r6$network_score_thr
        )
        
        highlights <- nodes[gene_selected(), ] %>% 
          pull(gene_names)
        
        fil <- isolate(input$keep_selected)
        
        if(length(highlights) == 0){
          highlights <- NULL
          fil <- FALSE
        }
        
        r6$plot_ppi_network(
          list_from = r6$network_from_statistic,
          score_thr = r6$network_score_thr,
          isolate_nodes = isolate(input$isolate_nodes_input),
          layout = isolate(input$layout),
          show_names = isolate(input$names_input),
          selected = highlights,
          filtred = fil
        )
      }
    })
    
    
    observeEvent(input$save_selected, {
      
      watch("ppi_network")
      
      req(input$generate_network)
      
      nodes <- r6$print_nodes(
        isolate_nodes = isolate(input$isolate_nodes_input),
        score_thr = r6$network_score_thr
      )
      
      r6$selected_nodes <- nodes[gene_selected(), ] %>% 
        pull(gene_names)
      
      toast(
        title = "Selected nodes saved!",
        body = "Now you can use them for funtional analysis.",
        options = list(
          class = "bg-success",
          autohide = TRUE,
          delay = 5000,
          icon = icon("check")
        )
      )
      
    })

  })
}
