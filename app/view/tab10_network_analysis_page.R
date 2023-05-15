box::use(
  shiny[moduleServer, NS, fluidRow, div, column, br, h4, observeEvent, uiOutput, renderUI, selectInput, req, isolate, sliderInput, reactive, icon],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, bs4Callout, boxLabel, toast],
  shinyWidgets[actionBttn, pickerInput, prettyCheckbox],
  gargoyle[init, watch, trigger],
  shinyGizmo[conditionalJS, jsCalls],
  echarts4r[echarts4rOutput, renderEcharts4r],
  dplyr[filter, `%>%`, pull, distinct],
  waiter[Waiter, spin_5],
  r3dmol[r3dmol, r3dmolOutput, renderR3dmol, m_add_model, m_fetch_pdb, m_set_style, m_style_cartoon, m_style_stick, m_zoom_to],
  reactable[reactableOutput, renderReactable, reactable, colDef, getReactableState],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "network",
    fluidRow(
      valueBoxOutput(ns("string_thr"), width = 4),
      valueBoxOutput(ns("n_nodes"), width = 4),
      valueBoxOutput(ns("n_edges"), width = 4)
    ),
    fluidRow(
      bs4Callout(
        div(
          style = "display: flex; justify-content: center; align-items: center; gap: 20px",
          div(
            style = "width: 100%; flex: 1 1 0;",
            uiOutput(ns("ui_from_statistic_input"))
          ),
          conditionalJS(
            div(
              style = "width: 100%; flex: 1 1 0;",
              uiOutput(ns("ui_test_input"))
            ),
            condition = "input.from_statistic_input == 'univariate'",
            jsCall = jsCalls$show(),
            ns = ns
          ),
          conditionalJS(
            div(
              style = "width: 100%; flex: 1 1 0;",
              selectInput(
                inputId = ns("ui_direction_input"),
                label = "Directions",
                choices = c("Up" = "up", "Down" = "down", "Both" = "both"),
                selected = "up", 
                width = "auto"
              )
            ),
            condition = "input.from_statistic_input == 'univariate'",
            jsCall = jsCalls$show(),
            ns = ns
          ),
          conditionalJS(
            div(
              style = "width: 100%; flex: 1 1 0;",
              uiOutput(ns("ui_cluster_input"))
            ),
            condition = "input.from_statistic_input == 'multivariate'",
            jsCall = jsCalls$show(),
            ns = ns
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
          ),
          div(
            style = "width: 100%; flex: 1 1 0;",
            actionBttn(
              inputId = ns("generate_network"),
              label = "Generate network",
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE,
              width = "auto"
            )
          )
        ),
        title = NULL,
        status = "info",
        width = 12,
        elevation = 1
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
              style = "display: flex; justify-content: center; align-items: center; gap: 20px",
              div(
                style = "width: 100%; flex: 1 1 0;",
                prettyCheckbox(
                  inputId = ns("keep_selected"),
                  label = "Only selected", 
                  value = FALSE,
                  shape = "curve", 
                  width = "auto"
                )
              ),
              div(
                style = "width: 100%; flex: 2 1 0;",
                actionBttn(
                  inputId = ns("save_selected"),
                  label = "Save selected nodes",
                  style = "bordered",
                  color = "primary",
                  size = "md"
                  # block = TRUE
                )
              )
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
        echarts4rOutput(ns("network_plot"), height = "900")
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
               label = boxLabel("Score info", "info", "The STRINGscore is derived from a combination of both the experimental score and the database score.\nThe CORUMscore indicates the total number of complexes that involve the interaction between two given nodes."),
               # sidebar = boxSidebar(
               #   id = ns("table_sidebar"),
               #   div(
               #     style = "padding-right: 0.5rem",
               #     h4("Download table"),
               #     actionBttn(
               #       inputId = ns("save_selected"),
               #       label = "Save selected nodes",
               #       style = "material-flat",
               #       color = "primary",
               #       size = "md",
               #       block = TRUE
               #     )
               #   )
               # ),
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
               r3dmolOutput(ns("protein_structure"))
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
    
    output$ui_from_statistic_input <- renderUI({
      
      watch("ppi_network")
      watch("stat")
      watch("anova")
      
      if(is.null(r6$stat_table) & is.null(r6$anova_table)) {
        scelte <- c("External table" = "external")
        sel <- "external"
      } else if (!is.null(r6$stat_table) & is.null(r6$anova_table)) {
        scelte <- c("Univariate" = "univariate", "External table" = "external")
        sel <- input$from_statistic_input
      } else if (is.null(r6$stat_table) & !is.null(r6$anova_table)) {
        scelte <- c("Multivariate" = "multivariate", "External table" = "external")
        sel <- input$from_statistic_input
      } else {
        scelte <- c("Univariate" = "univariate", "Multivariate" = "multivariate", "External table" = "external")
        sel <- input$from_statistic_input
      }
      
      selectInput(
        inputId = session$ns("from_statistic_input"),
        label = "Genes from",
        choices = scelte,
        selected = sel, 
        width = "auto"
      )
      
    })
    
    output$ui_test_input <- renderUI({
      
      watch("ppi_network")
      watch("stat")
      
      tests <- c(r6$primary_condition, r6$additional_condition)
      
      selectInput(
        inputId = session$ns("test_uni_input"),
        label = "Contrasts",
        choices = tests,
        selected = r6$primary_condition, 
        width = "auto"
      )
      
    })
    
    output$ui_cluster_input <- renderUI({
      
      watch("ppi_network")
      watch("anova")
      
      if (is.null(r6$anova_table)) {
        cluster_names <- NULL
        sel <- NULL
      } else {
        cluster_names <- r6$anova_table %>% 
          filter(significant) %>% 
          distinct(cluster) %>% 
          pull(cluster)
        if(r6$network_focus[1] %in% cluster_names){
          sel <- r6$network_focus
        } else {
          sel <- "cluster_1"
        }
      }
      
      pickerInput(
        inputId = session$ns("clusters_input"),
        label = "Clusters",
        choices = cluster_names,
        selected = sel,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          title = "None",
          `selected-text-format` = "count > 2",
          size = 5)
      )
      
      
    })
    
    observeEvent(input$generate_network, {
      
      w$show()
      
      req(input$from_statistic_input)
      req(input$db_source)
      
      if (input$from_statistic_input == "univariate") {
        r6$network_focus <- input$test_uni_input
      } else if (input$from_statistic_input == "multivariate") {
        r6$network_focus <- input$clusters_input
      } else {
        r6$network_focus <- NULL # da sistemare
      }
      
      r6$network_from_statistic <- input$from_statistic_input
      
      r6$make_nodes(list_from = r6$network_from_statistic, focus = r6$network_focus, direction = input$ui_direction_input)
      r6$make_edges(source = input$db_source)
      
      trigger("ppi_network") 
      
      w$hide()
      
    })
    
    observeEvent(input$update, {
      
      req(input$generate_network)
      
      # if (input$keep_selected) {
      #   
      # }
      
      trigger("ppi_network")
      
    })
    
    gene_selected <- reactive(getReactableState("table", "selected"))
    
    output$table <- renderReactable({
      
      watch("ppi_network")
      
      req(input$generate_network)
      
      # r6$print_edges(score_thr = isolate(input$score_thr))
      nodes <- r6$print_nodes(
        isolate_nodes = isolate(input$isolate_nodes_input),
        score_thr = isolate(input$score_thr)
      )
      
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
        onClick = "select",
        columns = list(
          gene_names = colDef(name = "Node"),
          category = colDef(name = "Category"),
          p_val = colDef(align = "center", name = "-log(p.value)"),
          p_adj = colDef(align = "center", name = "-log(p.adj)")
        )
      )
      
    })
    
    output$network_plot <- renderEcharts4r({
      
      watch("ppi_network")
      
      req(input$generate_network)
      
      if(is.null(r6$nodes_table)){
        highlights <- NULL
      }else{
        nodes <- r6$print_nodes(
          isolate_nodes = isolate(input$isolate_nodes_input),
          score_thr = isolate(input$score_thr)
        )
        
        highlights <- nodes[gene_selected(), ] %>% 
          pull(gene_names)
      }
      
      r6$plot_ppi_network(
        list_from = r6$network_from_statistic,
        score_thr = isolate(input$score_thr),
        isolate_nodes = isolate(input$isolate_nodes_input),
        layout = isolate(input$layout),
        show_names = isolate(input$names_input),
        selected = highlights,
        filtred = isolate(input$keep_selected)
      )
    })
    
    output$protein_structure <- renderR3dmol({
      
      watch("ppi_network")
      
      req(input$generate_network)
      
      r3dmol() %>% 
        m_add_model(data = m_fetch_pdb("4zyo")) %>% 
        m_set_style(style = c(m_style_cartoon(), m_style_stick())) %>% 
        m_zoom_to()
      
    })
    
    observeEvent(input$save_selected, {
      
      watch("ppi_network")
      
      req(input$generate_network)
      
      nodes <- r6$print_nodes(
        isolate_nodes = isolate(input$isolate_nodes_input),
        score_thr = isolate(input$score_thr)
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
