box::use(
  shiny[moduleServer, NS, fluidRow, div, selectInput, uiOutput, numericInput, h4, br, icon, p, renderUI, observeEvent, isolate, req, reactive, reactiveVal, downloadHandler],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, bs4Callout],
  shinyWidgets[actionBttn, prettyCheckbox, pickerInput, sliderTextInput, downloadBttn],
  gargoyle[init, watch, trigger],
  reactable[reactableOutput, renderReactable, reactable, colDef, getReactableState],
  dplyr[group_by, `%>%`, pull, slice_max],
  tibble[rowid_to_column],
  echarts4r[echarts4rOutput, renderEcharts4r, echarts4rProxy, e_focus_adjacency_p],
  waiter[Waiter, spin_5],
  shinyGizmo[conditionalJS, jsCalls],
  utils[write.csv]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "functional_analysis_ora",
    fluidRow(
      valueBoxOutput(ns("significant"), width = 3),
      valueBoxOutput(ns("fdr_thr"), width = 3),
      valueBoxOutput(ns("n_nodes"), width = 3),
      valueBoxOutput(ns("n_edges"), width = 3)
    ),
    fluidRow(
      bs4Callout(
        div(
          style = "display: flex; justify-content: center; gap: 20px; align-items: center;",
          div(
            style = "width: 100%; flex: 1 1 0;",
            uiOutput(ns("ui_from_statistic_input"))
          ),
          conditionalJS(
            div(
              style = "width: 100%; flex: 1 1 0;",
              uiOutput(ns("ui_groups_input"))
            ),
            condition = "input.from_statistic_input == 'univariate'",
            jsCall = jsCalls$show(),
            ns = ns
          ),
          div(
            style = "width: 100%; flex: 1 1 0;",
            selectInput(
              inputId = ns("alpha_input"),
              label = "Alpha",
              choices = c(0.05, 0.01),
              selected = 0.05, 
              width = "auto"
            )
          ),
          div(
            style = "width: 100%; flex: 1 1 0;",
            selectInput(
              inputId = ns("truncation_input"),
              label = "Truncation",
              choices = c(
                "Benjamini & Hochberg" = "BH",
                "Bonferroni" = "bonferroni",
                "Holm (1979)" = "holm",
                "Hochberg (1988)" = "hochberg",
                "Hommel (1988)" = "hommel",
                "Benjamini & Yekutieli" = "BY",
                "None" = "none"),
              selected = "BH", 
              width = "auto"
            )
          ),
          div(
            style = "width: 100%; flex: 1 1 0; text-align: center; margin-top: 1.5rem;",
            prettyCheckbox(
              inputId = ns("background_input"),
              label = "Background", 
              value = FALSE,
              shape = "curve", 
              width = "auto"
            )
          ),
          div(
            style = "width: 100%; flex: 1 1 0;",
            actionBttn(
              inputId = ns("run_analysis"),
              label = "Run analysis", 
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
    fluidRow(
      box(
        title = "Functional plot",
        status = "primary",
        width = 6,
        height = 700,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = ns("functional_sidebar"),
          div(
            style = "padding-right: 0.5rem",
            h4("Parameters"),
            div(
              style = "display: flex; justify-content: center; align-items: center; gap: 20px",
              div(
                style = "width: 100%; flex: 1 1 0;",
                selectInput(
                  inputId = ns("ontology"),
                  label = "Term",
                  choices = c("BP", "MF", "CC"),
                  selected = "BP"
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                numericInput(
                  inputId = ns("top_n_input"),
                  label = "Top n",
                  value = 10,
                  min = 1,
                  max = 15,
                  step = 1
                )
              ),
              conditionalJS(
                div(
                  style = "width: 100%; flex: 1 1 0;  text-align: center;",
                  uiOutput(ns("ui_direction_input"))
                ),
                condition = "input.from_statistic_input == 'univariate'",
                jsCall = jsCalls$show(),
                ns = ns
              ),
              conditionalJS(
                div(
                  style = "width: 100%; flex: 1 1 0;  text-align: center;",
                  uiOutput(ns("ui_cluster_input"))
                ),
                condition = "input.from_statistic_input == 'multivariate'",
                jsCall = jsCalls$show(),
                ns = ns
              ),
            ),
            sliderTextInput(
              inputId = ns("simplify_thr"), 
              label = "Reduce redundancy in GO terms",
              choices = c("highly simplified", "2", "3", "4", "5", "6", "7", "8", "9", "not simplified"), 
              selected = "7",
              grid = TRUE,
              force_edges = TRUE,
            ),
            selectInput(
              inputId = ns("plot_value_input"),
              label = "Genes from",
              choices = c("Fold change" = "fold_change", "p.value" = "pvalue", "p.ajdust" = "p.ajdust", "q.value" = "qvalue"),
              selected = "fold_change"
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
        echarts4rOutput(ns("functional_plot"), height = "650")
      ),
      box(
        title = "Network",
        status = "primary",
        width = 6,
        height = 700,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = ns("network_sidebar"),
          div(
            style = "padding-right: 0.5rem",
            h4("Parameters"),
            div(
              style = "display: flex; justify-content: center; align-items: center; gap: 20px",
              div(
                style = "width: 100%; flex: 3 1 0;",
                selectInput(
                  inputId = ns("layout"),
                  label = "Layout",
                  choices = c("force", "circular"),
                  selected = "force"
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0; text-align: center; margin-top: 1.5rem;",
                prettyCheckbox(
                  inputId = ns("names_input"),
                  label = "Show names", 
                  value = FALSE,
                  shape = "curve", 
                  width = "auto"
                )
              )
            ),
            br(),
            actionBttn(
              inputId = ns("update_net"),
              label = "Update", 
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          )
        ),
        echarts4rOutput(ns("network_plot"), height = "650")
      )
    ),
    fluidRow(
      box(
        title = "Result Table",
        status = "primary",
        width = 12,
        maximizable = TRUE,
        collapsible = TRUE, 
        sidebar = boxSidebar(
          id = ns("table_sidebar"),
          div(
            style = "padding-right: 0.5rem",
            h4("Download table"),
            downloadBttn(
              outputId  = ns("download_table"),
              label = "Download", 
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          )
        ),
        reactableOutput(ns("table"))
      )
    )
  )

}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    w <- Waiter$new(html = spin_5(), color = "#adb5bd")
    
    init("functional")
    
    output$ui_from_statistic_input <- renderUI({
      
      watch("functional")
      watch("stat")
      watch("anova")
      
      if(is.null(r6$stat_table) & is.null(r6$anova_table)) {
        scelte <- c("External table" = "external")
        sel <- "external"
      } else if (!is.null(r6$stat_table) & is.null(r6$anova_table)) {
        scelte <- c("Univariate" = "univariate", "External table" = "external", "Selected nodes" = "nodes")
        sel <- "univariate"
      } else if (is.null(r6$stat_table) & !is.null(r6$anova_table)) {
        scelte <- c("Multivariate" = "multivariate", "External table" = "external", "Selected nodes" = "nodes")
        sel <- "multivariate"
      } else {
        scelte <- c("Univariate" = "univariate", "Multivariate" = "multivariate", "External table" = "external", "Selected nodes" = "nodes")
        sel <- "univariate"
      }
      
      selectInput(
        inputId = session$ns("from_statistic_input"),
        label = "Genes from",
        choices = scelte,
        selected = sel, 
        width = "auto"
      )
      
    })
    
    output$ui_groups_input <- renderUI({
      
      watch("functional")
      watch("stat")
      watch("anova")
      
      tests <- c(r6$primary_condition, r6$additional_condition)
      
      pickerInput(
        inputId = session$ns("test_uni_input"),
        label = "Contrasts",
        choices = tests,
        selected = r6$primary_condition,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          title = "None",
          `selected-text-format` = "count > 2",
          size = 5)
      )
      
      
    })
    
    output$ui_direction_input <- renderUI({
      
      watch("functional")
      
      if(is.null(r6$ora_table)) {
        tests <- paste0(r6$primary_condition, "_up")
        sel <- tests
      } else {
        tests <- names(r6$ora_result_list)
        sel <- r6$go_ora_focus
      }
      
      pickerInput(
        inputId = session$ns("direction_input"),
        label = "Contrasts",
        choices = tests,
        selected = sel,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE, 
          title = "None",
          `selected-text-format` = "count > 1",
          size = 5)
      )
      
      
    })
    
    output$ui_cluster_input <- renderUI({
      
      watch("functional")
      
      if(is.null(r6$ora_table)) {
        tests <- "cluster_1"
        sel <- tests
      } else {
        tests <- names(r6$ora_result_list)
        sel <- r6$go_ora_focus
      }
      
      pickerInput(
        inputId = session$ns("cluster_input"),
        label = "Clusters",
        choices = tests,
        selected = sel,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE, 
          title = "None",
          `selected-text-format` = "count > 1",
          size = 5)
      )
      
      
    })
    
    output$significant <- renderValueBox({
      
      watch("functional")
      
      if(is.null(r6$ora_table)){
        value <- 0
      }else{
        sig <- nrow(r6$ora_table)
        
        term <- r6$go_ora_term
        
        value <- paste0(sig, " terms in ", term)
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("adjust", verify_fa = FALSE),
        color = "primary",
        footer = p("Significant", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$fdr_thr <- renderValueBox({
      
      watch("functional")
      
      if(is.null(r6$ora_table)){
        value <- "Undefinded"
      }else{
        value <- r6$go_ora_alpha
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("crosshairs"),
        color = "primary",
        footer = p("FDR", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$n_nodes <- renderValueBox({
      
      watch("functional")
      
      if(is.null(r6$ora_table)){
        value <- "Undefinded"
      }else{
        value <- 12
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("compress"),
        color = "primary",
        footer = p("N° of nodes", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$n_edges <- renderValueBox({
      
      watch("functional")
      
      if(is.null(r6$ora_table)){
        value <- "Undefinded"
      }else{
        value <- 54
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("compress"),
        color = "primary",
        footer = p("N° of edges", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    observeEvent(input$run_analysis ,{
      
      w$show()
      
      req(input$from_statistic_input)
      req(input$test_uni_input)
      req(input$alpha_input)
      req(input$truncation_input)
      req(input$ontology)
      req(input$direction_input)
      req(input$top_n_input)
      req(input$simplify_thr)
      req(input$plot_value_input)
      
      r6$go_ora_from_statistic <- input$from_statistic_input
      r6$go_ora_tested_condition <- input$test_uni_input
      r6$go_ora_alpha <- as.double(input$alpha_input)
      r6$go_ora_p_adj_method <- input$truncation_input
      r6$go_ora_term <- input$ontology
      r6$go_ora_top_n <- input$top_n_input
      r6$go_ora_plot_value <- input$plot_value_input
      
      if (input$from_statistic_input == "univariate") {
        r6$go_ora_focus <- input$direction_input
      } else if (input$from_statistic_input == "multivariate") {
        r6$go_ora_focus <- input$cluster_input
      } else {
        r6$go_ora_focus <- NULL ## da sistemare per external table
      }
      
      if (input$simplify_thr == "highly simplified") {
        simp_thr <- 0.1
      } else if (input$simplify_thr == "not simplified") {
        simp_thr <- 1
      } else {
        simp_thr <- as.numeric(input$simplify_thr) / 10
      }
      r6$go_ora_simplify_thr <- simp_thr
      
      r6$go_ora(
        list_from = r6$go_ora_from_statistic,
        test = r6$go_ora_tested_condition,
        alpha = r6$go_ora_alpha,
        p_adj_method = r6$go_ora_p_adj_method,
        background = isolate(input$background_input)
      )
      
      r6$go_ora_simplify(thr = r6$go_ora_simplify_thr)
      
      r6$print_ora_table(ontology = r6$go_ora_term, groups = r6$go_ora_focus)
      
      trigger("functional")
      
      w$hide()
    })
    
    observeEvent(input$update, {
      
      w$show()
      
      req(input$simplify_thr)
      req(input$ontology)
      req(input$direction_input)
      req(input$top_n_input)
      req(input$plot_value_input)
      
      r6$go_ora_term <- input$ontology
      r6$go_ora_top_n <- input$top_n_input
      r6$go_ora_plot_value <- input$plot_value_input
      
      if (input$from_statistic_input == "univariate") {
        r6$go_ora_focus <- input$direction_input
      } else if (input$from_statistic_input == "multivariate") {
        r6$go_ora_focus <- input$cluster_input
      } else {
        r6$go_ora_focus <- NULL ## da sistemare per external table
      }
      
      if (input$simplify_thr == "highly simplified") {
        simp_thr <- 0.1
      } else if (input$simplify_thr == "not simplified") {
        simp_thr <- 1
      } else {
        simp_thr <- as.numeric(input$simplify_thr) / 10
      }
      
      if (r6$go_ora_simplify_thr != simp_thr) {
        
        r6$go_ora_simplify_thr <- simp_thr
        
        r6$go_ora_simplify(thr = r6$go_ora_simplify_thr)
        
      }
      
      r6$print_ora_table(ontology = r6$go_ora_term, groups = r6$go_ora_focus)
      
      trigger("functional")
      
      w$hide()
      
    })
    
    observeEvent(input$update_net, {
      
      req(input$run_analysis)
      
      trigger("functional")
      
    })
    
    output$table <- renderReactable({
      
      watch("functional")
      
      req(input$run_analysis)
      
      default_selected <- r6$ora_table %>% 
        rowid_to_column() %>% 
        group_by(group) %>% 
        slice_max(get(r6$go_ora_plot_value), n = r6$go_ora_top_n) %>% 
        pull(rowid)
      
      reactable(
        r6$ora_table,
        searchable = TRUE,
        resizable = TRUE,
        wrap = FALSE,
        highlight = TRUE,
        defaultPageSize = 10,
        height = "auto",
        selection = "multiple",
        onClick = "select",
        defaultSelected = default_selected,
        columns = list(
          ONTOLOGY = colDef(align = "center", name = "Ontology"),
          ID = colDef(
            align = "center",
            sticky = "left",
            minWidth = 150,
            style = list(borderRight  = "1px solid #eee")
          ), 
          group = colDef(minWidth = 250),
          fold_change = colDef(minWidth = 150, align = "center", name = "Fold change"),
          Description = colDef(minWidth = 400),
          geneID = colDef(minWidth = 1000),
          GeneRatio = colDef(align = "center", name = "Gene ratio"),
          BgRatio = colDef(align = "center", name = "Bg ratio"),
          pvalue = colDef(align = "center", name = "-log(p.val)"),
          p.adjust = colDef(align = "center", name = "-log(p.adj)"),
          qvalue = colDef(align = "center", name = "-log(q.val)"),
          Count = colDef(align = "center")
        )
      )
      
    })
    
    gene_selected <- reactive(getReactableState("table", "selected"))
    
    output$functional_plot <- renderEcharts4r({
      
      watch("functional")
      
      req(input$run_analysis)
      
      table <- r6$ora_table
      
      if(is.null(gene_selected())) {
        r6$plot_ora_empty(val = r6$go_ora_plot_value)
      } else{
        highlights <- table[gene_selected(),] %>%
          pull(ID)
        
        r6$plot_ora(
          term = r6$go_ora_term,
          groups = r6$go_ora_focus,
          selected = highlights,
          value = r6$go_ora_plot_value
        )
      }
      
    })
    
    output$network_plot <- renderEcharts4r({
      
      watch("functional")
      
      req(input$run_analysis)
      
      table <- r6$ora_table
      
      if(is.null(gene_selected())) {
        r6$plot_ora_empty(val = r6$go_ora_plot_value)
      } else{
        highlights <- table[gene_selected(),] %>%
          pull(ID)
        
        r6$plot_ora_network(
          term = r6$go_ora_term,
          groups = r6$go_ora_focus,
          selected = highlights,
          val = r6$go_ora_plot_value,
          layout = isolate(input$layout), 
          show_names = isolate(input$names_input)
        )
      }
      
    })
    
    # e_focus <- reactiveVal(value = NULL)
    
    observeEvent(input$functional_plot_mouseover_data, {
      # print(input$functional_plot_mouseover_row)
      # e_focus(input$functional_plot_mouseover_data_value$name[1])
      
      echarts4rProxy(session$ns("network_plot")) %>% 
        e_focus_adjacency_p(
          seriesIndex = 0,
          index = input$functional_plot_mouseover_row
        )
      
    })
    
    output$download_table <- downloadHandler(
      filename = function() {
        paste("ora_table", ".csv", sep="")
      },
      content = function(file) {
        write.csv(r6$ora_table, file)
      }
    )

  })
}
