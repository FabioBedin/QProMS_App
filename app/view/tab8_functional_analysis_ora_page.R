box::use(
  shiny[moduleServer, NS, fluidRow, column, div, selectInput, uiOutput, numericInput, h4, br, icon, p, renderUI, observeEvent, observe, isolate, req, reactive, reactiveVal, tagList, img, h3],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, bs4Callout, accordion, accordionItem, updateAccordion, toast],
  shinyWidgets[actionBttn, prettyCheckbox, pickerInput, sliderTextInput, downloadBttn, updatePickerInput],
  gargoyle[init, watch, trigger],
  reactable[reactableOutput, renderReactable, reactable, colDef, getReactableState],
  dplyr[group_by, `%>%`, pull, slice_max, filter, case_when, if_all, ends_with],
  tibble[rowid_to_column],
  shinyjs[disabled, enable],
  echarts4r[echarts4rOutput, renderEcharts4r, echarts4rProxy, e_focus_adjacency_p],
  waiter[Waiter, spin_5],
  shinyGizmo[conditionalJS, jsCalls],
  utils[write.csv],
  callr[r_bg]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "functional_analysis_ora",
    fluidRow(
      valueBoxOutput(ns("significant_bp"), width = 4),
      valueBoxOutput(ns("significant_mf"), width = 4),
      valueBoxOutput(ns("significant_cc"), width = 4)
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
                    "Top rank proteins" = "top_rank",
                    "Selected nodes" = "nodes"
                  ),
                  selected = "univariate", 
                  width = "auto"
                ),
                conditionalJS(
                  pickerInput(
                    inputId = ns("test_uni_input"),
                    label = "Contrasts",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(
                      `live-search` = TRUE,
                      title = "None",
                      `selected-text-format` = "count > 2",
                      size = 5)
                  ),
                  condition = "input.from_statistic_input == 'univariate'",
                  jsCall = jsCalls$show(),
                  ns = ns
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                div(
                  style = "display: flex; justify-content: center; align-items: center; gap: 20px",
                  div(
                    style = "width: 100%; flex: 3 1 0;",
                    selectInput(
                      inputId = ns("alpha_input"),
                      label = "Alpha",
                      choices = c(0.05, 0.01),
                      selected = 0.05, 
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
                  )
                ),
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
              )
            )
          ),
          accordionItem(
            title = "Visual settings",
            status = "gray",
            collapsed = FALSE,
            solidHeader = TRUE,
            div(
              style = "display: flex; justify-content: center; align-items: start; gap: 5rem",
              div(
                style = "width: 100%; flex: 1 1 0;",
                div(
                  style = "display: flex; justify-content: center; align-items: center; gap: 20px",
                  div(
                    style = "width: 100%; flex: 1 1 0;",
                    selectInput(
                      inputId = ns("ontology"),
                      label = "Ontology term",
                      choices = c("BP", "MF", "CC"),
                      selected = "BP"
                    )
                  ),
                  conditionalJS(
                    div(
                      style = "width: 100%; flex: 1 1 0;",
                      pickerInput(
                        inputId = ns("focus_input"),
                        label = "Contrasts direction",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(
                          `live-search` = TRUE, 
                          title = "None",
                          `selected-text-format` = "count > 1",
                          size = 5)
                      )
                    ),
                    condition = "input.run_analysis > 0",
                    jsCall = jsCalls$show(),
                    ns = ns
                  )
                ),
                div(
                  style = "display: flex; justify-content: center; align-items: center; gap: 20px",
                  div(
                    style = "width: 100%; flex: 1 1 0;",
                    numericInput(
                      inputId = ns("top_n_input"),
                      label = "Show top n terms",
                      value = 10,
                      min = 1,
                      step = 1
                    )
                  ),
                  div(
                    style = "width: 100%; flex: 1 1 0;",
                    selectInput(
                      inputId = ns("plot_value_input"),
                      label = "Variable on X axis",
                      choices = c("Fold change" = "fold_change", "p.value" = "pvalue", "p.adjust" = "p.adjust", "q.value" = "qvalue"),
                      selected = "fold_change"
                    )
                  )
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                sliderTextInput(
                  inputId = ns("simplify_thr"), 
                  label = "Reduce redundancy in GO terms",
                  choices = c("highly simplified", "2", "3", "4", "5", "6", "7", "8", "9", "not simplified"), 
                  selected = "7",
                  grid = TRUE,
                  force_edges = TRUE,
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
            inputId = ns("run_analysis"),
            label = "Start", 
            style = "material-flat",
            color = "success",
            size = "md",
            block = TRUE, 
            width = "auto"
          )
        ),
        div(
          style = "margin-top: 22px;",
          disabled(
            actionBttn(
              inputId = ns("update"),
              label = "Update", 
              style = "material-flat",
              color = "success",
              size = "md",
              block = TRUE,
              width = "auto"
            )
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Functional plot",
        status = "primary",
        width = 6,
        height = 700,
        maximizable = TRUE,
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
        reactableOutput(ns("table"))
      )
    )
  )

}



#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    w <- Waiter$new(html = tagList(
      h3("Loading can take some minutes...", style = "color:white;"),
      br(),
      # spin_5()
      img(src = "https://media.giphy.com/media/y6Sl42U3xEFkk/giphy.gif", height = "200px")
    ), color = "#adb5bd")
    
    w2 <- Waiter$new(html = spin_5(), color = "#adb5bd")
    
    init("functional")
    
    observe({
      
      watch("stat")
      watch("ui_element")
      watch("functional")
      
      tests <- c(r6$primary_condition, r6$additional_condition)
      
      updatePickerInput(
        session = session,
        inputId = "test_uni_input",
        choices = tests,
        selected = r6$primary_condition
      )
      
    })
    
    
    output$significant_bp <- renderValueBox({
      
      watch("functional")
      
      if(is.null(r6$ora_table)){
        value <- "Undefinded"
      }else{
        value <- r6$ora_table_counts %>% 
          filter(ONTOLOGY == "BP") %>% 
          pull(count)
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("adjust", verify_fa = FALSE),
        color = "primary",
        footer = p("Significant terms in biological process (BP)", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$significant_mf <- renderValueBox({
      
      watch("functional")
      
      if(is.null(r6$ora_table)){
        value <- "Undefinded"
      }else{
        value <- r6$ora_table_counts %>% 
          filter(ONTOLOGY == "MF") %>% 
          pull(count)
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("adjust", verify_fa = FALSE),
        color = "primary",
        footer = p("Significant terms in molecular function (MF)", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$significant_cc <- renderValueBox({
      
      watch("functional")
      
      if(is.null(r6$ora_table)){
        value <- "Undefinded"
      }else{
        value <- r6$ora_table_counts %>% 
          filter(ONTOLOGY == "CC") %>% 
          pull(count)
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("adjust", verify_fa = FALSE),
        color = "primary",
        footer = p("Significant terms in cellular component (CC)", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    observeEvent(input$run_analysis, {
      
      req(input$from_statistic_input)
      req(input$test_uni_input)
      req(input$alpha_input)
      req(input$truncation_input)
      
      r6$go_ora_from_statistic <- input$from_statistic_input
      r6$go_ora_tested_condition <- input$test_uni_input
      r6$go_ora_alpha <- as.double(input$alpha_input)
      r6$go_ora_p_adj_method <- input$truncation_input
      r6$go_ora_term <- input$ontology
      r6$go_ora_top_n <- input$top_n_input
      r6$go_ora_plot_value <- input$plot_value_input
      
      if (input$simplify_thr == "highly simplified") {
        simp_thr <- 0.1
      } else if (input$simplify_thr == "not simplified") {
        simp_thr <- 1
      } else if (is.null(input$simplify_thr)) {
        simp_thr <- 0.7
      } else {
        simp_thr <- as.numeric(input$simplify_thr) / 10
      }
      r6$go_ora_simplify_thr <- simp_thr
      
      if (input$from_statistic_input == "univariate") {
        
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
        
        focus_label <-  "Contrasts direction"
        
      } else if (input$from_statistic_input == "multivariate") {
        
        if(is.null(r6$anova_table)) {
          input_error <- "You need to perform Multivariate statistics in order to run this analysis."
        } else {
          input_error <- case_when(
            nrow(filter(r6$anova_table, significant)) == 0 ~ "There are no significant, no network analysis will be performed",
            TRUE ~ ""
          )
        }
        
        focus_label <- "Clusters"
        
      } else if (input$from_statistic_input == "nodes") {
        input_error <- case_when(
          length(r6$selected_nodes) == 0 ~ "You need to select at least some nodes in order to run this analysis.",
          TRUE ~ ""
        )
        
        focus_label <- "Nodes"
      } else {
        
        input_error <- case_when(
          is.null(r6$protein_rank_list) |
            length(r6$protein_rank_list) == 0 ~ "You need to select at least some proteins in order to run this analysis.",
          TRUE ~ ""
        )
        
        focus_label <- "Protein rank"
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
      
      r6$go_ora(
        list_from = r6$go_ora_from_statistic,
        test = r6$go_ora_tested_condition,
        alpha = r6$go_ora_alpha,
        p_adj_method = r6$go_ora_p_adj_method,
        background = isolate(input$background_input)
      )
      
      r6$go_simplify(thr = r6$go_ora_simplify_thr, type = "ora")
      
      r6$go_ora_focus <- names(r6$ora_result_list)[1]

      r6$print_ora_table(ontology = r6$go_ora_term, groups = r6$go_ora_focus, value = r6$go_ora_plot_value)
      
      updatePickerInput(
        session = session,
        label = focus_label,
        inputId = "focus_input",
        choices = names(r6$ora_result_list),
        selected = r6$go_ora_focus
      )
      
      enable("update")
      
      updateAccordion(id = "advance_params", selected = 1)

      trigger("functional")

      w$hide()
    })
    
    
    observeEvent(input$update, {
      
      w2$show()
      
      req(input$simplify_thr)
      req(input$ontology)
      req(input$focus_input)
      req(input$top_n_input)
      req(input$plot_value_input)
      
      r6$go_ora_term <- input$ontology
      r6$go_ora_top_n <- input$top_n_input
      r6$go_ora_plot_value <- input$plot_value_input
      r6$go_ora_focus <- input$focus_input
      
      if(is.null(r6$go_ora_focus)){
        toast(
          title = "Input is null",
          options = list(
            class = "bg-danger",
            autohide = TRUE,
            delay = 5000,
            icon = icon("exclamation-circle", verify_fa = FALSE)
          )
        )
        w2$hide()
        return()
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
        
        r6$go_simplify(thr = r6$go_ora_simplify_thr, type = "ora")
        
      }
      
      r6$print_ora_table(ontology = r6$go_ora_term, groups = r6$go_ora_focus, value = r6$go_ora_plot_value)
      
      trigger("functional")
      
      w2$hide()
      
    })
    
    observeEvent(input$update_net, {
      
      req(input$run_analysis)
      
      trigger("functional")
      
    })
    
    output$table <- renderReactable({
      
      req(input$run_analysis)
      
      watch("functional")
      
      if (!is.null(r6$ora_table)) {
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
        
      }
      
      
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
    

    observeEvent(input$functional_plot_mouseover_data, {
      
      echarts4rProxy(session$ns("network_plot")) %>% 
        e_focus_adjacency_p(
          seriesIndex = 0,
          index = input$functional_plot_mouseover_row
        )
      
    })
    

  })
}
