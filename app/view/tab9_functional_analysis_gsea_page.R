box::use(
  shiny[moduleServer, NS, fluidRow, column, div, selectInput, uiOutput, numericInput, updateSelectInput, h4, br, icon, p, renderUI, observeEvent, isolate, req, reactive, reactiveVal, downloadHandler, tagList, img, h3, observe],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, bs4Callout, accordion, accordionItem, updateAccordion],
  shinyWidgets[actionBttn, prettyCheckbox, pickerInput, sliderTextInput, downloadBttn, updatePickerInput],
  gargoyle[init, watch, trigger],
  reactable[reactableOutput, renderReactable, reactable, colDef, getReactableState],
  dplyr[group_by, `%>%`, pull, slice_max, filter, distinct, select],
  tibble[rowid_to_column],
  shinyjs[disabled, enable],
  echarts4r[echarts4rOutput, renderEcharts4r, echarts4rProxy, e_focus_adjacency_p],
  waiter[Waiter, spin_5],
  shinyGizmo[conditionalJS, jsCalls],
  utils[write.csv]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "functional_analysis_gsea",
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
                  inputId = ns("rank_with"),
                  label = "Rank with",
                  choices = c("Fold change" = "fc", "Intensity" = "intensity"),
                  selected = "fc", 
                  width = "auto"
                ),
                conditionalJS(
                  pickerInput(
                    inputId = ns("tests_input"),
                    label = "Contrast",
                    choices = NULL,
                    multiple = TRUE,
                    selected = NULL,
                    options = list(
                      `live-search` = TRUE, 
                      title = "None",
                      `selected-text-format` = "count > 2",
                      size = 5)
                  ),
                  condition = "input.rank_with == 'fc'",
                  jsCall = jsCalls$show(),
                  ns = ns
                ),
                div(
                  style = "display: flex; justify-content: center; align-items: center; gap: 20px",
                  div(
                    style = "width: 100%; flex: 3 1 0;",
                    conditionalJS(
                      pickerInput(
                        inputId = ns("target"),
                        label = "Target",
                        choices = NULL,
                        multiple = TRUE,
                        selected = NULL,
                        options = list(
                          `live-search` = TRUE, 
                          title = "None",
                          `selected-text-format` = "count > 2",
                          size = 5)
                      ),
                      condition = "input.rank_with != 'fc'",
                      jsCall = jsCalls$show(),
                      ns = ns
                    )
                  ),
                  div(
                    style = "width: 100%; flex: 1 1 0; text-align: center; margin-top: 1.5rem;",
                    conditionalJS(
                      prettyCheckbox(
                        inputId = ns("by_cond_input"),
                        label = "Use mean of condition", 
                        value = FALSE,
                        shape = "curve", 
                        width = "auto"
                      ),
                      condition = "input.rank_with != 'fc'",
                      jsCall = jsCalls$show(),
                      ns = ns
                    )
                  )
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                selectInput(
                  inputId = ns("alpha_input"),
                  label = "Alpha",
                  choices = c(0.05, 0.01),
                  selected = 0.05, 
                  width = "auto"
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
                  div(
                    style = "width: 100%; flex: 1 1 0;",
                    numericInput(
                      inputId = ns("top_n_input"),
                      label = "Show top n terms",
                      value = 10,
                      min = 1,
                      step = 1
                    )
                  )
                ),
                div(
                  style = "width: 100%; flex: 1 1 0;",
                  prettyCheckbox(
                    inputId = ns("common_terms_input"),
                    label = "Keep only common terms", 
                    value = FALSE,
                    shape = "curve", 
                    width = "auto"
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
        width = 7,
        height = 700,
        maximizable = TRUE,
        echarts4rOutput(ns("functional_plot"), height = "650")
      ),
      box(
        title = "Result Table",
        status = "primary",
        width = 5,
        height = 700,
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

    init("gsea", "trigger2")
    
    observe({

      watch("ui_element")
      watch("trigger2")
      
      tests <- r6$all_test_combination
      
      updatePickerInput(
        session = session,
        inputId = "tests_input",
        choices = tests,
        selected = r6$primary_condition
      )
      
      if(!is.null(r6$expdesign)) {
        if(r6$protein_rank_by_cond_gsea){
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
        
        updatePickerInput(
          session = session,
          inputId = "target",
          choices = scelte,
          selected = scelte[1]
        )
      }
      
    })
    
    observeEvent(input$by_cond_input, {
      
      r6$protein_rank_by_cond_gsea <- input$by_cond_input
      
      trigger("trigger2")
      
    })


    output$significant_bp <- renderValueBox({
      
      watch("gsea")
      
      if(is.null(r6$gsea_table)){
        value <- "Undefinded"
      }else{
        value <- r6$gsea_table_counts %>% 
          filter(ONTOLOGY == "BP") %>% 
          pull(count)
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("adjust", verify_fa = FALSE),
        color = "primary",
        footer = p("Significant terms in BP", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$significant_mf <- renderValueBox({
      
      watch("gsea")
      
      if(is.null(r6$gsea_table)){
        value <- "Undefinded"
      }else{
        value <- r6$gsea_table_counts %>% 
          filter(ONTOLOGY == "MF") %>% 
          pull(count)
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("adjust", verify_fa = FALSE),
        color = "primary",
        footer = p("Significant terms in MF", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$significant_cc <- renderValueBox({
      
      watch("gsea")
      
      if(is.null(r6$gsea_table)){
        value <- "Undefinded"
      }else{
        value <- r6$gsea_table_counts %>% 
          filter(ONTOLOGY == "CC") %>% 
          pull(count)
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("adjust", verify_fa = FALSE),
        color = "primary",
        footer = p("Significant terms in CC", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    

    observeEvent(input$run_analysis ,{

      w$show()

      req(input$rank_with)
      req(input$tests_input)
      req(input$alpha_input)
      req(input$truncation_input)

      # r6$go_gsea_tested_condition <- input$tests_input
      r6$go_gsea_alpha <- as.double(input$alpha_input)
      r6$go_gsea_p_adj_method <- input$truncation_input
      r6$go_gsea_term <- input$ontology
      r6$go_gsea_top_n <- as.double(input$top_n_input)
      r6$go_gsea_common_terms <- input$common_terms_input
      # r6$go_gsea_focus <- input$tests_input
      
      if(input$rank_with == "fc") {
        r6$go_gsea_tested_condition <- input$tests_input
        r6$go_gsea_focus <- input$tests_input
      } else {
        r6$go_gsea_tested_condition <- input$target
        r6$go_gsea_focus <- input$target
      }
      
      if (input$simplify_thr == "highly simplified") {
        simp_thr <- 0.1
      } else if (input$simplify_thr == "not simplified") {
        simp_thr <- 1
      } else if (is.null(input$simplify_thr)) {
        simp_thr <- 0.7
      } else {
        simp_thr <- as.numeric(input$simplify_thr) / 10
      }
      r6$go_gsea_simplify_thr <- simp_thr

      r6$go_gsea(
        test = r6$go_gsea_tested_condition, 
        rank_type = isolate(input$rank_with), 
        by_condition = isolate(input$by_cond_input),
        alpha = r6$go_gsea_alpha,
        p_adj_method = r6$go_gsea_p_adj_method
      )

      r6$go_simplify(thr = r6$go_gsea_simplify_thr, type = "gsea")

      r6$print_gsea_table(ontology = r6$go_gsea_term, groups = r6$go_gsea_focus, only_common = r6$go_gsea_common_terms)
      
      enable("update")
      
      updateAccordion(id = "advance_params", selected = 1)

      trigger("gsea")

      w$hide()
    })

    observeEvent(input$update, {
      
      w2$show()

      req(input$simplify_thr)
      req(input$ontology)
      req(input$top_n_input)

      r6$go_gsea_term <- input$ontology
      r6$go_gsea_top_n <- as.double(input$top_n_input)
      r6$go_gsea_common_terms <- input$common_terms_input

      if (input$simplify_thr == "highly simplified") {
        simp_thr <- 0.1
      } else if (input$simplify_thr == "not simplified") {
        simp_thr <- 1
      } else {
        simp_thr <- as.numeric(input$simplify_thr) / 10
      }

      if (r6$go_gsea_simplify_thr != simp_thr) {

        r6$go_gsea_simplify_thr <- simp_thr

        r6$go_simplify(thr = r6$go_gsea_simplify_thr, type = "gsea")

      }

      r6$print_gsea_table(ontology = r6$go_gsea_term, groups = r6$go_gsea_focus, only_common = r6$go_gsea_common_terms)

      trigger("gsea")
      
      w2$hide()

    })

    
    output$table <- renderReactable({

      watch("gsea")

      req(input$run_analysis)

      default_selected <- r6$gsea_table %>%
        rowid_to_column() %>%
        group_by(group) %>%
        slice_max(pvalue, n = r6$go_gsea_top_n) %>%
        pull(rowid)

      reactable(
        r6$gsea_table,
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
          setSize = colDef(align = "center", name = "Set size"),
          Description = colDef(minWidth = 400),
          core_enrichment = colDef(minWidth = 1000),
          enrichmentScore = colDef(align = "center", name = "Enrichment score"),
          NES = colDef(align = "center", name = "NES"),
          pvalue = colDef(align = "center", name = "-log(p.val)"),
          p.adjust = colDef(align = "center", name = "-log(p.adj)"),
          qvalue = colDef(align = "center", name = "-log(q.val)"),
          rank = colDef(align = "center")
        )
      )

    })

    gene_selected <- reactive(getReactableState("table", "selected"))

    output$functional_plot <- renderEcharts4r({

      watch("gsea")

      req(input$run_analysis)

      table <- r6$gsea_table

      if(is.null(gene_selected())) {
        r6$plot_ora_empty(val = "pvalue")
      } else{
        highlights <- table[gene_selected(),] %>%
          pull(ID) %>% 
          unique()

        r6$plot_gsea(
          term = r6$go_gsea_term,
          groups = r6$go_gsea_focus,
          selected = highlights
        )
      }

    })
   
    
  })
}
