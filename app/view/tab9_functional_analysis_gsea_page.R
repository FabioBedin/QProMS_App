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
    tabName = "functional_analysis_gsea",
    fluidRow(
      valueBoxOutput(ns("significant"), width = 3),
      bs4Callout(
        div(
          style = "display: flex; justify-content: center; gap: 20px; align-items: center;",
          div(
            style = "width: 100%; flex: 1 1 0;",
            uiOutput(ns("ui_test_input"))
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
        width = 9,
        elevation = 1
      )
    ),
    fluidRow(
      box(
        title = "Functional plot",
        status = "primary",
        width = 7,
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
                  step = 1
                )
              )
            ),
            sliderTextInput(
              inputId = ns("simplify_thr"), 
              label = "Reduce redundancy in GO terms",
              choices = c("highly simplified", "2", "3", "4", "5", "6", "7", "8", "9", "not simplified"), 
              selected = "7",
              grid = TRUE,
              force_edges = TRUE,
            ),
            prettyCheckbox(
              inputId = ns("common_terms_input"),
              label = "Keep only common terms", 
              value = FALSE,
              shape = "curve", 
              width = "auto"
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
        title = "Result Table",
        status = "primary",
        width = 5,
        height = 700,
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

    init("gsea")

    output$ui_test_input <- renderUI({

      watch("gsea")

      test <- r6$all_test_combination
      
      if (is.null(input$tests_input)) {
        sel <- NULL
      } else {
        sel <- input$tests_input
      }
      
      pickerInput(
        inputId = session$ns("tests_input"),
        label = "Contrast",
        choices = test,
        multiple = TRUE,
        width = "auto",
        selected = sel,
        options = list(
          `live-search` = TRUE, 
          title = "None",
          `selected-text-format` = "count > 2",
          size = 5)
      )

    })
    
    output$significant <- renderValueBox({

      watch("gsea")

      if(is.null(r6$gsea_table)){
        value <- 0
      }else{
        sig <- nrow(r6$gsea_table)

        term <- r6$go_gsea_term

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
    

    observeEvent(input$run_analysis ,{

      w$show()

      req(input$tests_input)
      req(input$alpha_input)
      req(input$truncation_input)
      req(input$ontology)
      req(input$top_n_input)
      req(input$simplify_thr)

      r6$go_gsea_tested_condition <- input$tests_input
      r6$go_gsea_alpha <- as.double(input$alpha_input)
      r6$go_gsea_p_adj_method <- input$truncation_input
      r6$go_gsea_term <- input$ontology
      r6$go_gsea_top_n <- as.double(input$top_n_input)
      r6$go_gsea_common_terms <- input$common_terms_input
      r6$go_gsea_focus <- input$tests_input
      
      if (input$simplify_thr == "highly simplified") {
        simp_thr <- 0.1
      } else if (input$simplify_thr == "not simplified") {
        simp_thr <- 1
      } else {
        simp_thr <- as.numeric(input$simplify_thr) / 10
      }
      r6$go_gsea_simplify_thr <- simp_thr

      r6$go_gsea(
        test = r6$go_gsea_tested_condition,
        alpha = r6$go_gsea_alpha,
        p_adj_method = r6$go_gsea_p_adj_method
      )

      r6$go_simplify(thr = r6$go_gsea_simplify_thr, type = "gsea")

      r6$print_gsea_table(ontology = r6$go_gsea_term, groups = r6$go_gsea_focus, only_common = r6$go_gsea_common_terms)

      trigger("gsea")

      w$hide()
    })

    observeEvent(input$update, {

      w$show()

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

      w$hide()

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
   
    output$download_table <- downloadHandler(
      filename = function() {
        paste("gsea_table", ".csv", sep="")
      },
      content = function(file) {
        write.csv(r6$gsea_table, file)
      }
    )
    
  })
}
