box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, reactive, isolate, div, h4, p, plotOutput, renderPlot, observeEvent, req, numericInput, br, uiOutput, renderUI],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, bs4Callout],
  echarts4r[echarts4rOutput, renderEcharts4r],
  shinyWidgets[actionBttn, prettyCheckbox, pickerInput],
  stringr[str_replace_all],
  gargoyle[init, watch, trigger],
  magrittr[`%>%`],
  htmlwidgets[JS],
  dplyr[filter, select, pull],
  reactable[reactableOutput, renderReactable, reactable, colDef, getReactableState],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "statistics_uni",
    fluidRow(
      valueBoxOutput(ns("tested_cond"), width = 2),
      valueBoxOutput(ns("significant"), width = 2),
      valueBoxOutput(ns("up_reg"), width = 2),
      valueBoxOutput(ns("down_reg"), width = 2),
      valueBoxOutput(ns("fdr_thr"), width = 2),
      valueBoxOutput(ns("fc_thr"), width = 2)
    ),
    fluidRow(
      bs4Callout(
        div(
          style = "display: flex; justify-content: center; align-items: center; gap: 20px",
          div(
            style = "width: 100%; flex: 1 1 0;",
            div(
              style = "display: flex; justify-content: center; align-items: center; gap: 20px",
              div(
                style = "width: 100%; flex: 3 1 0;",
                selectInput(
                  inputId = ns("test_input"),
                  label = "Test type",
                  choices = c("Welch's T-test" = "welch", "Student's T-test" = "student", "Wilcox's test" = "wilcox"),
                  selected = "welch", 
                  width = "auto"
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0; text-align: center; margin-top: 1.5rem;",
                prettyCheckbox(
                  inputId = ns("paider_input"),
                  label = "Paired", 
                  value = FALSE,
                  shape = "curve", 
                  width = "auto"
                )
              )
            )
          ),
          div(
            style = "width: 100%; flex: 1 1 0;",
            numericInput(
              inputId = ns("fc_input"),
              label = "Fold change",
              value = 1,
              min = 0,
              step = 0.5, 
              width = "auto"
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
            uiOutput(ns("ui_primary_input"))
          ),
          div(
            style = "width: 100%; flex: 1 1 0;",
            actionBttn(
              inputId = ns("run_statistics"),
              label = "Run statistics", 
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
        title = "Volcano plot",
        status = "primary",
        width = 8,
        height = 700,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = ns("volcano_sidebar"),
          div(
            style = "padding-right: 0.5rem",
            h4("Additional contrast"),
            div(
              style = "width: 100%;",
              uiOutput(ns("ui_additional_input"))
            )
            #mettere update button
          )
        ),
        uiOutput(ns("volcano_plot_multiple"))
      ),
      box(
        title = "Result table",
        status = "primary",
        width = 4,
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
    
    init("stat")
    
    output$ui_primary_input <- renderUI({
      
      watch("stat")
      
      test <- r6$all_test_combination
      
      pickerInput(
        inputId = session$ns("primary_input"),
        label = "Primary contrast",
        choices = test, 
        selected = r6$primary_condition,
        multiple = FALSE,
        # width = "auto",
        options = list(
          `live-search` = TRUE, 
          size = 5)
      )
      
      
    })
    
    output$ui_additional_input <- renderUI({
      
      watch("stat")
      
      test <- r6$all_test_combination
      
      test <- test[! test %in% r6$primary_condition]
      
      pickerInput(
        inputId = session$ns("additional_input"),
        label = "Additional contrast",
        choices = test,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE, 
          title = "None",
          `selected-text-format` = "count > 2",
          size = 5)
      )
      
      
    })
    
    output$tested_cond <- renderValueBox({
      
      watch("stat")
      
      if(is.null(r6$stat_table)){
        value <- "Undefinded"
      }else{
        value <- str_replace_all(r6$primary_condition, pattern = "_", replacement = " ")
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("compress"),
        color = "primary",
        footer = p("Primary condition", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$significant <- renderValueBox({
      
      watch("stat")
      
      sig_tested <- paste0(r6$primary_condition, "_significant")
      
      if(is.null(r6$stat_table)){
        value <- 0
      }else{
        sig <- r6$stat_table %>% 
          filter(!!as.symbol(sig_tested)) %>% 
          nrow()
        
        total <- nrow(r6$stat_table)
        
        value <- paste0(sig, " out of ", total)
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("adjust", verify_fa = FALSE),
        color = "primary",
        footer = p("Significant in primary condition", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$up_reg <- renderValueBox({
      
      watch("stat")
      
      sig_tested <- paste0(r6$primary_condition, "_significant")
      fc_tested <- paste0(r6$primary_condition, "_fold_change")
      
      if(is.null(r6$stat_table)){
        value <- 0
      }else{
        sig <- r6$stat_table %>% 
          filter(!!as.symbol(sig_tested)& !!as.symbol(fc_tested) > 0) %>% 
          nrow()
        
        total <- r6$stat_table %>% 
          filter(!!as.symbol(sig_tested)) %>% 
          nrow()
        
        value <- paste0(sig, " out of ", total)
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("angle-up"),
        color = "primary",
        footer = p("Up-regulated in primary condition", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$down_reg <- renderValueBox({
      
      watch("stat")
      
      sig_tested <- paste0(r6$primary_condition, "_significant")
      fc_tested <- paste0(r6$primary_condition, "_fold_change")
      
      if(is.null(r6$stat_table)){
        value <- 0
      }else{
        sig <- r6$stat_table %>% 
          filter(!!as.symbol(sig_tested)& !!as.symbol(fc_tested) < 0) %>% 
          nrow()
        
        total <- r6$stat_table %>% 
          filter(!!as.symbol(sig_tested)) %>% 
          nrow()
        
        value <- paste0(sig, " out of ", total)
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("angle-down"),
        color = "primary",
        footer = p("Down-regulated in primary condition", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$fdr_thr <- renderValueBox({
      
      watch("stat")
      
      if(is.null(r6$stat_table)){
        value <- "Undefinded"
      }else{
        value <- r6$univariate_alpha
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
    
    output$fc_thr <- renderValueBox({
      
      watch("stat")
      
      if(is.null(r6$stat_table)){
        value <- "Undefinded"
      }else{
        value <- r6$fold_change
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("filter"),
        color = "primary",
        footer = p("Fold change", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    
    observeEvent(input$run_statistics ,{
      
      req(input$test_input)
      req(input$fc_input)
      req(input$alpha_input)
      req(input$truncation_input)
      req(input$primary_input)
      
      r6$univariate_test_type <- input$test_input
      r6$univariate_paired <- input$paider_input
      r6$fold_change <- as.double(input$fc_input)
      r6$univariate_alpha <- as.double(input$alpha_input)
      r6$univariate_p_adj_method <- input$truncation_input
      r6$primary_condition <- input$primary_input
      r6$additional_condition <- input$additional_input
      
      tests <- c(r6$primary_condition, r6$additional_condition)

      r6$stat_t_test(
        test = tests,
        fc = r6$fold_change,
        alpha = r6$univariate_alpha,
        p_adj_method = r6$univariate_p_adj_method,
        paired_test = r6$univariate_paired,
        test_type = r6$univariate_test_type
      )
      
      trigger("stat")
    })
    
    gene_selected <- reactive(getReactableState("table", "selected"))
    
    output$volcano_plot_multiple <- renderUI({

      watch("stat")

      req(input$run_statistics)

      tests <- c(r6$primary_condition, r6$additional_condition)
      
      if(is.null(r6$stat_table)){
        highlights <- NULL
      }else{
        table <- r6$print_stat_table(stat_table = r6$stat_table, test = r6$primary_condition)
        highlights <- table[gene_selected(), ] %>% 
          pull(gene_names)
      }
      
        r6$plot_volcano(test = tests, highlights_names = highlights)

    })
    
    output$table <- renderReactable({
      
      watch("stat")
      
      req(input$run_statistics)
      
      table <- r6$print_stat_table(stat_table = r6$stat_table, test = r6$primary_condition)
      
      reactable(
        table,
        searchable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        height = "auto",
        selection = "multiple",
        onClick = "select",
        defaultSelected = 1,
        columns = list(
          gene_names = colDef(align = "center", name = "Gene names"),
          p_val = colDef(align = "center", name = "-log(p.value)"),
          p_adj = colDef(align = "center", name = "-log(p.adj)"),
          fold_change = colDef(
            align = "center",
            name = "Fold change",
            style = JS("function(rowInfo) {
              const value = rowInfo.values['fold_change']
              let color
              if (value > 0) {
                color = '#cf4446'
              } else if (value < 0) {
                color = '#0d0887'
              } else {
                color = '#777'
              }
              return { color: color, fontWeight: 'bold' }
            }")
            ),
          significant = colDef(
            align = "center",
            name = "Significant",
            cell = function(value) {
              if (!value)
                "\u274c No"
              else
                "\u2714\ufe0f Yes"
            }
          )
        )
      )
      
    })
    
    

  })
}
