box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, updateSelectInput, updateNumericInput, reactive, isolate, div, h4, p, plotOutput, renderPlot, observeEvent, req, numericInput, br, uiOutput, renderUI, column, observe],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, bs4Callout, accordion, accordionItem, updateAccordion],
  echarts4r[echarts4rOutput, renderEcharts4r],
  shinyWidgets[actionBttn, prettyCheckbox, pickerInput, updatePrettyCheckbox, updatePickerInput],
  stringr[str_replace_all],
  gargoyle[init, watch, trigger],
  magrittr[`%>%`],
  htmlwidgets[JS],
  dplyr[filter, select, pull],
  reactable[reactableOutput, renderReactable, reactable, colDef, getReactableState],
  utils[write.csv],
  waiter[Waiter, spin_5],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "statistics_uni",
    fluidRow(
      valueBoxOutput(ns("tested_cond"), width = 3),
      valueBoxOutput(ns("significant"), width = 3),
      valueBoxOutput(ns("up_reg"), width = 3),
      valueBoxOutput(ns("down_reg"), width = 3)
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
                    selectInput(
                      inputId = ns("test_input"),
                      label = "Test type",
                      choices = c("Welch's T-test" = "welch", "Student's T-test" = "student", "limma", "Wilcox's test" = "wilcox"),
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
                ),
                numericInput(
                  inputId = ns("fc_input"),
                  label = "Fold change",
                  value = 1,
                  min = 0,
                  step = 0.5, 
                  width = "auto"
                ),
                div(
                  style = "display: flex; justify-content: center; align-items: center; gap: 20px",
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
                  )
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                pickerInput(
                  inputId = ns("primary_input"),
                  label = "Primary comparison",
                  choices = NULL, 
                  selected = NULL,
                  multiple = FALSE,
                  options = list(
                    `live-search` = TRUE, 
                    size = 5)
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                pickerInput(
                  inputId = ns("additional_input"),
                  label = "Additional comparison",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    `live-search` = TRUE, 
                    title = "None",
                    `selected-text-format` = "count > 2",
                    size = 5)
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
            inputId = ns("run_statistics"),
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
        title = "Volcano plot",
        status = "primary",
        width = 8,
        height = 700,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = ns("volcano_sidebar"),
          div(
            style = "padding-right: 0.5rem",
            h4("Visual settings"),
            br(),
            prettyCheckbox(
              inputId = ns("same_y_input"),
              label = "Share same Y axis", 
              value = TRUE,
              shape = "curve", 
              width = "auto"
            ),
            prettyCheckbox(
              inputId = ns("same_x_input"),
              label = "Share same X axis", 
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
        uiOutput(ns("volcano_plot_multiple"))
      ),
      box(
        title = "Profile plot",
        status = "primary",
        width = 4,
        height = 700,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = ns("profile_sidebar"),
          div(
            style = "padding-right: 0.5rem",
            h4("Visual settings"),
            numericInput(
              inputId = ns("grid_input"),
              label = "Grid n° of columns",
              value = 2,
              min = 1,
              step = 1, 
              width = "auto"
            ),
            br(),
            actionBttn(
              inputId = ns("update2"),
              label = "Update",
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          )
        ),
        uiOutput(ns("profile_plot"))
      )
    ),
    fluidRow(
      box(
        title = "Result table",
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
    
    w <- Waiter$new(html = spin_5(), color = "#adb5bd")
    
    init("stat")
    
    observe({
      
      watch("stat")
      watch("ui_element")
      
      test <- r6$all_test_combination
      
      test_add <- test[! test %in% r6$primary_condition]
      
      updatePickerInput(
        session = session,
        inputId = "primary_input",
        choices = test,
        selected = r6$primary_condition
      )
      updatePickerInput(
        session = session,
        choices = test_add,
        inputId = "additional_input",
        selected = r6$additional_condition
      )
      
    })
    
    observeEvent(input$primary_input, {
      
      test <- r6$all_test_combination
      
      test_add <- test[! test %in% input$primary_input]
      
      updatePickerInput(
        session = session,
        choices = test_add,
        inputId = "additional_input",
        selected = r6$additional_condition
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
    
    observe({
      
      watch("params")
      
      updateSelectInput(inputId = "test_input", selected = r6$univariate_test_type)
      updatePrettyCheckbox(inputId = "paider_input", value = r6$univariate_paired)
      updateNumericInput(inputId = "fc_input", value = r6$fold_change)
      updateSelectInput(inputId = "alpha_input", selected = r6$univariate_alpha)
      updateSelectInput(inputId = "truncation_input", selected = r6$univariate_p_adj_method)
      updatePickerInput(session = session, inputId = "primary_input", selected = r6$primary_condition)
      updatePickerInput(session = session, inputId = "additional_input", selected = r6$additional_condition)
      
    })
    
    observeEvent(input$run_statistics ,{
      
      w$show()
      
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
      
      updateAccordion(id = "advance_params", selected = NULL)
      
      trigger("stat")
      
      w$hide()
    })
    
    observeEvent(input$update ,{

      req(input$run_statistics)

      trigger("stat")
    })
    
    observeEvent(input$update2 ,{
      
      req(input$run_statistics)
      
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
        table <- r6$print_stat_table()
        highlights <- table[gene_selected(), ] %>% 
          pull(gene_names)
      }
      
      r6$plot_volcano(
        test = tests,
        highlights_names = highlights,
        same_x = isolate(input$same_x_input),
        same_y = isolate(input$same_y_input)
      )
      
    })
    
    output$table <- renderReactable({
      
      watch("stat")
      
      req(input$run_statistics)
      
      table <- r6$print_stat_table()
      
      reactable(
        table,
        searchable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        height = "auto",
        selection = "multiple",
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
    
    output$profile_plot <- renderUI({
      
      watch("stat")
      
      req(input$run_statistics)
      
      tests <- c(r6$primary_condition, r6$additional_condition)
      table <- r6$print_stat_table()
      
      if(is.null(gene_selected())){
        return(NULL)
      }else{
        highlights <- table[gene_selected(), ] %>% 
          pull(gene_names)
        
        r6$plot_stat_profile(tests = tests, highlights_names = highlights, grid_cols = isolate(input$grid_input))
      }
      
      
    })
    

  })
}
