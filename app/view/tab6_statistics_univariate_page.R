box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, isolate, div, h4, p, plotOutput, renderPlot, observeEvent, req, numericInput, br, uiOutput, renderUI],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox],
  echarts4r[echarts4rOutput, renderEcharts4r],
  shinyWidgets[actionBttn, prettyCheckbox, pickerInput],
  stringr[str_replace_all],
  gargoyle[init, watch, trigger],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "statistics",
    fluidRow(
      valueBoxOutput(ns("tested_cond"), width = 2),
      valueBoxOutput(ns("significant"), width = 2),
      valueBoxOutput(ns("up_reg"), width = 2),
      valueBoxOutput(ns("down_reg"), width = 2),
      valueBoxOutput(ns("fdr_thr"), width = 2),
      valueBoxOutput(ns("fc_thr"), width = 2)
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
          startOpen = TRUE,
          div(
            style = "padding-right: 0.5rem",
            h4("Test"),
            div(
              style = "display: flex; justify-content: center; align-items: center; gap: 20px",
              div(
                style = "width: 100%; flex: 3 1 0;",
                selectInput(
                  inputId = ns("test_input"),
                  label = NULL,
                  choices = c("Student's T-test" = "student", "Welch's T-test" = "welch"),
                  selected = "student"
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;  text-align: center;",
                prettyCheckbox(
                  inputId = ns("paider_input"),
                  label = "Paired", 
                  value = FALSE,
                  shape = "curve"
                )
              )
            ),
            h4("Parameters"),
            div(
              style = "display: flex; justify-content: center; gap: 20px; align-items: center;",
              div(
                style = "width: 100%; flex: 1 1 0;",
                numericInput(
                  inputId = ns("fc_input"),
                  label = "Fold change",
                  value = 1,
                  min = 0,
                  step = 0.5
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                selectInput(
                  inputId = ns("alpha_input"),
                  label = "Alpha",
                  choices = c(0.05, 0.01),
                  selected = 0.05
                )
              ),
              div(
                style = "width: 100%; flex: 2 1 0;",
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
                  selected = "BH"
                )
              )
            ),
            h4("Contrast"),
            div(
              style = "display: flex; justify-content: center; gap: 20px",
              div(
                style = "width: 100%;",
                uiOutput(ns("ui_primary_input"))
              ),
              div(
                style = "width: 100%;",
                uiOutput(ns("ui_additiolnal_input"))
              )
            ),
            br(),
            actionBttn(
              inputId = ns("run_statistics"),
              label = "Run statistics", 
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          )
        ),
        echarts4rOutput(ns("volcano_plot"), height = "650")
      ),
      box(
        title = "Result table",
        status = "primary",
        width = 4,
        height = 700,
        maximizable = TRUE,
        # collapsed = TRUE,
        echarts4rOutput(ns("scatter_plot"), height = "650")
        # plotOutput(ns("correlation_static_plot"), height = "650")
      )
    )
  )

}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    init("plot", "boxes")
    
    output$ui_primary_input <- renderUI({
      
      watch("boxes")
      
      test <- r6$all_test_combination
      
      pickerInput(
        inputId = session$ns("primary_input"),
        label = "Primary contrast",
        choices = test, 
        selected = r6$primary_condition,
        multiple = FALSE,
        options = list(
          `live-search` = TRUE, 
          size = 5)
      )
      
      
    })
    
    output$ui_additiolnal_input <- renderUI({
      
      watch("boxes")
      
      test <- r6$all_test_combination
      
      pickerInput(
        inputId = session$ns("additiolnal_input"),
        label = "Additiolnal contrast",
        choices = test, 
        selected = NULL,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE, 
          title = "None",
          `selected-text-format` = "count > 2",
          size = 5)
      )
      
      
    })
    
    output$tested_cond <- renderValueBox({
      
      watch("boxes")
      
      value <- str_replace_all(r6$primary_condition, pattern = "_", replacement = " ")
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("database"),
        color = "primary",
        footer = p("Primary tested condition", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$significant <- renderValueBox({
      
      watch("boxes")
      
      value <- 23
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("link"),
        color = "primary",
        footer = p("Significant", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$up_reg <- renderValueBox({
      
      watch("boxes")
      
      value <- 21
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("database"),
        color = "primary",
        footer = p("Up-regulated", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$down_reg <- renderValueBox({
      
      watch("boxes")
      
      value <- 23
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("link"),
        color = "primary",
        footer = p("Down-regulated", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$fdr_thr <- renderValueBox({
      
      watch("boxes")
      
      value <- r6$univariate_alpha
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("database"),
        color = "primary",
        footer = p("FDR", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$fc_thr <- renderValueBox({
      
      watch("boxes")
      
      value <- r6$fold_change
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("link"),
        color = "primary",
        footer = p("Fold change", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$volcano_plot <- renderEcharts4r({
      
      watch("plot")
      
      req(input$primary_input)
      req(input$run_statistics)
      
      tests <- c(r6$primary_condition, r6$additiolnal_condition)
      
      r6$plot_volcano(test = tests) 
      
    })
    
    observeEvent(input$run_statistics ,{
      
      req(input$test_input)
      req(input$fc_input)
      req(input$alpha_input)
      req(input$truncation_input)
      req(input$primary_input)
      
      r6$univariate_test_type <- input$test_input
      r6$univariate_paired <- input$paider_input
      r6$fold_change <- input$fc_input
      r6$univariate_alpha <- input$alpha_input
      r6$univariate_p_adj_method <- input$truncation_input
      r6$primary_condition <- input$primary_input
      r6$additiolnal_condition <- input$additiolnal_input
      
      tests <- c(r6$primary_condition, r6$additiolnal_condition)
      
      r6$stat_t_test(
        test = tests,
        fc = r6$fold_change,
        alpha = r6$univariate_alpha,
        p_adj_method = r6$univariate_p_adj_method,
        paired_test = r6$univariate_paired,
        test_type = r6$univariate_test_type
      )
      
      trigger("plot")
      trigger("boxes")
      
    })

  })
}
