box::use(
  shiny[moduleServer, NS, fluidRow, div, column, icon, h3, h4, p, selectInput, br, numericInput, h5, h6, observeEvent, req, reactive, uiOutput, renderUI, isolate, observe],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, bs4Callout],
  shinyWidgets[actionBttn, prettyCheckbox, pickerInput],
  dplyr[filter, `%>%`, pull, distinct],
  iheatmapr[...],
  gargoyle[init, watch, trigger],
  shinyjqui[orderInput],
  reactable[reactableOutput, renderReactable, reactable, colDef, getReactableState],
  echarts4r[echarts4rOutput, renderEcharts4r, e_chart],
  shinyGizmo[conditionalJS, jsCalls],
  waiter[Waiter, spin_5, withWaiter],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "statistics_multi",
    fluidRow(
      valueBoxOutput(ns("significant"), width = 4),
      valueBoxOutput(ns("fdr_thr"), width = 4),
      valueBoxOutput(ns("clusters"), width = 4)
    ),
    fluidRow(
      bs4Callout(
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
          ),
          div(
            style = "width: 100%; flex: 1 1 0;",
            selectInput(
              inputId = ns("clust_method"),
              label = "Clustering method",
              choices = c("Hierarchical Clustering" = "hclust", "K-means Clustering" = "kmeans"),
              selected = "hclust", 
              width = "auto"
            )
          ),
          div(
            style = "width: 100%; flex: 1 1 0;",
            div(
              style = "display: flex; justify-content: center; align-items: center; gap: 20px",
              div(
                style = "width: 100%; flex: 3 1 0;",
                numericInput(
                  inputId = ns("n_cluster_input"),
                  label = "N° of clusters",
                  value = 2,
                  min = 0,
                  step = 1, 
                  width = "auto"
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0; text-align: center; margin-top: 1.5rem;",
                prettyCheckbox(
                  inputId = ns("zscore_input"),
                  label = "Z-score", 
                  value = TRUE,
                  shape = "curve", 
                  width = "auto"
                )
              ),
            )
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
      column(7,
        box(
          title = "Heatmap",
          status = "primary",
          width = 12,
          height = 1000,
          maximizable = TRUE,
          sidebar = boxSidebar(
            id = ns("heatmap_sidebar"),
            div(
              style = "padding-right: 0.5rem",
              h4("Heatmap parameters"),
              prettyCheckbox(
                inputId = ns("reorder_input"),
                label = "Order column manually", 
                value = FALSE,
                shape = "curve"
              ),
              conditionalJS(
                uiOutput(ns("profile_order_ui")),
                condition = "input.reorder_input",
                jsCall = jsCalls$show(),
                ns = ns
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
          conditionalJS(
            div(
              echarts4rOutput(ns("fake_heatmal_plot"), height = "1")
            ),
            condition = "input.run_statistics === null",
            jsCall = jsCalls$show(),
            ns = ns
          ),
          conditionalJS(
            div(
              iheatmaprOutput(ns("heatmap"), height = "900px")
            ),
            condition = "input.run_statistics > 0",
            jsCall = jsCalls$show(),
            ns = ns
          )
        )
      ), 
      column(5,
        fluidRow(
          box(
            title = "Result table",
            status = "primary",
            width = 12,
            height = 466,
            maximizable = TRUE,
            reactableOutput(ns("table"))
          )
        ),
        fluidRow(
          box(
            title = "Protein profile plot",
            status = "primary",
            width = 12,
            height = 466,
            maximizable = TRUE,
            echarts4rOutput(ns("profile_plot"))
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Clusters profile plots",
        id = ns("multi"),
        status = "primary",
        width = 12,
        maximizable = TRUE,
        collapsible = TRUE,
        uiOutput(ns("cluster_profile_plots"))
        # echarts4rOutput(ns("fake_loading_plot"), height = "1")
      )
    )
  )

}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    init("anova", "profile")
    
    defaultW <- getOption("warn") 
    
    options(warn = -1) 
    
    w <- Waiter$new(html = spin_5(), color = "#adb5bd")
    
    output$profile_order_ui <- renderUI({
      
      watch("anova")
      
      conds <- r6$expdesign %>% distinct(condition) %>% pull()
      
      orderInput(
        inputId = session$ns("profile_order"),
        label = "Drag and drop",
        items = conds
      )
      
      
    })
    
    output$significant <- renderValueBox({
      
      watch("anova")

      if(is.null(r6$anova_table)){
        value <- 0
      }else{
        sig <- r6$anova_table %>%
          filter(significant) %>%
          nrow()

        total <- nrow(r6$anova_table)

        value <- paste0(sig, " out of ", total)
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
      
      watch("anova")
      
      if(is.null(r6$anova_table)){
        value <- "Undefinded"
      }else{
        value <- r6$anova_alpha
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
    
    output$clusters <- renderValueBox({
      
      watch("anova")
      
      if(is.null(r6$anova_table)){
        value <- "Undefinded"
      }else{
        value <- r6$clusters_number
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("crosshairs"),
        color = "primary",
        footer = p("N° of clusters", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    observeEvent(input$run_statistics, {
      
      w$show()
      
      req(input$alpha_input)
      req(input$truncation_input)
      req(input$clust_method)
      req(input$n_cluster_input)
      
      r6$anova_alpha <- as.double(input$alpha_input)
      r6$anova_p_adj_method <- input$truncation_input
      r6$anova_clust_method <- input$clust_method
      r6$z_score <- input$zscore_input
      r6$clusters_number <- as.double(input$n_cluster_input)
      r6$anova_manual_order <- input$reorder_input
      
      r6$stat_anova(alpha = r6$anova_alpha, p_adj_method = r6$anova_p_adj_method)
      
      trigger("anova")
      trigger("profile")
      
      w$hide()
    })
    
    observeEvent(input$update, {
      
      w$show()
      
      req(input$run_statistics)
      
      r6$anova_manual_order <- input$reorder_input
      
      r6$stat_anova(alpha = r6$anova_alpha, p_adj_method = r6$anova_p_adj_method)
      
      trigger("anova")
      trigger("profile")
      
      w$hide()
    })
    
    
    output$heatmap <- renderIheatmap({
      
      watch("anova")
      
      req(input$run_statistics)

      if(is.null(input$run_statistics)){
        return(NULL)
      }else{
        r6$plot_heatmap(
          z_score = r6$z_score,
          clustering_method = r6$anova_clust_method,
          n_cluster = r6$clusters_number,
          manual_order = r6$anova_manual_order,
          order = isolate(input$profile_order)
        )
      }

    })
    
    output$table <- renderReactable({
      
      watch("anova")
      
      req(input$run_statistics)
      
      table <- r6$print_anova_table()
      
      reactable(
        table,
        searchable = TRUE,
        resizable = TRUE,
        wrap = FALSE,
        highlight = TRUE,
        defaultPageSize = 7,
        height = "auto",
        selection = "multiple",
        onClick = "select",
        defaultSelected = 1,
        columns = list(
          gene_names = colDef(align = "center", name = "Gene names"),
          p_val = colDef(align = "center", name = "-log(p.value)"),
          p_adj = colDef(align = "center", name = "-log(p.adj)"),
          cluster = colDef(align = "center", name = "Cluster"),
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
    
    gene_selected <- reactive(getReactableState("table", "selected"))
    
    output$profile_plot <- renderEcharts4r({
      
      req(input$run_statistics)
      
      watch("profile")
      
      table <- r6$print_anova_table()
      
      if(is.null(gene_selected())){
        return(NULL)
      }else{
        highlights <- table[gene_selected(), ] %>% 
          pull(gene_names)
        
        r6$plot_protein_profile(gene = highlights)
      }
      
      
      
    })
    
    # output$fake_loading_plot <- renderEcharts4r({
    #   
    #   req(input$run_statistics)
    #   
    #   watch("profile")
    #   
    #   req(input$profile_plot)
    #   
    #   e_chart()
    #   
    #   # w$hide()
    #   
    # })
    
    output$fake_heatmap_plot <- renderEcharts4r({
      
      e_chart()
      
    })

    
    output$cluster_profile_plots <- renderUI({
      
      watch("profile")
      
      req(input$run_statistics)
      
      if(r6$clusters_number > 0){
        r6$plot_cluster_profile()
      }else{
        return(NULL)
      }
      
      
    })
    
    options(warn = defaultW)


  })
}
