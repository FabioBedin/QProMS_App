box::use(
  shiny[moduleServer, NS, fluidRow, div, column, icon, h3, h4, p, selectInput, br, numericInput, h5, observeEvent, req, reactive, uiOutput, renderUI],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox],
  shinyWidgets[actionBttn, prettyCheckbox, pickerInput],
  dplyr[filter, `%>%`, pull, distinct],
  plotly[renderPlotly, plotlyOutput],
  gargoyle[init, watch, trigger],
  shinyjqui[orderInput],
  reactable[reactableOutput, renderReactable, reactable, colDef, getReactableState],
  echarts4r[echarts4rOutput, renderEcharts4r],
  # waiter[Waiter, spin_5, Waitress],
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
      column(7,
        box(
          title = "Heatmap",
          status = "primary",
          width = 12,
          height = 1000,
          maximizable = TRUE,
          sidebar = boxSidebar(
            id = ns("heatmap_sidebar"),
            startOpen = TRUE,
            div(
              style = "padding-right: 0.5rem",
              h4("Test parameters"),
              div(
                style = "display: flex; justify-content: center; gap: 20px; align-items: center;",
                div(
                  style = "width: 100%;",
                  selectInput(
                    inputId = ns("alpha_input"),
                    label = "Alpha",
                    choices = c(0.05, 0.01),
                    selected = 0.05
                  )
                ),
                div(
                  style = "width: 100%;",
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
              h4("Heatmap parameters"),
              div(
                style = "display: flex; justify-content: center; gap: 20px; align-items: center;",
                div(
                  style = "width: 100%;",
                  selectInput(
                    inputId = ns("clust_distance"),
                    label = "Distance method",
                    choices = c(
                      "Euclidean" = "euclidean",
                      "Maximum" = "maximum",
                      "Manhattan" = "manhattan",
                      "Canberra" = "canberra",
                      "Binary" = "binary",
                      "Minkowski" = "minkowski"
                    ), 
                    selected = "euclidean"
                  )
                ),
                div(
                  style = "width: 100%;",
                  selectInput(
                    inputId = ns("clust_method"),
                    label = "Clustering method",
                    choices = c("complete", "single", "average", "median", "centroid"),
                    selected = "complete"
                  )
                )
              ),
              h5("N° of cluster"),
              div(
                style = "display: flex; justify-content: center; gap: 20px; align-items: center;",
                div(
                  style = "width: 100%; flex: 2 1 0;",
                  numericInput(
                    inputId = ns("n_cluster_input"),
                    label = NULL,
                    value = 0,
                    min = 0,
                    step = 1
                  )
                ),
                div(
                  style = "width: 100%; flex: 1 1 0; text-align: center;",
                  prettyCheckbox(
                    inputId = ns("reorder_input"),
                    label = "Reorder", 
                    value = FALSE,
                    shape = "curve"
                  )
                ), 
                div(
                  style = "width: 100%; flex: 1 1 0; text-align: center;",
                  prettyCheckbox(
                    inputId = ns("show_name"),
                    label = "Row names", 
                    value = FALSE,
                    shape = "curve"
                  )
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
          plotlyOutput(ns("heatmap"), height = "900px")
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
            title = "Profile plot",
            status = "primary",
            width = 12,
            height = 466,
            maximizable = TRUE,
            sidebar = boxSidebar(
              id = ns("profle_sidebar"),
              div(
                style = "padding-right: 0.5rem",
                h4("Define order"),
                uiOutput(ns("profile_order_ui")),
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
            echarts4rOutput(ns("profile_plot"))
          )
        )
      )
    )
  )

}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    init("anova", "profile")
    
    # waitress <- Waitress$new(session$ns("heatmap"))
    
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
      
      req(input$alpha_input)
      req(input$truncation_input)
      req(input$clust_distance)
      req(input$clust_method)
      req(input$n_cluster_input)
      
      r6$anova_alpha <- as.double(input$alpha_input)
      r6$anova_p_adj_method <- input$truncation_input
      r6$anova_clust_distance <- input$clust_distance
      r6$anova_clust_method <- input$clust_method
      r6$clusters_number <- as.double(input$n_cluster_input)
      r6$anova_reorder <- input$reorder_input
      r6$anova_profile_order <- input$profile_order
      
      # waitress$start()

      
      r6$stat_anova(alpha = r6$anova_alpha, p_adj_method = r6$anova_p_adj_method)
      # waitress$hide()
      
      trigger("anova")
      trigger("profile")
    })
    
    
    output$heatmap <- renderPlotly({
      
      watch("anova")
      
      req(input$run_statistics)

      r6$plot_heatmap(
        distance_method = r6$anova_clust_distance,
        clustering_method = r6$anova_clust_method,
        n_cluster = r6$clusters_number,
        reorder = r6$anova_reorder,
        show_names = input$show_name
      )


    })
    
    output$table <- renderReactable({
      
      watch("anova")
      
      req(input$run_statistics)
      
      table <- r6$print_anova_table()
      
      reactable(
        table,
        searchable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultPageSize = 7,
        height = "auto",
        selection = "single",
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
        
        r6$plot_cluster_profile(gene = highlights, order = r6$anova_profile_order)
      }
      
    })
    
    observeEvent(input$update, {
      
      req(input$run_statistics)
      req(input$profile_order)
      
      r6$anova_profile_order <- input$profile_order
      
      trigger("profile")
      
    })


  })
}
