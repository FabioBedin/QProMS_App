box::use(
  shiny[moduleServer, NS, fluidRow, div, column, icon, h3, h4, p, selectInput, br, numericInput, h5, observeEvent, req],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox],
  InteractiveComplexHeatmap[originalHeatmapOutput, subHeatmapOutput, makeInteractiveComplexHeatmap, HeatmapInfoOutput, InteractiveComplexHeatmapOutput],
  shinyWidgets[actionBttn, prettyCheckbox, pickerInput],
  ComplexHeatmap[draw]
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
                    label = "Clustering distance",
                    choices = c("Euclidean" = "euclidean", "Pearson" = "pearson", "Kendall" = "kendall", "Spearman" = "spearman"),
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
          # InteractiveComplexHeatmapOutput()
          originalHeatmapOutput(heatmap_id = ns("heatmap_out"), height = 800, containment = TRUE, title = NULL)
        )
      ), 
      column(5,
        fluidRow(
          box(
            title = "Sub-heatmap",
            status = "primary",
            width = 12,
            height = 466,
            maximizable = TRUE,
            subHeatmapOutput(heatmap_id = ns("heatmap_out"), containment = TRUE, title = NULL)
          )
        ),
        fluidRow(
          box(
            title = "Table",
            status = "primary",
            width = 12,
            height = 466,
            maximizable = TRUE,
            HeatmapInfoOutput(heatmap_id = ns("heatmap_out"), title = NULL)
          )
        )
      )
    )
  )

}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    output$significant <- renderValueBox({
      
      # watch("stat")

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
      
      # watch("stat")
      
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
      
      # watch("stat")
      
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
    
    observeEvent(input$run_statistics ,{
      
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
      
      r6$stat_anova(alpha = r6$anova_alpha, p_adj_method = r6$anova_p_adj_method)
      
      # ht = r6$plot_heatmap(
      #   clustering_distance = r6$anova_clust_distance,
      #   clustering_method = r6$anova_clust_method,
      #   n_cluster = r6$clusters_number,
      #   reorder = r6$anova_reorder
      # )
      
      # draw(ht)
      
      # print(r6$anova_table)
      
      # if(!is.null(ht)){
      makeInteractiveComplexHeatmap(input, output, session, r6$plot_heatmap(
        clustering_distance = r6$anova_clust_distance,
        clustering_method = r6$anova_clust_method,
        n_cluster = r6$clusters_number,
        reorder = r6$anova_reorder
      ), "heatmap_out")
      # }
      
      
      
      # trigger("stat")
    })


  })
}
