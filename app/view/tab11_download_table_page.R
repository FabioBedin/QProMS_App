box::use(
  shiny[moduleServer, NS, fluidRow, div, downloadHandler, selectInput, hr, icon, observeEvent, h1, uiOutput, renderUI, isolate],
  bs4Dash[tabItem, toast, accordion, accordionItem, updateAccordion],
  shinyWidgets[downloadBttn, pickerInput],
  reactable[reactableOutput, renderReactable, reactable, colDef],
  utils[write.csv, write.table],
  dplyr[`%>%`, select, left_join],
  gargoyle[watch],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "download",
    # use_empty_state(),
    fluidRow(
      width = 11,
      accordion(
        id = ns("downloads_params"),
        accordionItem(
          title = "Processed tables",
          status = "primary",
          collapsed = TRUE,
          solidHeader = FALSE,
          div(
            style = "display: flex; justify-content: center; gap: 5rem; align-items: start;",
            div(
              style = "width: 100%; flex: 1 1 0;",
              selectInput(
                inputId = ns("processed_table_input"),
                label = "Table",
                choices = c("Filtred", "Normalized", "Imputed"),
                selected = "Filtred"
              )
            ),
            div(
              style = "width: 100%; flex: 1 1 0;",
              selectInput(
                inputId = ns("processed_table_extension"),
                label = "File extension ",
                choices = c(".xlsx", ".csv", ".tsv"),
                selected = ".xlsx"
              )
            ),
            div(
              style = "width: 100%; flex: 1 1 0; padding-top: 1.6rem;",
              downloadBttn(
                outputId  = ns("download_processed_table"),
                label = "Download", 
                style = "material-flat",
                color = "primary",
                size = "md",
                block = TRUE
              )
            )
          ),
          hr(),
          div(
            style = "padding: 1rem",
            reactableOutput(ns("processed_table"))
          )
        ),
        accordionItem(
          title = "Statistic tables",
          status = "primary",
          collapsed = TRUE,
          solidHeader = FALSE,
          div(
            style = "display: flex; justify-content: center; gap: 5rem; align-items: start;",
            div(
              style = "width: 100%; flex: 1 1 0;",
              selectInput(
                inputId = ns("stat_table_input"),
                label = "Table",
                choices = c("Univariate", "Multivariate"),
                selected = "Univariate"
              )
            ),
            div(
              style = "width: 100%; flex: 1 1 0;",
              uiOutput(ns("add_column"))
            ),
            div(
              style = "width: 100%; flex: 1 1 0;",
              selectInput(
                inputId = ns("stat_table_extension"),
                label = "File extension ",
                choices = c(".xlsx", ".csv", ".tsv"),
                selected = ".xlsx"
              )
            ),
            div(
              style = "width: 100%; flex: 1 1 0; padding-top: 1.6rem;",
              downloadBttn(
                outputId  = ns("download_stat_table"),
                label = "Download", 
                style = "material-flat",
                color = "primary",
                size = "md",
                block = TRUE
              )
            )
          ),
          hr(),
          div(
            style = "padding: 1rem",
            reactableOutput(ns("stat_table"))
          )
        ),
        accordionItem(
          title = "Network tables",
          status = "primary",
          collapsed = TRUE,
          solidHeader = FALSE,
          div(
            style = "display: flex; justify-content: center; gap: 5rem; align-items: start;",
            div(
              style = "width: 100%; flex: 1 1 0;",
              selectInput(
                inputId = ns("network_table_input"),
                label = "Table",
                choices = c("Nodes", "Edges"),
                selected = "Nodes"
              )
            ),
            div(
              style = "width: 100%; flex: 1 1 0;",
              selectInput(
                inputId = ns("network_table_extension"),
                label = "File extension ",
                choices = c(".xlsx", ".csv", ".tsv"),
                selected = ".xlsx"
              )
            ),
            div(
              style = "width: 100%; flex: 1 1 0; padding-top: 1.6rem;",
              downloadBttn(
                outputId  = ns("download_network_table"),
                label = "Download", 
                style = "material-flat",
                color = "primary",
                size = "md",
                block = TRUE
              )
            )
          ),
          hr(),
          div(
            style = "padding: 1rem",
            reactableOutput(ns("network_table"))
          )
        ),
        accordionItem(
          title = "Functional tables",
          status = "primary",
          collapsed = TRUE,
          solidHeader = FALSE,
          div(
            style = "display: flex; justify-content: center; gap: 5rem; align-items: start;",
            div(
              style = "width: 100%; flex: 1 1 0;",
              selectInput(
                inputId = ns("functional_table_input"),
                label = "Table",
                choices = c("ORA", "GSEA"),
                selected = "ORA"
              )
            ),
            div(
              style = "width: 100%; flex: 1 1 0;",
              selectInput(
                inputId = ns("functional_table_extension"),
                label = "File extension ",
                choices = c(".xlsx", ".csv", ".tsv"),
                selected = ".xlsx"
              )
            ),
            div(
              style = "width: 100%; flex: 1 1 0; padding-top: 1.6rem;",
              downloadBttn(
                outputId  = ns("download_functional_table"),
                label = "Download", 
                style = "material-flat",
                color = "primary",
                size = "md",
                block = TRUE
              )
            )
          ),
          hr(),
          div(
            style = "padding: 1rem",
            reactableOutput(ns("functional_table"))
          )
        ),
        width = 12
      )
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    output$add_column <- renderUI({
      
      test <- colnames(r6$raw_data_unique)
      
      pickerInput(
        inputId = session$ns("extra_cols"),
        label = "Add extra columns",
        choices = test,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE, 
          title = "None",
          `selected-text-format` = "count > 2",
          size = 5)
      )
      
      
    })
    
    
    output$download_processed_table <- downloadHandler(
      filename = function() {
        paste0(input$processed_table_input, "_table_", Sys.Date(), input$processed_table_extension)
      },
      content = function(file) {
        
        if(input$processed_table_input == "Filtred") {
          data <- r6$filtered_data
        }
        
        if(input$processed_table_input == "Normalized") {
          data <- r6$normalized_data
        }
        
        if(input$processed_table_input == "Imputed") {
          data <- r6$imputed_data
        }
        
        if(input$processed_table_extension == ".xlsx") {
          r6$download_excel(
            table = r6$print_table(data, df = TRUE),
            name = paste0(input$processed_table_input, "_data"),
            handler = file
          )
        }
          
        if(input$processed_table_extension == ".csv") {
          write.csv(r6$print_table(data, df = TRUE), file)
        }
        
        if(input$processed_table_extension == ".tsv") {
          write.table(r6$print_table(data, df = TRUE), file, sep = "\t", row.names = FALSE, quote = FALSE)
        }
        
        
      }
    )
    
    output$processed_table <- renderReactable({
      
      if(input$processed_table_input == "Filtred") {
        data <- r6$filtered_data
      }
      
      if(input$processed_table_input == "Normalized") {
        data <- r6$normalized_data
      }
      
      if(input$processed_table_input == "Imputed") {
        data <- r6$imputed_data
      }
      
      r6$print_table(data)
      
    })
    
    output$download_stat_table <- downloadHandler(
      filename = function() {
        paste0(input$stat_table_input, "_table_", Sys.Date(), input$stat_table_extension)
      },
      content = function(file) {
        
        if(input$stat_table_input == "Univariate") {
          data0 <- r6$print_stat_table()
        }
        
        if(input$stat_table_input == "Multivariate") {
          data0 <- r6$print_anova_table()
        }
        
        extra_clos_data <- r6$raw_data_unique %>% 
          select(gene_names, isolate(input$extra_cols))
        
        data <- left_join(data0, extra_clos_data, by = "gene_names")
        
        if(input$stat_table_extension == ".xlsx") {
          r6$download_excel(
            table = data,
            name = paste0(input$stat_table_input, "_analysis"),
            handler = file
          )
        }
        
        if(input$stat_table_extension == ".csv") {
          write.csv(data, file)
        }
        
        if(input$stat_table_extension == ".tsv") {
          write.table(data, file, sep = "\t", row.names = FALSE, quote = FALSE)
        }
        
        
      }
    )
    
    
    output$stat_table <- renderReactable({
      
      watch("stat")
      watch("anova")
      
      if(input$stat_table_input == "Univariate") {
        
        if(is.null(r6$stat_table)) {
          return()
        } else {
          reactable(
            r6$print_stat_table(),
            searchable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            wrap = FALSE,
            height = "auto",
            defaultColDef = colDef(align = "center", minWidth = 200),
            columns = list(
              gene_names = colDef(
                name = "Gene names",
                sticky = "left",
                style = list(borderRight  = "1px solid #eee")
              )
            )
          )
        }
      } else {
        
        if(is.null(r6$anova_table)) {
          return()
        } else {
          reactable(
            r6$print_anova_table(),
            searchable = TRUE,
            resizable = TRUE,
            wrap = FALSE,
            highlight = TRUE,
            height = "auto",
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
        }
        
      }
      
    })
    
    ## network
    
    output$download_network_table <- downloadHandler(
      filename = function() {
        paste0(input$network_table_input, "_table_", Sys.Date(), input$network_table_extension)
      },
      content = function(file) {
        
        if(input$network_table_input == "Nodes") {
          data <- r6$print_nodes(isolate_nodes = FALSE, score_thr = 0)
        }
        
        if(input$network_table_input == "Edges") {
          data <- r6$print_edges(score_thr = 0, selected_nodes = NULL)
        }
        
        if(input$network_table_extension == ".xlsx") {
          r6$download_excel(
            table = data,
            name = paste0(input$network_table_input, "_analysis"),
            handler = file
          )
        }
        
        if(input$network_table_extension == ".csv") {
          write.csv(data, file)
        }
        
        if(input$network_table_extension == ".tsv") {
          write.table(data, file, sep = "\t", row.names = FALSE, quote = FALSE)
        }
        
        
      }
    )
    
    
    output$network_table <- renderReactable({
      
      watch("ppi_network")
      
      if(input$network_table_input == "Nodes") {
        
        if(is.null(r6$nodes_table)) {
          return()
        } else {
          reactable(
            r6$print_nodes(isolate_nodes = FALSE, score_thr = 0),
            searchable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            wrap = FALSE,
            paginateSubRows = TRUE,
            height = "auto",
            columns = list(
              gene_names = colDef(name = "Node"),
              category = colDef(name = "Category"),
              p_val = colDef(align = "center", name = "-log(p.value)"),
              p_adj = colDef(align = "center", name = "-log(p.adj)")
            )
          )
        }
      } else {
        
        if(is.null(r6$edges_table)) {
          return()
        } else {
          reactable(
            r6$print_edges(score_thr = 0, selected_nodes = NULL),
            searchable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            wrap = FALSE,
            height = "auto",
            columns = list(
              target = colDef(aggregate = "unique", minWidth = 100, name = "Target"),
              source = colDef(name = "Source"),
              database = colDef(name = "Database"),
              complex = colDef(minWidth = 300, name = "Complex"),
              score = colDef(align = "center", name = "Score")
            )
          )
        }
        
      }
      
    })
    
    ## functional
    
    output$download_functional_table <- downloadHandler(
      filename = function() {
        paste0(input$functional_table_input, "_table_", Sys.Date(), input$functional_table_extension)
      },
      content = function(file) {
        
        if(input$functional_table_input == "ORA") {
          data <- r6$ora_table_all_download
        }
        
        if(input$functional_table_input == "GSEA") {
          data <- r6$gsea_table_all_download
        }
        
        if(input$functional_table_extension == ".xlsx") {
          r6$download_excel(
            table = data,
            name = paste0(input$functional_table_input, "_analysis"),
            handler = file
          )
        }
        
        if(input$functional_table_extension == ".csv") {
          write.csv(data, file)
        }
        
        if(input$functional_table_extension == ".tsv") {
          write.table(data, file, sep = "\t", row.names = FALSE, quote = FALSE)
        }
        
        
      }
    )
    
    
    output$functional_table <- renderReactable({
      
      watch("ppi_network")
      
      if(input$functional_table_input == "ORA") {
        
        if(is.null(r6$ora_table_all_download)) {
          return()
        } else {
          reactable(
            r6$ora_table_all_download,
            searchable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            wrap = FALSE,
            paginateSubRows = TRUE,
            height = "auto",
            columns = list(
              ONTOLOGY = colDef(align = "center", name = "Ontology"),
              ID = colDef(
                align = "center",
                sticky = "left",
                minWidth = 150,
                style = list(borderRight  = "1px solid #eee")
              ), 
              group = colDef(minWidth = 250),
              fold_enrichment = colDef(minWidth = 150, align = "center", name = "Fold Enrichment"),
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
      } else {
        
        if(is.null(r6$gsea_table_all_download)) {
          return()
        } else {
          reactable(
            r6$gsea_table_all_download,
            searchable = TRUE,
            resizable = TRUE,
            wrap = FALSE,
            highlight = TRUE,
            height = "auto",
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
        }
        
      }
      
    })

  })
}
