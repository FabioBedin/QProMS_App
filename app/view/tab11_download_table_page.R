box::use(
  shiny[moduleServer, NS, fluidRow, div, downloadHandler, selectInput, hr, icon, observeEvent, h1],
  bs4Dash[tabItem, toast, accordion, accordionItem, updateAccordion],
  shinyWidgets[downloadBttn],
  reactable[reactableOutput, renderReactable, reactable, colDef],
  utils[write.csv, write.table],
  dplyr,
  shiny.emptystate[use_empty_state, EmptyStateManager],
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
          collapsed = FALSE,
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
            style = "display: flex; justify-content: center;",
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
            style = "display: flex; justify-content: center;",
            reactableOutput(ns("functional_table"))
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
            style = "display: flex; justify-content: center;",
            reactableOutput(ns("network_table"))
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
    
    # empty_state_manager <- EmptyStateManager$new(
    #   id = session$ns("stat_table"),
    #   html_content = h1(
    #     "This is example empty state content"
    #   )
    # )
    
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
          data <- r6$print_stat_table()
        }
        
        if(input$stat_table_input == "Multivariate") {
          data <- r6$print_anova_table()
        }
        
        if(input$processed_table_extension == ".xlsx") {
          r6$download_excel(
            table = data,
            name = paste0(input$stat_table_input, "_analysis"),
            handler = file
          )
        }
        
        if(input$processed_table_extension == ".csv") {
          write.csv(data, file)
        }
        
        if(input$processed_table_extension == ".tsv") {
          write.table(data, file, sep = "\t", row.names = FALSE, quote = FALSE)
        }
        
        
      }
    )
    
    
    
    output$stat_table <- renderReactable({
      
      # watch("stat")
      
      if(input$stat_table_input == "Univariate") {
        
        if(is.null(r6$stat_table)) {
          return()
          # empty_state_manager$show()
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

  })
}
