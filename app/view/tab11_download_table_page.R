box::use(
  shiny[moduleServer, NS, fluidRow, div, downloadHandler],
  bs4Dash[tabItem, toast, accordion, accordionItem, updateAccordion],
  shinyWidgets[downloadBttn],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "download",
    fluidRow(
      
      width = 11,
      accordion(
        id = ns("advance_params"),
        accordionItem(
          title = "Processed tables",
          status = "primary",
          collapsed = FALSE,
          solidHeader = TRUE,
          div(
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
        accordionItem(
          title = "Statistic tables",
          status = "primary",
          collapsed = TRUE,
          solidHeader = TRUE
        ),
        accordionItem(
          title = "Functional tables",
          status = "primary",
          collapsed = TRUE,
          solidHeader = TRUE
        ),
        accordionItem(
          title = "Network tables",
          status = "primary",
          collapsed = TRUE,
          solidHeader = TRUE
        ),
        width = 12
      )
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    output$download_table <- downloadHandler(
      filename = function() {
        paste0("table", ".xlsx", sep="")
      },
      content = function(file) {
        r6$download_excel(table = r6$print_table(r6$filtered_data, df = TRUE), name = "filtred_data", handler = file)
      }
    )
    

  })
}
