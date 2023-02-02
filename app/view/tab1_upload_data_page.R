box::use(
  shiny[moduleServer, NS, fluidRow, icon, fileInput, div, br],
  bs4Dash[tabItem, infoBox, box, accordion, accordionItem],
  shinyWidgets[radioGroupButtons, actionBttn]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "upload_data",
    fluidRow(
      infoBox(
        title = "N° Proteins",
        value = 0,
        icon = icon("envelope"),
        width = 3, 
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "Missing Values",
        value = 0,
        icon = icon("envelope"),
        width = 3,
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "Condition",
        value = 0,
        icon = icon("envelope"),
        width = 3,
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "Replicate",
        value = 0,
        icon = icon("envelope"),
        width = 3,
        color = "primary",
        fill = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Upload",
        status = "primary",
        width = 3,
        height = "70vh",
        br(),
        fileInput(
          inputId = ns("upload_file"),
          label = NULL,
          multiple = FALSE,
          width = "100%",
          placeholder = "proteinGroups.txt",
          accept = ".txt"
        ),
        br(),
        radioGroupButtons(
          inputId = ns("intensity_type"),
          label = NULL,
          choices = c("Intensity", "LFQ Intensity", "iBAQ Intensity"),
          selected = "LFQ Intensity",
          justified = TRUE
        ),
        br(),
        radioGroupButtons(
          inputId = ns("source_type"),
          label = NULL,
          choices = c("MaxQuant", "External table"),
          selected = "MaxQuant",
          justified = TRUE
        ),
        br(),
        radioGroupButtons(
          inputId = ns("organism"),
          label = NULL,
          choices = c("Homo Sapiens", "Mus Musculus"),
          selected = "Homo Sapiens",
          justified = TRUE
        ),
        br(),
        br(),
        div(
          style = "display: flex; justify-content: center;",
          div(
            style = "width: 100%; margin-right: 10px;",
            actionBttn(
              inputId = ns("start"),
              label = "Start", 
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          ),
          div(
            style = "width: 100%; margin-left: 10px;",
            actionBttn(
              inputId = ns("tutorial"),
              label = "Tutorial", 
              style = "material-flat",
              color = "default",
              size = "md",
              block = TRUE
            )
          )
        )
      ),
      box(
        title = "Experimental Design",
        status = "primary",
        width = 9,
        height = "70vh",
        maximizable = TRUE
      )
    )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}