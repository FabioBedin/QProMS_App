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
        value = 1410,
        icon = icon("envelope"),
        width = 3, 
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "Missing Values",
        value = 240,
        icon = icon("envelope"),
        width = 3,
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "Condition",
        value = 4,
        icon = icon("envelope"),
        width = 3,
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "Replicate",
        value = 5,
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
        accordion(
          id = "upload_accordion",
          accordionItem(
            title = "Upload a file",
            status = "primary",
            collapsed = FALSE,
            solidHeader = FALSE,
            fileInput(inputId = "upload_file", label = NULL, multiple = FALSE, width = "100%", placeholder = "proteinGroups.txt", accept = ".txt")
          ),
          accordionItem(
            title = "Select intensity",
            status = "primary",
            collapsed = TRUE,
            solidHeader = FALSE,
            radioGroupButtons(
              inputId = "intensity_type",
              label = NULL,
              choices = c("Intensity", "LFQ Intensity", "iBAQ Intensity"),
              selected = "LFQ Intensity",
              justified = TRUE
            )
          ),
          accordionItem(
            title = "Source from",
            status = "primary",
            collapsed = TRUE,
            solidHeader = FALSE,
            radioGroupButtons(
              inputId = "source_type",
              label = NULL,
              choices = c("MaxQuant", "External table"),
              selected = "MaxQuant",
              justified = TRUE
            )
          ),
          accordionItem(
            title = "Organism",
            status = "primary",
            collapsed = TRUE,
            solidHeader = FALSE,
            radioGroupButtons(
              inputId = "organism",
              label = NULL,
              choices = c("Homo Sapiens", "Mus Musculus"),
              selected = "Homo Sapiens",
              justified = TRUE
            )
          )
        ),
        br(),
        div(
          style = "text-align: center; padding: 0.4rem;",
          actionBttn(
            inputId = "start",
            label = "Start", 
            style = "material-flat",
            color = "primary",
            size = "md",
            block = TRUE
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