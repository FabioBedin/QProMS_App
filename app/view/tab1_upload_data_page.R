box::use(
  shiny[moduleServer, NS, fluidRow, icon],
  bs4Dash[tabItem, infoBox, box]
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
        height = "70vh"
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