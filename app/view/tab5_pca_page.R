box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, div],
  bs4Dash[tabItem, infoBox, box, boxSidebar],
  shinyWidgets[actionBttn]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "pca",
    fluidRow(
      infoBox(
        title = "PC1",
        value = 1370,
        icon = icon("envelope"),
        width = 4, 
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "PC2",
        value = 170,
        icon = icon("envelope"),
        width = 4,
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "PC3",
        value = 17,
        icon = icon("envelope"),
        width = 4,
        color = "primary",
        fill = TRUE
      )
    ),
    fluidRow(
      box(
        title = "2D PCA",
        status = "primary",
        width = 6,
        height = "70vh",
        maximizable = TRUE
      ),
      box(
        title = "3D PCA",
        status = "primary",
        width = 6,
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