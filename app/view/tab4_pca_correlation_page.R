box::use(
  shiny[moduleServer, NS, fluidRow, icon, p],
  bs4Dash[tabItem, infoBox, box, boxSidebar]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "pca_correlation",
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
        value = 4,
        icon = icon("envelope"),
        width = 4,
        color = "primary",
        fill = TRUE
      )
    ),
    fluidRow(
      box(
        title = "PCA",
        status = "primary",
        width = 6,
        height = "70vh",
        maximizable = TRUE
      ),
      box(
        title = "Correlation matrix",
        status = "primary",
        width = 6,
        height = "70vh",
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = "correlation_sidebar",
          p("Correlation parameters")
        )
      )
    )
  )
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}