box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, div],
  bs4Dash[tabItem, infoBox, box, boxSidebar],
  shinyWidgets[actionBttn]
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
          id = ns("correlation_sidebar"),
          div(
            style="padding-right: 0.5rem",
            h3("Correlation methods"),
            selectInput(
              inputId = ns("correlation_input"),
              label = NULL,
              choices = c("Pearson", "Kendall", "Spearman"),
              selected = "Pearson"
            ),
            actionBttn(
              inputId = ns("update"),
              label = "Update", 
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          )
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