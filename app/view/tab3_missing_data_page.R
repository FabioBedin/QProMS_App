box::use(
  shiny[moduleServer, NS, fluidRow, icon, p],
  bs4Dash[tabItem, infoBox, box, boxSidebar]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "missing_data",
    fluidRow(
      infoBox(
        title = "N° Proteins",
        value = 1370,
        icon = icon("envelope"),
        width = 3, 
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "Missing Values",
        value = 170,
        icon = icon("envelope"),
        width = 3,
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "MNAR",
        value = 4,
        icon = icon("envelope"),
        width = 3,
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "MAR",
        value = 5,
        icon = icon("envelope"),
        width = 3,
        color = "primary",
        fill = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Missing data Counts",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE
      ),
      box(
        title = "Missing data distribution",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE
      ),
      box(
        title = "Effect of imputation: Before",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE
      ),
      box(
        title = "Effect of imputation: After",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = "imputation_sidebar",
          p("Imputation parameters")
        )
      )
    ),
    fluidRow(
      box(
        title = "Imputed Table",
        status = "primary",
        width = 12,
        maximizable = TRUE,
        collapsible = TRUE
      )
    )
  )
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}