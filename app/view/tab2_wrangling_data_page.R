box::use(
  shiny[moduleServer, NS, fluidRow, icon, p],
  bs4Dash[tabItem, infoBox, box, boxSidebar]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "wrangling_data",
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
        title = "Average mean",
        value = 4,
        icon = icon("envelope"),
        width = 3,
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "average CV score",
        value = 5,
        icon = icon("envelope"),
        width = 3,
        color = "primary",
        fill = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Protein Counts",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = "filters_sidebar",
          p("Filters options")
        )
      ),
      box(
        title = "Upset Plot",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = "valid_values_sidebar",
          p("Valid values options")
        )
      ),
      box(
        title = "Intensity distribution",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = "normalization_sidebar",
          p("Normalization")
        )
      ),
      box(
        title = "CV score",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Filtred Table",
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