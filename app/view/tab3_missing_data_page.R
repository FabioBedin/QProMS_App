box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, sliderInput, div],
  bs4Dash[tabItem, infoBox, box, boxSidebar],
  shinyWidgets[actionBttn]
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
          div(
            style="padding-right: 0.5rem",
            h3("Imputation"),
            selectInput(
              inputId = ns("imputation_input"),
              label = "imputation strategy",
              choices = c("Mixed", "Perseus"),
              selected = "Mixed"
            ),
            sliderInput(
              inputId = ns("shift_slider"),
              label = "Down shift",
              min = 1.6,
              max = 2,
              value = 1.8,
              step = 0.1
            ),
            sliderInput(
              inputId = ns("scale_slider"),
              label = "Scale",
              min = 0.1,
              max = 0.5,
              value = 0.3,
              step = 0.1
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