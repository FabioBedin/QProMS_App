box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, sliderInput, checkboxGroupInput, br, div],
  bs4Dash[tabItem, infoBox, box, boxSidebar],
  shinyWidgets[actionBttn]
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
          id = ns("filters_sidebar"),
          div(
            style="padding-right: 0.5rem",
            h3("Filters options"),
            selectInput(
              inputId = ns("peptides_input"),
              label = "Select peptides type",
              choices = c("Peptides", "Unique peptides", "Razor and unique peptides"),
              selected = "Peptides"
            ),
            sliderInput(
              inputId = ns("peptides_slider"),
              label = "Threshold",
              min = 0,
              max = 10,
              value = 2,
              step = 1
            ),
            br(),
            checkboxGroupInput(
              inputId = ns("rev_cont_oibs"),
              label = NULL,
              choices = c("Reverse", "Contaminant", "Identify by site"),
              selected = c("Reverse", "Contaminant", "Identify by site")
            ),
            actionBttn(
              inputId = ns("update_filters"),
              label = "Update", 
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          )
        )
      ),
      box(
        title = "Upset Plot",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = ns("valid_values_sidebar"),
          div(
            style="padding-right: 0.5rem",
            h3("Valid values options"),
            selectInput(
              inputId = ns("valid_values_input"),
              label = "Valid values approach",
              choices = c("At least one group", "Each group", "Toral"),
              selected = "At least one group"
            ),
            sliderInput(
              inputId = ns("valid_values_slider"),
              label = "Persentage",
              min = 0,
              max = 100,
              value = 75,
              step = 1
            ),
            actionBttn(
              inputId = ns("update_valid_values"),
              label = "Update", 
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          )
        )
      ),
      box(
        title = "Intensity distribution",
        status = "primary",
        width = 3,
        height = 500,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = ns("normalization_sidebar"),
          div(
            style="padding-right: 0.5rem",
            h3("Normalization"),
            selectInput(
              inputId = ns("normalization_input"),
              label = "Normalization strategy",
              choices = c("None", "VSN"),
              selected = "None"
            ),
            actionBttn(
              inputId = ns("update_normalization"),
              label = "Update", 
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          )
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