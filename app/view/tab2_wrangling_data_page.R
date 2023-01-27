box::use(
  shiny[moduleServer, NS, fluidRow, icon, h3, selectInput, p, sliderInput, checkboxGroupInput, br],
  bs4Dash[tabItem, infoBox, box, boxSidebar],
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
          h3("Filters options"),
          selectInput(
            inputId = ns("peptides_input"),
            label = "Select peptides type",
            choices = c("Peptides", "Unique peptides", "Razor and unique peptides"),
            selected = "Peptides",
            width = "95%"
          ),
          sliderInput(
            inputId = ns("peptides_slider"),
            label = "Threshold",
            min = 0,
            max = 10,
            value = 2,
            step = 1,
            width = "95%"
          ),
          br(),
          checkboxGroupInput(
            inputId = ns("rev_cont_oibs"),
            label = NULL,
            choices = c("Reverse", "Contaminant", "Identify by site"),
            selected = c("Reverse", "Contaminant", "Identify by site")
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
          width = 50,
          h3("Valid values options"),
          selectInput(
            inputId = ns("valid_values_input"),
            label = "Valid values approach",
            choices = c("At least one group", "Each group", "Toral"),
            selected = "At least one group",
            width = "95%"
          ),
          sliderInput(
            inputId = ns("valid_values_slider"),
            label = "Persentage",
            min = 0,
            max = 100,
            value = 75,
            step = 1,
            width = "95%"
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
          h3("Normalization"),
          selectInput(
            inputId = ns("normalization_input"),
            label = "Normalization strategy",
            choices = c("None", "VSN"),
            selected = "None",
            width = "95%"
          ),
          sliderInput(
            inputId = ns("shift_slider"),
            label = "Down shift",
            min = 1.6,
            max = 2,
            value = 1.8,
            step = 0.1,
            width = "95%"
          ),
          sliderInput(
            inputId = ns("scale_slider"),
            label = "Scale",
            min = 0.1,
            max = 0.5,
            value = 0.3,
            step = 0.1,
            width = "95%"
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