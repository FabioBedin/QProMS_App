box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput, icon, fluidRow],
  bs4Dash[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    title = "QProMS",
    fullscreen = TRUE,
    header = dashboardHeader(
      title =  dashboardBrand(
        title = "QProMS",
        color = "primary",
        href = NULL,
        image = NULL
      )
    ),
    sidebar = dashboardSidebar(
      tags$br(),
      # sidebarHeader("Workflow"),
      # tags$br(),
      sidebarMenu(
        id = "sidebarMenu",
        menuItem(
          text = "Upload Data",
          tabName = "upload_data",
          icon = icon("upload")
        ),
        menuItem(
          text = "Wrangling Data",
          tabName = "wrangling_data",
          icon = icon("filter")
        ),
        menuItem(
          text = "Missing Data",
          tabName = "missing_data",
          icon = icon("magnifying-glass")
        ),
        menuItem(
          text = "Imputation",
          tabName = "imputation",
          icon = icon("wand-magic-sparkles")
        ),
        menuItem(
          text = "Statistics",
          tabName = "statistics",
          icon = icon("circle-half-stroke")
        )
      )
    ),
    controlbar = dashboardControlbar(),
    footer = dashboardFooter(),
    body = dashboardBody(
      tabItems(
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
              height = 700
            ),
            box(
              title = "Experimental Design",
              status = "primary",
              width = 9,
              height = 700,
              maximizable = TRUE
            )
          )
        ),
        tabItem(
          tabName = "wrangling_data"
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
