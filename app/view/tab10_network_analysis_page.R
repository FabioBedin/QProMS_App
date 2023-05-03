box::use(
  shiny[moduleServer, NS, fluidRow, div, column, br, h4],
  bs4Dash[tabItem, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, bs4Callout],
  shinyWidgets[actionBttn],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "network",
    fluidRow(
      valueBoxOutput(ns("string_thr"), width = 4),
      valueBoxOutput(ns("n_nodes"), width = 4),
      valueBoxOutput(ns("n_edges"), width = 4)
    ),
    fluidRow(
      bs4Callout(
        div(
          style = "display: flex; justify-content: center; align-items: center; gap: 20px",
          div(style = "width: 100%; flex: 1 1 0;"),
          div(style = "width: 100%; flex: 1 1 0;"),
          div(style = "width: 100%; flex: 1 1 0;"),
          div(style = "width: 100%; flex: 1 1 0;"),
          div(
            style = "width: 100%; flex: 1 1 0;",
            actionBttn(
              inputId = ns("generate_network"),
              label = "Generate network",
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE,
              width = "auto"
            )
          )
        ),
        title = NULL,
        status = "info",
        width = 12,
        elevation = 1
      )
    ),
    fluidRow(column(
      7,
      box(
        title = "Network",
        status = "primary",
        width = 12,
        height = 1000,
        maximizable = TRUE,
        sidebar = boxSidebar(
          id = ns("network_sidebar"),
          div(
            style = "padding-right: 0.5rem",
            h4("Network parameters"),
            br(),
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
        # iheatmaprOutput(ns("heatmap"), height = "900px")
      )
    ),
    column(5,
           fluidRow(
             box(
               title = "Table",
               status = "primary",
               width = 12,
               height = 466,
               maximizable = TRUE
               # reactableOutput(ns("table"))
             )
           ),
           fluidRow(
             box(
               title = "Info",
               status = "primary",
               width = 12,
               height = 466,
               maximizable = TRUE
               # echarts4rOutput(ns("profile_plot"))
             )
           )
          )
    )
  )
  
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {

  })
}
