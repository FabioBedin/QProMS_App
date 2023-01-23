box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput],
  bs4Dash[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    title = "QProMS",
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    controlbar = dashboardControlbar(),
    footer = dashboardFooter(),
    body = dashboardBody()
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
