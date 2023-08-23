box::use(
  shiny[moduleServer, NS],
  bs4Dash[tabItem, infoBox, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, toast],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "report",
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    

  })
}
