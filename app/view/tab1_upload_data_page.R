box::use(
  shiny[moduleServer, NS, fluidRow, icon, fileInput, div, br, observeEvent, req, selectInput],
  bs4Dash[tabItem, infoBox, box, boxSidebar],
  shinyWidgets[actionBttn],
  rhandsontable[rHandsontableOutput, renderRHandsontable, rhandsontable, hot_cols, hot_col],
  magrittr[`%>%`],
)

box::use(
  app/logic/R6Class_QProMS
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "upload_data",
    fluidRow(
      infoBox(
        title = "N° Proteins",
        value = 0,
        icon = icon("envelope"),
        width = 3, 
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "Missing Values",
        value = 0,
        icon = icon("envelope"),
        width = 3,
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "Condition",
        value = 0,
        icon = icon("envelope"),
        width = 3,
        color = "primary",
        fill = TRUE
      ),
      infoBox(
        title = "Replicate",
        value = 0,
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
        height = "60vh",
        sidebar = boxSidebar(
          id = ns("upload_sidebar"),
          selectInput(
            inputId = ns("intensity_type"),
            label = "Select Intensity type",
            choices = c("Intensity", "LFQ Intensity", "iBAQ Intensity"),
            selected = "LFQ Intensity"
          ),
          selectInput(
            inputId = ns("source_type"),
            label = "Table source",
            choices = c("MaxQuant", "External table"),
            selected = "MaxQuant"
          ),
          selectInput(
            inputId = ns("organism"),
            label = "Organism",
            choices = c("Homo Sapiens", "Mus Musculus"),
            selected = "Homo Sapiens"
          )
        ),
        br(),
        fileInput(
          inputId = ns("upload_file"),
          label = NULL,
          multiple = FALSE,
          width = "100%",
          placeholder = "proteinGroups.txt",
          accept = ".txt"
        ),
        footer = div(
          style = "display: flex; justify-content: center;",
          div(
            style = "width: 100%; margin-right: 10px;",
            actionBttn(
              inputId = ns("start"),
              label = "Start", 
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          ),
          div(
            style = "width: 100%; margin-left: 10px;",
            actionBttn(
              inputId = ns("tutorial"),
              label = "Tutorial", 
              style = "material-flat",
              color = "default",
              size = "md",
              block = TRUE
            )
          )
        )
      ),
      box(
        title = "Experimental Design",
        status = "primary",
        width = 9,
        height = "60vh",
        maximizable = TRUE,
        rHandsontableOutput(ns("expdesign_table")),
        footer = div(
          style = "display: flex; justify-content: end;",
          div(
            style = "width: 100px; margin-right: 10px;",
            actionBttn(
              inputId = ns("reset"),
              label = "Reset", 
              style = "material-flat",
              color = "default",
              size = "md",
              block = TRUE
            )
          ),
          div(
            style = "width: 100px; margin-left: 10px;",
            actionBttn(
              inputId = ns("apply"),
              label = "Apply", 
              style = "material-flat",
              color = "default",
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
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$start, {
      
      r6$loading_data(input_path = input$upload_file$datapath, input_type = input$source_type)
      
      r6$make_expdesign(start_with = "lfq_intensity_bc_")
      
    })
    
    output$expdesign_table <- renderRHandsontable({
      
      req(input$start)
      
      if(!is.null(r6$expdesign)){
        rhandsontable(data = r6$expdesign, width = "100%", stretchH = "all") %>%
          hot_cols(colWidths = "25%") %>%
          hot_col("key", readOnly = TRUE)
      }
      
    })
    
  })
}