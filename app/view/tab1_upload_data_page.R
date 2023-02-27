box::use(
  shiny[moduleServer, NS, fluidRow, icon, fileInput, div, br, observeEvent, req, selectInput, reactiveValues, h4, p],
  bs4Dash[tabItem, infoBox, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox],
  shinyWidgets[actionBttn],
  rhandsontable[rHandsontableOutput, renderRHandsontable, rhandsontable, hot_cols, hot_col, hot_to_r],
  magrittr[`%>%`],
  gargoyle[init, watch, trigger],
  dplyr,
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
      valueBoxOutput(ns("n_proteins"), width = 3),
      valueBoxOutput(ns("total_missing_data"), width = 3),
      valueBoxOutput(ns("n_condition"), width = 3),
      valueBoxOutput(ns("n_replicate"), width = 3)
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
            choices = c("Intensity" = "intensity_", "LFQ Intensity" = "lfq_intensity_", "iBAQ Intensity" = "ibaq_intensity_"),
            selected = "lfq_intensity_"
          ),
          selectInput(
            inputId = ns("source_type"),
            label = "Table source",
            choices = c("MaxQuant" = "max_quant", "External table"),
            selected = "max_quant"
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
          style = "display: flex; justify-content: end; gap: 20px",
          div(
            style = "width: 150px;",
            actionBttn(
              inputId = ns("tutorial"),
              label = "Tutorial", 
              style = "material-flat",
              color = "default",
              size = "md",
              block = TRUE
            )
          ),
          div(
            style = "width: 150px;",
            actionBttn(
              inputId = ns("start"),
              label = "Start", 
              style = "material-flat",
              color = "primary",
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
          style = "display: flex; justify-content: end; gap: 20px",
          div(
            style = "width: 150px;",
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
            style = "width: 150px;",
            actionBttn(
              inputId = ns("confirm"),
              label = "Confirm", 
              style = "material-flat",
              color = "primary",
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
    
    init("make_expdesign", "boxes")
    
    output$n_proteins <- renderValueBox({
      
      watch("boxes")
      
      if(is.null(input$expdesign_table)){
        value <- 0
      }else{
        selected_cond <-
          r6$expdesign %>% 
          dplyr$distinct(label) %>% 
          dplyr$pull()
        
        data <- r6$data %>% 
          dplyr$filter(label %in% selected_cond)
        
        value <- length(unique(data$gene_names))
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("file"),
        color = "primary",
        footer = p("Total N° of proteins", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$total_missing_data <- renderValueBox({
      
      watch("boxes")
      
      if(is.null(input$expdesign_table)){
        value <- 0
      }else{
        value <- r6$total_missing_data(raw = TRUE)
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("ban"),
        color = "primary",
        footer = p("Total missing values", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$n_condition <- renderValueBox({
      
      watch("boxes")
      
      if(is.null(input$expdesign_table)){
        value <- 0
      }else{
        value <- length(unique(r6$expdesign$condition))
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("object-group"),
        color = "primary",
        footer = p("Condition", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    output$n_replicate <- renderValueBox({
      
      watch("boxes")
      
      if(is.null(input$expdesign_table)){
        value <- 0
      }else{
        value <- length(unique(r6$expdesign$replicate))
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("list-ol"),
        color = "primary",
        footer = p("Replicate", style = "margin: 0; padding-left: 0.5rem; text-align: left;"),
        elevation = 2
      )
      
    })
    
    observeEvent(input$upload_file, {
      
      req(input$intensity_type)
      req(input$source_type)
      
      r6$loading_data(input_path = input$upload_file$datapath, input_type = input$source_type)
      
      r6$make_expdesign(start_with = input$intensity_type)
      
    })
    
    output$expdesign_table <- renderRHandsontable({
      
      watch("make_expdesign")
      
      req(input$upload_file)
      # aggiungere una colonna logical per rimuovere i campioni che non si vogliono più tenere
      if(!is.null(r6$expdesign)){
        rhandsontable(data = r6$expdesign, width = "100%", stretchH = "all") %>%
          hot_cols(colWidths = "25%") %>%
          hot_col("key", readOnly = TRUE)
      }
      
    })
    
    observeEvent(input$reset, {
      
      req(input$upload_file)
      req(input$intensity_type)
      
      r6$loading_data(input_path = input$upload_file$datapath, input_type = input$source_type)
      r6$make_expdesign(start_with = input$intensity_type)
      
      trigger("make_expdesign")
      
    })
    
    observeEvent(input$confirm, {
      
      req(input$upload_file)
      req(input$intensity_type)
      
      r6$expdesign <- hot_to_r(input$expdesign_table)
      
      trigger("boxes")
      
    })
    
    observeEvent(input$start, {
      
      req(input$upload_file)
      req(input$intensity_type)
      req(input$expdesign_table)
      req(input$confirm)
      
      r6$data_wrangling(
        valid_val_filter = r6$valid_val_filter,
        valid_val_thr = r6$valid_val_thr,
        pep_filter = r6$pep_filter,
        pep_thr = r6$pep_thr,
        rev = r6$rev,
        cont = r6$cont,
        oibs = r6$oibs
      )
      
      #updateTabItems(session = getDefaultReactiveDomain(), "wrangling_data")
      
    })
    
  })
}