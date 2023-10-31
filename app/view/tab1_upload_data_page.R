box::use(
  shiny[moduleServer, NS, fluidRow, icon, fileInput, div, br, observeEvent, req, selectInput, updateSelectInput, reactiveValues, h4, p, reactive, textInput, column],
  bs4Dash[tabItem, infoBox, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, toast, accordion, accordionItem, updateAccordion],
  shinyWidgets[actionBttn, prettyCheckbox],
  rhandsontable[rHandsontableOutput, renderRHandsontable, rhandsontable, hot_cols, hot_col, hot_to_r],
  magrittr[`%>%`],
  janitor[make_clean_names, get_dupes],
  gargoyle[init, watch, trigger],
  shinyjs[useShinyjs, disabled, enable],
  esquisse[palettePicker],
  viridis[viridis],
  waiter[Waiter, spin_5],
  quarto[quarto_render],
  here[here],
  yaml[as.yaml],
  dplyr,
  stringr,
  shinyGizmo[conditionalJS, jsCalls],
)

box::use(
  app/logic/R6Class_QProMS
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "upload_data",
    useShinyjs(),
    fluidRow(
      valueBoxOutput(ns("n_proteins"), width = 3),
      valueBoxOutput(ns("total_missing_data"), width = 3),
      valueBoxOutput(ns("n_condition"), width = 3),
      valueBoxOutput(ns("n_replicate"), width = 3)
    ),
    fluidRow(
      column(
        width = 10,
        accordion(
          id = ns("upload_files"),
          accordionItem(
            title = "Upload data",
            status = "primary",
            collapsed = FALSE,
            solidHeader = FALSE,
            div(
              style = "display: flex; justify-content: center; gap: 5rem; align-items: start;",
              div(
                style = "width: 100%; flex: 1 1 0;",
                selectInput(
                  inputId = ns("source_type"),
                  label = "Table source",
                  choices = c(
                    "MaxQuant" = "max_quant",
                    "DIA-NN" = "dia_nn",
                    "FragPipe" = "fragpipe",
                    "Spectronaut" = "spectronaut",
                    "AlphaPept" = "external", 
                    "External table" = "external"
                  ),
                  selected = "max_quant"
                ),
                conditionalJS(
                  div(
                    textInput(
                      inputId = ns("intensity_type2"),
                      label = "Write intensity regex",
                      value = ""
                    ),
                    textInput(
                      inputId = ns("genes_col"),
                      label = "Specify ID column",
                      value = ""
                    )
                  ),
                  condition = "input.source_type == 'external'",
                  jsCall = jsCalls$show(),
                  ns = ns
                ),
                conditionalJS(
                  selectInput(
                    inputId = ns("intensity_type"),
                    label = "Select Intensity type",
                    choices = c("Intensity" = "intensity_", "LFQ Intensity" = "lfq_intensity_", "iBAQ Intensity" = "i_baq_"),
                    selected = "lfq_intensity_"
                  ),
                  condition = "input.source_type == 'max_quant' | input.source_type == 'fragpipe' | input.source_type == 'spectronaut'",
                  jsCall = jsCalls$show(),
                  ns = ns
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                selectInput(
                  inputId = ns("organism"),
                  label = "Organism",
                  choices = c("Homo Sapiens" = "human", "Mus Musculus" = "mouse"),
                  selected = "human"
                ),
                palettePicker(
                  inputId = ns("palette"),
                  label = "Palettes:",
                  choices = list(
                    "A" = viridis(
                      n = 6,
                      direction = -1,
                      end = 0.90,
                      begin = 0.10,
                      option = "A"
                    ),
                    "B" = viridis(
                      n = 6,
                      direction = -1,
                      end = 0.90,
                      begin = 0.10,
                      option = "B"
                    ),
                    "C" = viridis(
                      n = 6,
                      direction = -1,
                      end = 0.90,
                      begin = 0.10,
                      option = "C"
                    ),
                    "D" = viridis(
                      n = 6,
                      direction = -1,
                      end = 0.90,
                      begin = 0.10,
                      option = "D"
                    ),
                    "E" = viridis(
                      n = 6,
                      direction = -1,
                      end = 0.90,
                      begin = 0.10,
                      option = "E"
                    ),
                    "F" = viridis(
                      n = 6,
                      direction = -1,
                      end = 0.90,
                      begin = 0.10,
                      option = "F"
                    ),
                    "G" = viridis(
                      n = 6,
                      direction = -1,
                      end = 0.90,
                      begin = 0.10,
                      option = "G"
                    ),
                    "H" = viridis(
                      n = 6,
                      direction = -1,
                      end = 0.90,
                      begin = 0.10,
                      option = "H"
                    )
                  ),
                  selected = "D"
                )
              ),
              div(
                style = "width: 100%; flex: 1 1 0;",
                fileInput(
                  inputId = ns("upload_file"),
                  label = "Input table",
                  multiple = FALSE,
                  width = "100%",
                  placeholder = "Input_table",
                  accept = c(".txt", ".tsv", ".csv")
                ),
                prettyCheckbox(
                  inputId = ns("use_params"),
                  label = "Load parameters", 
                  value = FALSE,
                  shape = "curve", 
                  width = "auto"
                ),
                conditionalJS(
                  fileInput(
                    inputId = ns("upload_params"),
                    label = NULL,
                    multiple = FALSE,
                    width = "100%",
                    placeholder = "QProMS_parameters.yaml",
                    accept = ".yaml"
                  ),
                  condition = "input.use_params",
                  jsCall = jsCalls$show(),
                  ns = ns
                )
              )
            )
          ),
          accordionItem(
            title = "Experimental Design",
            status = "primary",
            collapsed = TRUE,
            solidHeader = FALSE,
            div(
              style = "display: flex; justify-content: center; gap: 5rem; align-items: start;",
              div(
                style = "width: 100%; flex: 1 1 0; padding: 0.5rem",
                rHandsontableOutput(ns("expdesign_table"))
              )
            )
          ),
          width = 12
        )
      ),
      column(
        width = 2,
        div(
          style = "width: 100%; margin-top: 2.5px;",
          disabled(
            actionBttn(
              inputId = ns("upload"),
              label = "Upload", 
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          )
        ),
        div(
          style = "display: flex; justify-content: center; gap: 1rem; align-items: start;",
          div(
            style = "width: 100%; flex: 1 1 0; margin-top: 22px;",
            conditionalJS(
              ui = actionBttn(
                inputId = ns("reset"),
                label = "Reset", 
                style = "material-flat",
                color = "default",
                size = "md",
                block = TRUE
              ),
              condition = "input.upload > 0",
              jsCall = jsCalls$show(),
              ns = ns
            )
          ),
          div(
            style = "width: 100%; flex: 1 1 0; margin-top: 22px;",
            conditionalJS(
              ui = actionBttn(
                inputId = ns("confirm"),
                label = "Apply", 
                style = "material-flat",
                color = "primary",
                size = "md",
                block = TRUE
              ),
              condition = "input.upload > 0",
              jsCall = jsCalls$show(),
              ns = ns
            )
          )
        ),
        div(
          style = "width: 100%; margin-top: 22px;",
          conditionalJS(
            ui = actionBttn(
              inputId = ns("start"),
              label = "Start", 
              style = "material-flat",
              color = "success",
              size = "md",
              block = TRUE
            ),
            condition = "input.confirm > 0",
            jsCall = jsCalls$show(),
            ns = ns
          )
        )
      )
    )
  )
  

}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    w <- Waiter$new(html = spin_5(), color = "#adb5bd")
    
    init("make_expdesign", "boxes", "params", "ui_element")
    
    output$n_proteins <- renderValueBox({
      
      watch("boxes")
      
      if(is.null(input$expdesign_table)){
        value <- 0
      }else{
        value <- length(unique(r6$data$gene_names))
      }
      
      valueBox(
        subtitle = NULL,
        value = h4(value, style = "margin-top: 0.5rem;"),
        icon = icon("database"),
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
        icon = icon("eye-slash"),
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
      
      if(!is.null(input$upload_file)){
        enable("upload")
      }
      
    })
    
    observeEvent(input$source_type, {
      
      if (input$source_type == "fragpipe") {
        updateSelectInput(
          inputId = "intensity_type",
          choices = c("MAX LFQ Intensity" = "_max_lfq_intensity", "Intensity" = "_intensity"),
          selected = "_max_lfq_intensity"
        )
      }
      
      if (input$source_type == "max_quant") {
        updateSelectInput(
          inputId = "intensity_type",
          choices = c("Intensity" = "intensity_", "LFQ Intensity" = "lfq_intensity_", "iBAQ Intensity" = "i_baq_"),
          selected = "lfq_intensity_"
        )
      }
      
      if (input$source_type == "spectronaut") {
        updateSelectInput(
          inputId = "intensity_type",
          choices = c("Quantity" = "_quantity", "MS1 Quantity" = "_ms1quantity", "MS2 Quantity" = "_ms2quantity"),
          selected = "_quantity"
        )
      }
      
    })
    
    
    observeEvent(input$upload, {
      
      req(input$upload_file)
      req(input$intensity_type)
      req(input$source_type)
      req(input$palette)
      
      r6$palette <- input$palette
      
      r6$loading_data(input_path = input$upload_file$datapath, input_type = input$source_type, input_name = input$upload_file$name)
      
      if(!is.null(input$upload_params)) {
        r6$loading_patameters(input_path = input$upload_params$datapath)
        
        input_error <- dplyr$case_when(
          input$upload_file$name != r6$input_file_name ~ "the file name is different form the one saved in parameters file",
          TRUE ~ ""
        )
        if (input_error != "") {
          toast(
            title = "Imput table have different name",
            body = input_error,
            options = list(
              class = "bg-danger",
              autohide = TRUE,
              delay = 5000,
              icon = icon("exclamation-circle", verify_fa = FALSE)
            )
          )
          return() 
        }
      }
      
      if(r6$input_type == "max_quant"){
        
        input_error <- dplyr$case_when(
          nrow(r6$raw_data) < 1 ~ "The file is empty",
          length(dplyr$select(r6$raw_data, dplyr$starts_with(input$intensity_type))) < 1 ~ paste0("No ", input$intensity_type, " columns find"),
          !"gene_names" %in% names(r6$raw_data) ~ "The Gene names column is missing",
          !"protein_i_ds" %in% names(r6$raw_data) ~ "The Protein ID column is missing",
          !"id" %in% names(r6$raw_data) ~ "The ID column is missing",
          !"peptides" %in% names(r6$raw_data) ~ "The Peptides column is missing",
          !"razor_unique_peptides" %in% names(r6$raw_data) ~ "The Razor unique peptides column is missing",
          !"unique_peptides" %in% names(r6$raw_data) ~ "The unique_peptides column is missing",
          !"reverse" %in% names(r6$raw_data) ~ "The Reverse column is missing",
          !"potential_contaminant" %in% names(r6$raw_data) ~ "The Potential contaminant column is missing",
          !"only_identified_by_site" %in% names(r6$raw_data) ~ "The Only identified by site column is missing",
          TRUE ~ ""
        )
        if (input_error != "") {
          toast(
            title = "Incomplete data",
            body = input_error,
            options = list(
              class = "bg-danger",
              autohide = TRUE,
              delay = 5000,
              icon = icon("exclamation-circle", verify_fa = FALSE)
            )
          )
          return() 
        }
        
        if(!r6$parameters_loaded) {
          r6$make_expdesign(intensity_type = input$intensity_type)
        }
      }
      
      if(r6$input_type == "dia_nn") {
        
        input_error <- dplyr$case_when(
          nrow(r6$raw_data) < 1 ~ "The file is empty",
          !"genes" %in% names(r6$raw_data) ~ "The Genes column is missing",
          TRUE ~ ""
        )
        if (input_error != "") {
          toast(
            title = "Incomplete data",
            body = input_error,
            options = list(
              class = "bg-danger",
              autohide = TRUE,
              delay = 5000,
              icon = icon("exclamation-circle", verify_fa = FALSE)
            )
          )
          return() 
        }
        
        if(!r6$parameters_loaded) {
          r6$make_expdesign(intensity_type = NULL)
        }
      }
      
      if(r6$input_type == "fragpipe") {
        
        input_error <- dplyr$case_when(
          nrow(r6$raw_data) < 1 ~ "The file is empty",
          !"gene" %in% names(r6$raw_data) ~ "The Genes column is missing",
          !"protein_id" %in% names(r6$raw_data) ~ "The Protein ID column is missing",
          TRUE ~ ""
        )
        if (input_error != "") {
          toast(
            title = "Incomplete data",
            body = input_error,
            options = list(
              class = "bg-danger",
              autohide = TRUE,
              delay = 5000,
              icon = icon("exclamation-circle", verify_fa = FALSE)
            )
          )
          return() 
        }
        
        if(!r6$parameters_loaded) {
          r6$make_expdesign(intensity_type = input$intensity_type)
        }
      }
      
      if(r6$input_type == "spectronaut") {
        
        input_error <- dplyr$case_when(
          nrow(r6$raw_data) < 1 ~ "The file is empty",
          !"pg_genes" %in% names(r6$raw_data) ~ "The Genes column is missing",
          !"pg_protein_groups" %in% names(r6$raw_data) ~ "The PG Protein Groups column is missing",
          TRUE ~ ""
        )
        if (input_error != "") {
          toast(
            title = "Incomplete data",
            body = input_error,
            options = list(
              class = "bg-danger",
              autohide = TRUE,
              delay = 5000,
              icon = icon("exclamation-circle", verify_fa = FALSE)
            )
          )
          return() 
        }
        
        if(!r6$parameters_loaded) {
          r6$make_expdesign(intensity_type = input$intensity_type)
        }
      }
      
      if(r6$input_type == "external"){
        id_col <- make_clean_names(input$genes_col)
        intensity_regex <- make_clean_names(input$intensity_type2)
        
        check_id <- r6$raw_data %>% 
          dplyr$select(dplyr$any_of(id_col)) %>% 
          ncol()
        
        check_int <- r6$raw_data %>% 
          dplyr$select(dplyr$contains(intensity_regex)) %>% 
          ncol()
        
        check_unique <- r6$raw_data %>% 
          dplyr$select(dplyr$any_of(id_col)) %>% 
          get_dupes() %>% 
          nrow()
        
        if(check_id > 0 & check_int > 0){
          if(check_unique == 0){
            if(!r6$parameters_loaded) {
              r6$make_expdesign(intensity_type = intensity_regex, genes_column = id_col)
            }
          }else{
            toast(
              title = "Wrong Inputs",
              body = "ID column is not unique!",
              options = list(
                class = "bg-danger",
                autohide = TRUE,
                delay = 5000,
                icon = icon("exclamation-circle", verify_fa = FALSE)
              )
            )
          }
        }else{
          toast(
            title = "Wrong Inputs",
            body = "ID column or intensity regex are wrong!",
            options = list(
              class = "bg-danger",
              autohide = TRUE,
              delay = 5000,
              icon = icon("exclamation-circle", verify_fa = FALSE)
            )
          )
        }
      }
      
      updateAccordion(id = "upload_files", selected = 2)
      
    })
    
    output$expdesign_table <- renderRHandsontable({
      
      watch("make_expdesign")
      
      req(input$upload_file)
      req(input$upload)
      
      if(!is.null(r6$expdesign)){
        
        des <- r6$expdesign %>% dplyr$mutate(keep = TRUE)
        
        rhandsontable(data = des, width = "100%", stretchH = "all") %>%
          hot_cols(colWidths = "25%") %>%
          hot_col("key", readOnly = TRUE) 
      }
      
    })
    
    observeEvent(input$reset, {
      
      req(input$upload)
      req(input$upload_file)
      req(input$intensity_type)
      
      r6$loading_data(input_path = input$upload_file$datapath, input_type = input$source_type, input_name = input$upload_file$name)
      
      if(r6$input_type != "external"){
        if(!r6$parameters_loaded) {
          r6$make_expdesign(intensity_type = input$intensity_type)
        }
      }
      
      if(r6$input_type == "external"){
        id_col <- make_clean_names(input$genes_col)
        intensity_regex <- make_clean_names(input$intensity_type2)
        
        check_id <- r6$raw_data %>% 
          dplyr$select(dplyr$any_of(id_col)) %>% 
          ncol()
        
        check_int <- r6$raw_data %>% 
          dplyr$select(dplyr$contains(intensity_regex)) %>% 
          ncol()
        
        if(check_id > 0 & check_int > 0){
          if(!r6$parameters_loaded) {
            r6$make_expdesign(intensity_type = intensity_regex, genes_column = id_col)
          }
        }else{
          toast(
            title = "Wrong Inputs",
            body = "ID column or intensity regex are wrong!",
            options = list(
              class = "bg-danger",
              autohide = TRUE,
              delay = 5000,
              icon = icon("check")
            )
          )
        }
      }
      
      trigger("make_expdesign")
      
    })
    
    observeEvent(input$confirm, {
      
      req(input$upload)
      req(input$upload_file)
      req(input$intensity_type)
      
      
      des <- hot_to_r(input$expdesign_table) %>% 
        dplyr$filter(keep) %>% 
        dplyr$select(-keep)
      
      r6$is_ok <- TRUE
      
      input_error <- dplyr$case_when(
        nrow(des) < 1 ~ "The Experimental design is empty",
        !isTRUE(all(duplicated(des$label) == FALSE)) ~ "duplicate names in label column",
        TRUE ~ ""
      )
      if (input_error != "") {
        toast(
          title = "Experimental design incorrect",
          body = input_error,
          options = list(
            class = "bg-danger",
            autohide = TRUE,
            delay = 5000,
            icon = icon("exclamation-circle", verify_fa = FALSE)
          )
        )
        r6$is_ok <- FALSE
        return() 
      }
      
      input_warning <- dplyr$case_when(
        min(dplyr$count(des, condition)$n) < 3 ~ "One or more condition group as less then 3 replicates! Statistics is discouraged",
        min(dplyr$count(des, replicate)$n) < 2 ~ "There is only one condition. Otherwise replicate names are not consistent between conditions",
        min(stringr$str_length(des$condition)) < 2 ~ "Condition names should be at least 2 letters long",
        TRUE ~ ""
      )
      
      if (input_warning != "") {
        toast(
          title = "Warning!",
          body = input_warning,
          options = list(
            class = "bg-warning",
            autohide = TRUE,
            delay = 5000,
            icon = icon("exclamation-circle", verify_fa = FALSE)
          )
        )
        r6$is_ok <- FALSE
        return() 
      }
      
      r6$expdesign <- des
      
      if(!r6$parameters_loaded) {
        r6$protein_rank_target <- r6$expdesign$label[1]
      }
      
      r6$pg_preprocessing()
      
      trigger("boxes")
      
      toast(
        title = "Experimental design defined!",
        body = "Now you can press START!",
        options = list(
          class = "bg-success",
          autohide = TRUE,
          delay = 5000,
          icon = icon("check")
        )
      )
      
    })
    
    observeEvent(input$start, {
      
      req(input$upload)
      req(input$upload_file)
      req(input$intensity_type)
      req(input$expdesign_table)
      req(input$confirm)
      req(input$organism)
      
      r6$organism <- input$organism
      
      input_error <- dplyr$case_when(
        !r6$is_ok ~ "The Experimental design isn't correct!",
        TRUE ~ ""
      )
      if (input_error != "") {
        toast(
          title = "Experimental design error",
          body = input_error,
          options = list(
            class = "bg-danger",
            autohide = TRUE,
            delay = 5000,
            icon = icon("exclamation-circle", verify_fa = FALSE)
          )
        )
        return() 
      }
      
      w$show()
      
      r6$data_wrangling(
        valid_val_filter = r6$valid_val_filter,
        valid_val_thr = r6$valid_val_thr,
        pep_filter = r6$pep_filter,
        pep_thr = r6$pep_thr,
        rev = r6$rev,
        cont = r6$cont,
        oibs = r6$oibs
      )
      
      r6$normalization(norm_methods = r6$norm_methods)
      
      r6$imputation(
        imp_methods = r6$imp_methods,
        shift = r6$imp_shift,
        scale = r6$imp_scale,
        unique_visual = FALSE
      )
      
      w$hide()
      
      toast(
        title = "Filtred data page unlock!",
        options = list(
          class = "bg-white",
          autohide = TRUE,
          delay = 5000,
          icon = icon("unlock")
        )
      )
      
      Sys.sleep(0.2)
      
      toast(
        title = "Missing data page unlock!",
        options = list(
          class = "bg-white",
          autohide = TRUE,
          delay = 5000,
          icon = icon("unlock")
        )
      )
      
      Sys.sleep(0.2)
      
      toast(
        title = "Correlation page unlock!",
        options = list(
          class = "bg-white",
          autohide = TRUE,
          delay = 5000,
          icon = icon("unlock")
        )
      )
      
      Sys.sleep(0.2)
      toast(
        title = "PCA page unlock!",
        options = list(
          class = "bg-white",
          autohide = TRUE,
          delay = 5000,
          icon = icon("unlock")
        )
      )
      Sys.sleep(0.2)
      toast(
        title = "Statistic page unlock!",
        options = list(
          class = "bg-white",
          autohide = TRUE,
          delay = 5000,
          icon = icon("unlock")
        )
      )
      Sys.sleep(0.2)
      toast(
        title = "Network page unlock!",
        options = list(
          class = "bg-white",
          autohide = TRUE,
          delay = 5000,
          icon = icon("unlock")
        )
      )
      Sys.sleep(0.2)
      toast(
        title = "Functional page unlock!",
        options = list(
          class = "bg-white",
          autohide = TRUE,
          delay = 5000,
          icon = icon("unlock")
        )
      )
      
      trigger("plot")
      trigger("boxes")
      trigger("ui_element")
      
      if(r6$parameters_loaded) {
        trigger("params")
      }
      
    })
    
    return(reactive(input$start))
    
  })
}