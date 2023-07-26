box::use(
  shiny[moduleServer, NS, fluidRow, icon, fileInput, div, br, observeEvent, req, selectInput, reactiveValues, h4, p, reactive, textInput],
  bs4Dash[tabItem, infoBox, box, boxSidebar, valueBoxOutput, renderValueBox, valueBox, toast],
  shinyWidgets[actionBttn],
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
      box(
        title = "Upload data",
        status = "primary",
        width = 3,
        height = 700,
        br(),
        fileInput(
          inputId = ns("upload_file"),
          label = NULL,
          multiple = FALSE,
          width = "100%",
          placeholder = "proteinGroups.txt",
          accept = ".txt"
        ),
        selectInput(
          inputId = ns("source_type"),
          label = "Table source",
          choices = c("MaxQuant" = "max_quant", "External table" = "external"),
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
            choices = c("Intensity" = "intensity_", "LFQ Intensity" = "lfq_intensity_", "iBAQ Intensity" = "ibaq_intensity_"),
            selected = "lfq_intensity_"
          ),
          condition = "input.source_type != 'external'",
          jsCall = jsCalls$show(),
          ns = ns
        ),
        selectInput(
          inputId = ns("organism"),
          label = "Organism",
          choices = c("Homo Sapiens", "Mus Musculus"),
          selected = "Homo Sapiens"
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
        ),
        br(),
        div(
          style = "display: flex; justify-content: center; gap: 20px",
          div(
            style = "width: 100%;",
            actionBttn(
              inputId = ns("help_me"),
              label = "Help me!", 
              style = "material-flat",
              color = "default",
              size = "md",
              block = TRUE
            )
          ),
          div(
            style = "width: 100%;",
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
          )
        ),
        footer = div(
          style = "display: flex; justify-content: center; gap: 20px; height: 40px",
          div(
            style = "width: 100%;",
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
      ),
      box(
        title = "Experimental Design",
        status = "primary",
        width = 9,
        height = 700,
        maximizable = TRUE,
        rHandsontableOutput(ns("expdesign_table")),
        footer = div(
          style = "display: flex; justify-content: end; gap: 20px; height: 40px",
          div(
            style = "width: 150px;",
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
            style = "width: 150px;",
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
        )
      )
    )
  )

}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    w <- Waiter$new(html = spin_5(), color = "#adb5bd")
    
    init("make_expdesign", "boxes")
    
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
    
    
    observeEvent(input$upload, {
      
      req(input$upload_file)
      req(input$intensity_type)
      req(input$source_type)
      req(input$palette)
      
      r6$palette <- input$palette
      
      r6$loading_data(input_path = input$upload_file$datapath, input_type = input$source_type)
      
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
        
        r6$make_expdesign(intensity_type = input$intensity_type)
        r6$pg_preprocessing()
      }else{
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
            r6$make_expdesign(intensity_type = intensity_regex, genes_column = id_col)
            r6$pg_preprocessing()
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
      
      r6$loading_data(input_path = input$upload_file$datapath, input_type = input$source_type)
      
      if(r6$input_type == "max_quant"){
        r6$make_expdesign(intensity_type = input$intensity_type)
        r6$pg_preprocessing()
      }else{
        id_col <- make_clean_names(input$genes_col)
        intensity_regex <- make_clean_names(input$intensity_type2)
        
        check_id <- r6$raw_data %>% 
          dplyr$select(dplyr$any_of(id_col)) %>% 
          ncol()
        
        check_int <- r6$raw_data %>% 
          dplyr$select(dplyr$contains(intensity_regex)) %>% 
          ncol()
        
        if(check_id > 0 & check_int > 0){
          r6$make_expdesign(intensity_type = intensity_regex, genes_column = id_col)
          r6$pg_preprocessing()
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
      
      input_error <- dplyr$case_when(
        nrow(des) < 1 ~ "The Experimental design is empty",
        nrow(get_dupes(des, label)) != 0 ~ "duplicate names in label column",
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
        return() 
      }
      
      input_warning <- dplyr$case_when(
        min(dplyr$count(des, condition)$n) < 3 ~ "One or more condition group as less then 3 replicates! Statistics is discouraged",
        min(dplyr$count(des, replicate)$n) < 2 ~ "There is only one condition",
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
        return() 
      }
      
      r6$expdesign <- des
      
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
      
      w$show()
      
      req(input$upload)
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
      
      trigger("plot")
      trigger("boxes")
      
    })
    
    ## non serve caricare tutti i parametri, mi basta caricare le tabelle generate dall'analisi e usare quelle per fare tutti i grafici.
    
    observeEvent(input$help_me, {
      
      if(!is.null(r6$ora_table)) {
        r6$print_ora_table(ontology = "BP", groups = r6$go_ora_focus, value = r6$go_ora_plot_value)
        ora_bp <- r6$ora_table
        
        r6$print_ora_table(ontology = "MF", groups = r6$go_ora_focus, value = r6$go_ora_plot_value)
        ora_mf <- r6$ora_table
        
        r6$print_ora_table(ontology = "CC", groups = r6$go_ora_focus, value = r6$go_ora_plot_value)
        ora_cc <- r6$ora_table
      } else {
        ora_bp <- NULL
        ora_mf <- NULL
        ora_cc <- NULL
          
      }
      
      if(!is.null(r6$gsea_table)) {
        r6$print_gsea_table(ontology = "BP", groups = r6$go_gsea_focus, only_common = FALSE)
        gsea_bp <- r6$gsea_table
        
        r6$print_gsea_table(ontology = "MF", groups = r6$go_gsea_focus, only_common = FALSE)
        gsea_mf <- r6$gsea_table
        
        r6$print_gsea_table(ontology = "CC", groups = r6$go_gsea_focus, only_common = FALSE)
        gsea_cc <- r6$gsea_table
      } else {
        gsea_bp <- NULL
        gsea_mf <- NULL
        gsea_cc <- NULL
      }
      
      
      quarto_render(
        here("app/logic/QProMS_Report.qmd"),
        execute_params = list(
          palette = r6$palette,
          color_palette = r6$color_palette,
          expdesign = r6$expdesign,
          filtered_data = r6$filtered_data,
          normalized_data = r6$normalized_data,
          imputed_data = r6$imputed_data,
          tests = c(r6$primary_condition, r6$additional_condition),
          stat_table = r6$stat_table,
          anova_table = r6$anova_table,
          z_score = r6$z_score,
          anova_clust_method = r6$anova_clust_method,
          clusters_number = r6$clusters_number,
          nodes_table = r6$nodes_table,
          edges_table = r6$edges_table,
          net_stat = r6$network_from_statistic,
          net_score = r6$network_score_thr,
          ora_result_bp = ora_bp,
          ora_result_mf = ora_mf,
          ora_result_cc = ora_cc,
          gsea_result_bp = gsea_bp,
          gsea_result_mf = gsea_mf,
          gsea_result_cc = gsea_cc
        )
      )
    })
    
    return(reactive(input$start))
    
  })
}