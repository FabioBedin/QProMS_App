box::use(
  shiny[moduleServer, NS, fluidRow, div, downloadHandler, isolate],
  bs4Dash[tabItem, accordion, accordionItem, toast],
  shinyGizmo[pickCheckboxInput],
  shinyWidgets[downloadBttn],
  esquisse[palettePicker],
  viridis[viridis],
  quarto[quarto_render],
  here[here],
  yaml[write_yaml],
  waiter[Waiter, spin_5, withWaiter],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "report",
    fluidRow(
      width = 11,
      accordion(
        id = ns("downloads_params"),
        accordionItem(
          title = "Layout settings",
          status = "primary",
          collapsed = FALSE,
          solidHeader = FALSE,
          div(
            style = "display: flex; justify-content: center; gap: 5rem; align-items: start;",
            div(
              style = "width: 100%; flex: 1 1 0;",
              pickCheckboxInput(
                inputId = ns("report_settings"),
                label = "Define report layout",
                choices = list(
                  "Quality control" = c("Protein counts", "Upset plot", "Intensity Distribution", "CV"),
                  "Missing data" = c("Counts", "Distribution", "Effect of Imputation"),
                  "Exploratory Data Analysis" = c("PCA", "Correlation heatmap"),
                  "Statistics" = c("Univariate", "Multivariate"),
                  "Network analysis" = "Network results",
                  "Functional analysis" = c("ORA", "GSEA")
                ),
                selected = list(
                  "Quality control" = c("Protein counts", "Upset plot", "Intensity Distribution", "CV"),
                  "Missing data" = c("Counts", "Distribution", "Effect of Imputation"),
                  "Exploratory Data Analysis" = c("PCA", "Correlation heatmap"),
                  "Statistics" = c("Univariate", "Multivariate"),
                  "Network analysis" = "Network results",
                  "Functional analysis" = c("ORA", "GSEA")
                ),
                options = list(`selected-text-format` = "count > 2")
              )
            ),
            div(
              style = "width: 100%; flex: 1 1 0;",
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
            ),
            div(
              style = "width: 100%; flex: 1 1 0; padding-top: 1.6rem;",
              downloadBttn(
                outputId  = ns("download_report"),
                label = "Download",
                style = "material-flat",
                color = "primary",
                size = "md",
                block = TRUE
              )
            )
          )
        ),
        accordionItem(
          title = "Save parameters",
          status = "primary",
          collapsed = TRUE,
          solidHeader = FALSE,
          div(
            style = "display: flex; justify-content: center; gap: 5rem; align-items: start;",
            div(
              style = "width: 100%; flex: 1 1 0;",
              
            ),
            div(
              style = "width: 100%; flex: 1 1 0;",
              
            ),
            div(
              style = "width: 100%; flex: 1 1 0; padding-top: 1.6rem;",
              downloadBttn(
                outputId  = ns("save_params"),
                label = "Download", 
                style = "material-flat",
                color = "primary",
                size = "md",
                block = TRUE
              )
            )
          )
        ),
        width = 12
      )
    )
  )
}

#' @export
server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    w <- Waiter$new(html = spin_5(), color = "#adb5bd")
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("QProMS_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        
        w$show()
        
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
        
        param_list <- isolate(input$report_settings)
        
        param_protein_counts = FALSE
        param_upset_plot = FALSE
        param_intensity_dist = FALSE
        param_cv = FALSE
        param_md_counts = FALSE
        param_md_distribution = FALSE
        param_imputation = FALSE
        param_pca = FALSE
        param_correlation = FALSE
        param_univariate = FALSE
        param_multivariate = FALSE
        param_network = FALSE
        param_ora = FALSE
        param_gsea = FALSE
        
        for(n in 1:length(param_list)) {
          for(m in 1:length(param_list[[n]])) {
            switch(
              param_list[[n]][m],
              "Protein counts" = (param_protein_counts = TRUE),
              "Upset plot" = (param_upset_plot = TRUE),
              "Intensity Distribution" = (param_intensity_dist = TRUE),
              "CV" = (param_cv = TRUE),
              "Counts" = (param_md_counts = TRUE),
              "Distribution" = (param_md_distribution = TRUE),
              "Effect of Imputation" = (param_imputation = TRUE),
              "PCA" = (param_pca = TRUE),
              "Correlation heatmap" = (param_correlation = TRUE),
              "Univariate" = (param_univariate = TRUE),
              "Multivariate" = (param_multivariate = TRUE),
              "Network results" = (param_network = TRUE),
              "ORA" = (param_ora = TRUE),
              "GSEA" = (param_gsea = TRUE)
            )
          }
        }
        
        
        quarto_render(
          here("app/logic/QProMS_Report.qmd"),
          execute_params = list(
            par_protein_counts = param_protein_counts,
            par_upset_plot = param_upset_plot,
            par_intensity_dist = param_intensity_dist,
            par_cv = param_cv,
            par_md_counts = param_md_counts,
            par_md_distribution = param_md_distribution,
            par_imputation = param_imputation,
            par_pca = param_pca,
            par_correlation = param_correlation,
            par_univariate = param_univariate,
            par_multivariate = param_multivariate,
            par_network = param_network,
            par_ora = param_ora,
            par_gsea = param_gsea, 
            palette = isolate(input$palette),
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
          ),
          quiet = TRUE
        )
        
        file.copy(here("app/logic/QProMS_Report.html"), file)
        
        w$hide()
        
        file.remove(here("app/logic/QProMS_Report.html"))
      }
    )
    
    output$save_params <- downloadHandler(
      filename = function() {
        paste0("QProMS_parameters", ".yaml")
      },
      content = function(file) {
        
        parameters_list <- list(
          input_file_name = r6$input_file_name,
          expdesign = r6$expdesign,
          valid_val_filter = r6$valid_val_filter,
          valid_val_thr = r6$valid_val_thr,
          norm_methods = r6$norm_methods,
          pep_filter = r6$pep_filter,
          pep_thr = r6$pep_thr,
          rev = r6$rev,
          cont = r6$cont,
          oibs = r6$oibs,
          imp_methods = r6$imp_methods,
          imp_shift = r6$imp_shift,
          imp_scale = r6$imp_scale,
          univariate_test_type = r6$univariate_test_type,
          univariate_paired = r6$univariate_paired,
          fold_change = r6$fold_change,
          univariate_alpha = r6$univariate_alpha,
          univariate_p_adj_method = r6$univariate_p_adj_method,
          primary_condition = r6$primary_condition,
          additional_condition = r6$additional_condition,
          anova_alpha = r6$anova_alpha,
          z_score = r6$z_score,
          anova_p_adj_method = r6$anova_p_adj_method,
          anova_clust_method = r6$anova_clust_method,
          clusters_number = r6$clusters_number,
          pdb_database = r6$pdb_database,
          network_score_thr = r6$network_score_thr
        )
        
        write_yaml(x = parameters_list, file = file)
        
      }
    )
    

  })
}
