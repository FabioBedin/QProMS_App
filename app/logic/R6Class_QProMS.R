box::use(
  R6[R6Class],
  data.table[fread],
  janitor[make_clean_names, get_dupes],
  tibble[as_tibble, column_to_rownames],
  viridis[viridis],
  magrittr[`%>%`],
  stats[sd, rnorm, prcomp, cor],
  dplyr,
  tidyr,
  stringr,
  reactable[reactable, colDef],
  echarts4r,
  htmlwidgets[JS],
  vsn[vsn2, predict],
  corrmorant[...],
  ggplot2[scale_fill_viridis_c, geom_point]
)

#' @export
QProMS <- R6Class(
  classname = "QProMS",
  public = list(
    ####################
    # Input parameters #
    raw_data = NULL,
    data = NULL,
    input_type = "max_quant",
    intensity_type = "lfq_intensity_",
    organism = NULL, #questo potrebbe essere tolto visto usiamo solo humans
    expdesign = NULL,
    palette = "D",
    color_palette = NULL,
    # parameters for data wrangling #
    filtered_data = NULL,
    valid_val_filter = "alog",
    valid_val_thr = 0.75,
    pep_filter = "peptides",
    pep_thr = 2,
    rev = TRUE,
    cont = TRUE,
    oibs = TRUE,
    ###############################
    # parameters for normalization #
    normalized_data = NULL,
    norm_methods = "None",
    is_norm = FALSE,
    ############################
    # parameters for imputation #
    imputed_data = NULL,
    imp_methods = "mixed",
    imp_shift = 1.8,
    imp_scale = 0.3,
    pcs = NULL,
    cor_method = "pearson",
    is_mixed = NULL,
    is_imp = FALSE,
    #################
    # parameters For Statistics #
    all_test_combination = NULL,
    tested_condition = NULL,
    univariate = NULL,
    clusters_def = NULL,
    clusters_number = NULL,
    stat_table = NULL,
    fold_change = 1,
    p_adj_method = NULL,
    alpha_ttest = NULL,
    anova_table = NULL,
    cluster_table = NULL,
    alpha_anova = NULL,
    ###########
    # Methods #
    loading_data = function(input_path, input_type){
      
      self$raw_data <- fread(input = input_path) %>%
        as_tibble(.name_repair = make_clean_names)
      
      self$input_type <- input_type
    },
    define_colors = function(){
      n_of_color <- max(self$expdesign %>% dplyr$distinct(condition) %>% nrow())
      self$color_palette <- viridis(n = n_of_color , direction = -1, end = 0.90, begin = 0.10, option = self$palette)
    },
    total_missing_data = function(raw = TRUE){
      
      if(raw){
        data <- self$data
      }else{
        data <- self$filtered_data
      }
      
      if(0 %in% data$bin_intensity){
        value <- data %>%
          dplyr$count(bin_intensity) %>%
          dplyr$mutate(missing = paste0(round(n/nrow(data)*100,0), " %")) %>%
          dplyr$filter(bin_intensity == 0) %>%
          dplyr$pull(missing)
      }else{
        value <- "0 %"
      }
      
      return(value)
    },
    missing_data_type = function(type){
      
      data <- self$filtered_data
      
      if(0 %in% data$bin_intensity){
        value <- data %>% 
          dplyr$group_by(gene_names, condition) %>%
          dplyr$mutate(for_mean_imp = dplyr$if_else((sum(bin_intensity) / dplyr$n()) >= 0.75, TRUE, FALSE)) %>%
          dplyr$ungroup() %>% 
          dplyr$mutate(missing_type = dplyr$case_when(bin_intensity == 0 & for_mean_imp ~ "MAR",
                                                      bin_intensity == 0 & !for_mean_imp ~ "MNAR",
                                                      TRUE ~ "not missing")) %>% 
          dplyr$count(missing_type) %>%
          dplyr$mutate(total = sum(n)) %>% 
          dplyr$mutate(missing_type_perc = paste0(round(n/total*100,0), " %")) %>% 
          dplyr$filter(!missing_type == "not missing") %>% 
          dplyr$select(missing_type, missing_type_perc) %>% 
          dplyr$filter(missing_type == type) %>% 
          dplyr$pull(missing_type_perc)
      }else{
        value <- "0 %"
      }
      
      if(length(value) == 0){
        value <- "0 %"
      }
      
      return(value)
    },
    make_expdesign = function(start_with = "lfq_intensity_"){
      ## qui mettere tutti gli if in base all'intensity type
      
      self$intensity_type <- start_with
      
      data <- self$raw_data %>% 
        dplyr$mutate(dplyr$across(dplyr$starts_with(start_with), ~ log2(.))) %>%
        dplyr$mutate(dplyr$across(dplyr$starts_with(start_with), ~ dplyr$na_if(.,-Inf)))
      
      self$expdesign <- data %>%
        dplyr$select(gene_names, dplyr$starts_with(start_with)) %>%
        tidyr$pivot_longer(!gene_names, names_to = "key", values_to = "intensity") %>%
        dplyr$distinct(key) %>%
        dplyr$mutate(label = stringr$str_remove(key, start_with)) %>%
        dplyr$mutate(condition = stringr$str_remove(label, "_[^_]*$")) %>%
        dplyr$mutate(replicate = stringr$str_remove(label, ".*_"))
    },
    define_tests = function(){
      conditions <-
        dplyr$distinct(self$expdesign, condition) %>% dplyr$pull(condition)
      
      tests <-
        tidyr$expand_grid(cond1 = conditions, cond2 = conditions) %>%
        dplyr$filter(cond1 != cond2) %>%
        dplyr$mutate(test = paste0(cond1, "_vs_", cond2)) %>%
        dplyr$pull(test)
      
      self$all_test_combination <- tests
    },
    pg_preprocessing = function(){
      ########################################################################
      #### This function prepare the proteing groups in the QProMS format ####
      #### and remove duplicates.                                         ####
      ########################################################################
      
      ### this firts part remove duplicate and missing gene names
      ### in proteinGroups.txt input
      
      ## Indentify all duplicate gene names 
      ## and add after __ the protein iD
      
      data <- self$raw_data %>%
        dplyr$mutate(dplyr$across(dplyr$starts_with(self$intensity_type), ~ log2(.))) %>%
        dplyr$mutate(dplyr$across(dplyr$starts_with(self$intensity_type), ~ dplyr$na_if(.,-Inf)))
      
      expdesign <- self$expdesign
      
      self$define_colors()
      self$define_tests()
      
      data_standardized <- data %>%
        dplyr$select(protein_i_ds, gene_names, id) %>%
        dplyr$mutate(gene_names = stringr$str_extract(gene_names, "[^;]*")) %>%
        ## every protein gorups now have only 1 gene name associated to it
        dplyr$rename(unique_gene_names = gene_names) %>%
        get_dupes(unique_gene_names) %>%
        dplyr$mutate(
          unique_gene_names = dplyr$case_when(
            unique_gene_names != "" ~ paste0(
              unique_gene_names,
              "__",
              stringr$str_extract(protein_i_ds, "[^;]*")
            ),
            TRUE ~ stringr$str_extract(protein_i_ds, "[^;]*")
          )
        ) %>%
        dplyr$select(unique_gene_names, id) %>%
        dplyr$right_join(data, by = "id") %>%
        dplyr$mutate(
          gene_names = dplyr$case_when(unique_gene_names != "" ~ unique_gene_names,
                                        TRUE ~ gene_names)
        ) %>%
        dplyr$select(-unique_gene_names) %>%
        dplyr$mutate(gene_names = dplyr$if_else(
          gene_names == "",
          stringr$str_extract(protein_i_ds, "[^;]*"),
          gene_names
        )) %>%
        dplyr$mutate(gene_names = stringr$str_extract(gene_names, "[^;]*")) %>% 
        dplyr$select(
          gene_names,
          dplyr$all_of(expdesign$key),
          peptides,
          razor_unique_peptides,
          unique_peptides,
          reverse,
          potential_contaminant,
          only_identified_by_site
        ) %>%
        tidyr$pivot_longer(
          !c(gene_names,
             peptides,
             razor_unique_peptides,
             unique_peptides,
             reverse,
             potential_contaminant,
             only_identified_by_site),
          names_to = "key",
          values_to = "raw_intensity"
        ) %>%
        dplyr$inner_join(., expdesign, by = "key") %>%
        dplyr$mutate(bin_intensity = dplyr$if_else(is.na(raw_intensity), 0, 1)) %>%
        dplyr$select(-key)
      
      self$data <- data_standardized
    },
    data_wrangling = function(valid_val_filter = "alog", valid_val_thr = 0.75, pep_filter = "peptides", pep_thr, rev = TRUE, cont = TRUE, oibs = TRUE, rescue_cont = NULL) {
      
      ##############################################################
      #### this function is divided in 2 steps:                 ####
      #### the first apply filer specific to maxquant input.    ####
      #### the second part filer the data base on valid values. ####
      ##############################################################
      
      data <- self$data 
      
      if (self$input_type == "max_quant"){
        ### pep filter puo essere:
        ## c("peptides", "unique", "razor")
        
        data_wrang <- data %>%
          dplyr$mutate(potential_contaminant = dplyr$case_when(
            gene_names %in% rescue_cont ~ "", TRUE ~ potential_contaminant)) %>%
          ## remove reverse, potentialcontaminant and oibs from data base on user input
          {if(rev)dplyr$filter(., !reverse == "+") else .} %>%
          {if(cont)dplyr$filter(., !potential_contaminant == "+") else .} %>%
          {if(oibs)dplyr$filter(., !only_identified_by_site == "+") else .} %>%
          ## filter on peptides:
          {if(pep_filter == "peptides"){dplyr$filter(., peptides >= pep_thr)}
            else if (pep_filter == "unique") {dplyr$filter(., unique_peptides >= pep_thr)}
            else {dplyr$filter(., razor_unique_peptides >= pep_thr)}}
      }else{
        data_wrang <- data
      }
      
      ## different type of strategy for filter missing data:
      ## c("alog", "each_grp", "total") alog -> at least one group
      
      filtered_data <- data_wrang %>%
        {if(valid_val_filter == "total")dplyr$group_by(., gene_names)
          else dplyr$group_by(., gene_names, condition)} %>%
        dplyr$mutate(miss_val = dplyr$n() - sum(bin_intensity)) %>%
        dplyr$mutate(n_size = dplyr$n()) %>%
        dplyr$ungroup() %>%
        dplyr$group_by(gene_names) %>%
        ## rage compreso tra 0 e 100% espresso in valori tra 0 e 1
        {if(valid_val_filter == "alog") dplyr$filter(., any(miss_val <= round(n_size * (1 - valid_val_thr), 0)))
          else dplyr$filter(., all(miss_val <= round(n_size * (1 - valid_val_thr), 0)))} %>%
        dplyr$ungroup() %>%
        dplyr$select(gene_names, label, condition, replicate, bin_intensity, raw_intensity) %>% 
        dplyr$rename(intensity = raw_intensity)
      
      self$filtered_data <- filtered_data
      
    },
    normalization = function(norm_methods = "None"){
      
      data <- self$filtered_data
      
      if(norm_methods == "None"){
        self$is_norm <- FALSE
        self$normalized_data <- data
      }else{
        self$is_norm <- TRUE
        
        ## convert tibble data into a matrix
        raw_matrix <- data %>%
          tidyr$pivot_wider(id_cols = gene_names,
                             names_from = label,
                             values_from = intensity) %>%
          column_to_rownames("gene_names") %>%
          as.matrix()
        
        set.seed(11)
        ## Variance stabilization transformation on matrix
        vsn_fit <- vsn2(2 ^ raw_matrix, verbose = FALSE)
        norm_matrix <- predict(vsn_fit, 2 ^ raw_matrix)
        
        ## return a table with QProMS object format
        normalized_data <- norm_matrix %>%
          as_tibble(rownames = "gene_names") %>%
          tidyr$pivot_longer(cols = !gene_names,
                              names_to = "label",
                              values_to = "norm_intensity") %>%
          dplyr$full_join(data, .x, by = c("gene_names", "label")) %>%
          dplyr$mutate(intensity = norm_intensity) %>% 
          dplyr$select(-norm_intensity) %>% 
          dplyr$relocate(intensity, .after = dplyr$last_col())
        
        self$normalized_data <- normalized_data
      }
      
    },
    imputation = function(imp_methods = "mixed", shift = 1.8, scale = 0.3, unique_visual = FALSE) {
      
      data <- self$normalized_data %>% 
        dplyr$mutate(imputed = dplyr$if_else(bin_intensity == 1, FALSE, TRUE))
      
      if(imp_methods == "mixed"){
        self$is_mixed <- TRUE
      }else{
        self$is_mixed <- FALSE
      }
      
      if(imp_methods == "mixed" | imp_methods == "perseus"){
        self$is_imp <- TRUE
        
        if(self$is_mixed){
          data_mixed <- data %>%
            dplyr$group_by(gene_names, condition) %>%
            dplyr$mutate(for_mean_imp = dplyr$if_else((sum(bin_intensity) / dplyr$n()) >= 0.75, TRUE, FALSE)) %>%
            dplyr$mutate(mean_grp = mean(intensity, na.rm = TRUE)) %>%
            dplyr$ungroup() %>%
            dplyr$mutate(imp_intensity = dplyr$case_when(
              bin_intensity == 0 & for_mean_imp ~ mean_grp,
              TRUE ~ as.numeric(intensity))) %>%
            dplyr$mutate(intensity = imp_intensity) %>% 
            dplyr$select(-c(for_mean_imp, mean_grp, imp_intensity))
          
          data <- data_mixed
        }
        
        if(unique_visual){
          data_unique <- data %>%
            dplyr$group_by(gene_names, condition) %>%
            dplyr$mutate(miss_val = dplyr$n() - sum(bin_intensity)) %>%
            dplyr$mutate(n_size = dplyr$n()) %>%
            dplyr$ungroup() %>%
            dplyr$group_by(gene_names) %>%
            dplyr$filter(any(miss_val <= 0)) %>%
            dplyr$ungroup() %>%
            dplyr$filter(miss_val == n_size) %>% 
            dplyr$mutate(intensity = min(data$intensity, na.rm = TRUE), unique = TRUE) %>% 
            dplyr$select(-c(bin_intensity, miss_val, n_size)) %>% 
            dplyr$rename(unique_intensity = intensity) 
          
          data <- data %>% 
            dplyr$left_join(data_unique, by = c("gene_names", "label", "condition", "replicate")) %>% 
            dplyr$mutate(unique = if_else(is.na(unique), FALSE, unique)) %>% 
            dplyr$mutate(intensity = if_else(unique, unique_intensity, intensity)) %>% 
            dplyr$mutate(bin_intensity = if_else(unique, 1, bin_intensity)) %>% 
            dplyr$select(-c(unique_intensity, unique)) 
        }
        ## this funcion perform classical Perseus imputation
        ## sice use random nomral distibution i will set a set.seed()
        set.seed(11)
        
        imputed_data <- data %>%
          dplyr$group_by(label) %>%
          # Define statistic to generate the random distribution relative to sample
          dplyr$mutate(
            mean = mean(intensity, na.rm = TRUE),
            sd = sd(intensity, na.rm = TRUE),
            n = sum(!is.na(intensity)),
            total = nrow(data) - n
          ) %>%
          dplyr$ungroup() %>%
          # Impute missing values by random draws from a distribution
          # which is left-shifted by parameter 'shift' * sd and scaled by parameter 'scale' * sd.
          dplyr$mutate(imp_intensity = dplyr$case_when(
            is.na(intensity) ~ rnorm(total, mean = (mean - shift * sd), sd = sd * scale),
            TRUE ~ intensity
          )) %>%
          dplyr$mutate(intensity = imp_intensity) %>%
          dplyr$select(-c(mean, sd, n, total, imp_intensity))
        
        self$imputed_data <- imputed_data
        
      }else{
        self$is_imp <- FALSE
      }
    },
    print_table = function(data) {
      
      table <- data %>% 
        dplyr$select(gene_names, label, intensity) %>% 
        dplyr$mutate(intensity = round(intensity, 2)) %>% 
        tidyr$pivot_wider(gene_names, names_from = label, values_from = intensity) %>% 
        reactable(
          searchable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          height = "auto",
          columns = list(
            gene_names = colDef(
              minWidth = 200,
              sticky = "left",
              style = list(borderRight  = "1px solid #eee"
                           )
              )
            )
          )
      
      return(table)
    },
    plot_protein_counts = function(){
      
      # controllare che ci sia il self$filtered_data
      data <- self$filtered_data
      expdes <- self$expdesign
      
      p <- data %>%
        dplyr$group_by(label) %>%
        dplyr$summarise(counts = sum(bin_intensity)) %>%
        dplyr$ungroup() %>%
        dplyr$inner_join(., expdes, by = "label") %>%
        dplyr$mutate(replicate = as.factor(replicate)) %>%
        dplyr$group_by(condition) %>%
        echarts4r$e_charts(replicate, renderer = "svg") %>%
        echarts4r$e_bar(counts) %>%
        echarts4r$e_x_axis(name = "Replicates") %>%
        echarts4r$e_y_axis(name = "Counts") %>%
        echarts4r$e_tooltip(trigger = "item") %>%
        echarts4r$e_color(self$color_palette) %>%
        echarts4r$e_theme("QProMS_theme") %>% 
        echarts4r$e_y_axis(
          name = "Counts",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%
        echarts4r$e_x_axis(
          name = "Replicate",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            lineHeight = 60
          )
        ) %>% 
        echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore", "dataView")) %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      return(p)
    },
    plot_protein_coverage = function(){
      
      # controllare che ci sia il self$filtered_data
      data <- self$filtered_data
      
      p <- data %>%
        dplyr$group_by(gene_names) %>%
        dplyr$summarise(counts = sum(bin_intensity)) %>%
        dplyr$ungroup() %>%
        dplyr$select(counts) %>%
        table() %>%
        as_tibble() %>%
        dplyr$rename(occurrence = n) %>%
        echarts4r$e_charts(counts, renderer = "svg") %>%
        echarts4r$e_bar(occurrence) %>%
        echarts4r$e_y_axis(name = "Counts") %>%
        echarts4r$e_tooltip(trigger = "item") %>%
        echarts4r$e_color(self$color_palette) %>%
        echarts4r$e_theme("QProMS_theme") %>% 
        echarts4r$e_y_axis(
          name = "Counts",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>% 
        echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore", "dataView")) %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      return(p)
    },
    plot_distribution = function(){
      
      if(self$is_norm){
        data <- self$normalized_data
      }else{
        data <- self$filtered_data
      }
      
      intervals <- length(unique(self$expdesign$replicate))
      
      p <- data %>%
        dplyr$mutate(intensity = round(intensity, 2)) %>%
        dplyr$group_by(condition, label) %>%
        echarts4r$e_charts(renderer = "svg") %>%
        echarts4r$e_boxplot(
          intensity,
          colorBy = "data",
          layout = 'horizontal',
          outliers = FALSE,
          itemStyle = list(borderWidth = 3)
        ) %>%
        echarts4r$e_tooltip(trigger = "item") %>%
        echarts4r$e_color(self$color_palette) %>%
        echarts4r$e_theme("QProMS_theme") %>% 
        echarts4r$e_y_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>% 
        echarts4r$e_x_axis(axisLabel = list(interval = intervals)) %>% 
        echarts4r$e_toolbox_feature(feature = "saveAsImage") %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      return(p)
    },
    plot_cv = function(){
      
      if(self$is_norm){
        data <- self$normalized_data
      }else{
        data <- self$filtered_data
      }
      
      p <- data %>% 
        dplyr$group_by(gene_names, condition) %>% 
        dplyr$summarise(
          mean = mean(intensity, na.rm = TRUE),
          sd = sd(intensity, na.rm = TRUE),
          CV = round(sd / mean, 3)
        ) %>% 
        dplyr$ungroup() %>% 
        dplyr$group_by(condition) %>% 
        echarts4r$e_chart() %>% 
        echarts4r$e_boxplot(
          CV,
          colorBy = "data",
          outliers = FALSE,
          itemStyle = list(borderWidth = 3)
        ) %>%  
        echarts4r$e_tooltip(trigger = "axis") %>% 
        echarts4r$e_y_axis(
          name = "Densiry",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%  
        echarts4r$e_color(self$color_palette) %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      return(p)
    },
    plot_missing_data = function(){
      
      data <- self$filtered_data
      color <- viridis(n = 2 , direction = -1, end = 0.90, begin = 0.10, option = self$palette)
      
      p <- data %>%
        dplyr$group_by(label) %>%
        dplyr$mutate(bin_intensity = dplyr$if_else(bin_intensity == 1, "Valid", "Missing")) %>%
        dplyr$count(bin_intensity) %>%
        tidyr$pivot_wider(id_cols = label, names_from = bin_intensity, values_from = n) %>%
        {if(ncol(.) == 2) dplyr$mutate(., Missing = 0)else . } %>%
        dplyr$ungroup() %>%
        dplyr$mutate(total = Valid + Missing) %>%
        dplyr$mutate(perc_present = paste0(round(Valid*100/total, 1), "%")) %>%
        dplyr$mutate(perc_missing = paste0(round(Missing*100/total, 1), "%")) %>%
        echarts4r$e_charts(label, renderer = "svg") %>%
        echarts4r$e_bar(Valid, stack = "grp", bind = perc_present) %>%
        echarts4r$e_bar(Missing, stack = "grp", bind = perc_missing) %>%
        echarts4r$e_x_axis(name = "", axisLabel = list(interval = 0, rotate = 45)) %>%
        echarts4r$e_y_axis(name = "Counts") %>%
        echarts4r$e_tooltip(trigger = "item") %>%
        echarts4r$e_color(color) %>%
        echarts4r$e_theme("QProMS_theme") %>% 
        echarts4r$e_y_axis(
          name = "Counts",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>% 
        echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore", "dataView")) %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      return(p)
    },
    plot_missval_distribution = function(){
      
      if(self$is_norm){
        data <- self$normalized_data
      }else{
        data <- self$filtered_data
      }
      
      color <- viridis(n = 2 , direction = -1, end = 0.90, begin = 0.10, option = self$palette)
      
      p <- data %>%
        dplyr$group_by(gene_names) %>%
        dplyr$summarise(mean = mean(intensity, na.rm = TRUE),
                         missval = any(is.na(intensity))) %>%
        dplyr$ungroup() %>%
        dplyr$mutate(missing_value = dplyr$if_else(missval, "Missing", "Valid")) %>%
        dplyr$mutate(missing_value = factor(missing_value, levels = c("Valid", "Missing"))) %>%
        dplyr$group_by(missing_value) %>%
        echarts4r$e_charts(renderer = "svg") %>%
        echarts4r$e_histogram(mean) %>%
        echarts4r$e_x_axis(min = 10, max = 40) %>% 
        echarts4r$e_y_axis(
          name = "Densiry",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%  
        echarts4r$e_x_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            lineHeight = 60
          )
        ) %>%
        echarts4r$e_color(color) %>% 
        echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore")) %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      return(p)
    },
    plot_imputation = function(data, imp_visualization = FALSE){
      
      br <- pretty(10:40, n = 100)
      
      p <- data %>%
        dplyr$group_by(condition) %>%
        echarts4r$e_charts(renderer = "svg") %>%
        echarts4r$e_histogram(intensity, breaks = br) %>%
        echarts4r$e_color(self$color_palette) %>%
        echarts4r$e_x_axis(min = 10, max = 40) %>% 
        echarts4r$e_y_axis(
          name = "Densiry",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%  
        echarts4r$e_x_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            lineHeight = 60
          )
        ) %>%
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
        
      if(imp_visualization){
        imputed_dist <- self$imputed_data %>% 
          dplyr$filter(imputed) %>% 
          dplyr$group_by(condition)
        
        p <- data %>%
          dplyr$group_by(condition) %>%
          echarts4r$e_charts(renderer = "svg") %>%
          echarts4r$e_histogram(intensity, breaks = br) %>%
          echarts4r$e_color(c(self$color_palette, "#bc3754")) %>%
          echarts4r$e_x_axis(min = 10, max = 40) %>%
          echarts4r$e_data(imputed_dist, intensity) %>% 
          echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore")) %>% 
          echarts4r$e_histogram(intensity, name = "Imputed", breaks = br) %>%
          echarts4r$e_x_axis(min = 10, max = 40) %>%
          echarts4r$e_legend(selected = list('Imputed'= FALSE)) %>% 
          echarts4r$e_y_axis(
            name = "Density",
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 16,
              lineHeight = 60
            )
          ) %>%  
          echarts4r$e_x_axis(
            name = "log2 Intensity",
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 14,
              lineHeight = 60
            )
          ) %>%
          echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      }
      
      return(p)
    },
    plot_pca = function(view_3d = FALSE){
      
      # verificare che ci sia
      data <- self$imputed_data
      
      ## generate a matrix from imputed intensiy
      mat <- data %>%
        dplyr$select(gene_names, label, intensity) %>%
        tidyr$pivot_wider(id_cols = "gene_names",
                           names_from = "label",
                           values_from = "intensity") %>%
        column_to_rownames("gene_names") %>%
        as.matrix()
      
      ## perform PCA
      
      pca <- prcomp(t(mat), center = TRUE, scale = TRUE) 
      
      ## calculate persentage of each PC
      pca_var <- pca$sdev^2
      pca_var_perc <- round(pca_var/sum(pca_var)*100, 1)
      self$pcs <- pca_var_perc
      
      ## create a data.frame for the first 3 PC
      pca_table <- data.frame(
        label = rownames(pca$x),
        x = pca$x[, 1],
        y = pca$x[, 2],
        z = pca$x[, 3]
      ) %>% 
        dplyr$left_join(self$expdesign, by = "label") 
      
      ## generate plot
      if(!view_3d){
        p <- pca_table %>%
          dplyr$group_by(condition) %>%
          echarts4r$e_chart(x, renderer = "svg") %>%
          echarts4r$e_scatter(y, symbol_size = c(10, 10), bind = replicate) %>%
          echarts4r$e_tooltip(
            trigger = "item",
            formatter = JS("
        function(params){
          return('Rep: ' + params.name);
        }
      ")
          ) %>%
          echarts4r$e_x_axis(
            name = paste0("PC1 - ", pca_var_perc[1], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>%
          echarts4r$e_y_axis(
            name = paste0("PC2 - ", pca_var_perc[2], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>% 
          echarts4r$e_color(self$color_palette) %>% 
          echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore", "dataView")) %>% 
          echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      }else{
        p <- pca_table %>%
          dplyr$group_by(condition) %>%
          echarts4r$e_chart(x) %>%
          echarts4r$e_scatter_3d(y, z, symbol_size = c(10, 10), bind = replicate) %>%
          echarts4r$e_tooltip(
            trigger = "item",
            formatter = JS("
        function(params){
          return('Rep: ' + params.name);
        }
      ")
          ) %>%
          echarts4r$e_x_axis_3d(
            name = paste0("PC1 - ", pca_var_perc[1], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>%
          echarts4r$e_y_axis_3d(
            name = paste0("PC2 - ", pca_var_perc[2], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>%
          echarts4r$e_z_axis_3d(
            name = paste0("PC3 - ", pca_var_perc[3], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>%
          echarts4r$e_legend() %>% 
          echarts4r$e_color(self$color_palette) %>% 
          echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore")) %>% 
          echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      }
      
      return(p)
    },
    plot_correlation_interactive = function(cor_method = "pearson"){
      
      if(self$is_imp){
        data <- self$imputed_data
      }else{
        data <- self$normalized_data
      }
      
      color <- viridis(n = 3, direction = -1, end = 0.90, begin = 0.10, option = self$palette)
      
      mat <- data %>%
        dplyr$select(gene_names, label, intensity) %>%
        tidyr$pivot_wider(names_from = label, values_from = intensity) %>%
        dplyr$filter(dplyr$if_all(.cols = dplyr$everything(), .fns = ~ !is.na(.x))) %>%
        column_to_rownames("gene_names") %>%
        cor(method = cor_method) %>% 
        round(digits = 2)
      
      p <- mat %>% 
        echarts4r$e_charts(renderer = "svg") %>%
        echarts4r$e_correlations(order = "hclust", visual_map = FALSE) %>%
        echarts4r$e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
        echarts4r$e_y_axis(axisLabel = list(interval = 0, rotate = 0), position = "right") %>%
        echarts4r$e_tooltip(trigger = "item", formatter = JS("
          function(params){
          return('X: ' + params.value[0] + '<br />Y: ' + params.value[1] + '<br />Value: ' + params.value[2])
          }")) %>%
        echarts4r$e_visual_map(
          min = min(mat),
          max = 1,
          bottom = 150,
          inRange = list(color = color)
        ) %>%
        echarts4r$e_theme("QProMS_theme") %>% 
        echarts4r$e_toolbox_feature(feature = c("saveAsImage"))
        
      
      return(p)
    },
    plot_correlation_scatter = function(x, y) {
      
      if(self$is_imp){
        data <- self$imputed_data
      }else{
        data <- self$normalized_data
      }
      
      cols <- c(x, y)
      
      data_scatter <- data %>%
        dplyr$filter(label %in% cols) %>% 
        dplyr$select(gene_names, label, intensity) %>%
        tidyr$pivot_wider(gene_names, names_from = "label", values_from = "intensity") %>% 
        dplyr$select(x = !!cols[1], y = !!cols[2])
      
      if(x == y){
        p <- data_scatter %>% 
          echarts4r$e_charts() %>%
          echarts4r$e_histogram(x) %>% 
          echarts4r$e_x_axis(min = round(min(data_scatter, na.rm = TRUE)-1, 0), max = round(max(data_scatter, na.rm = TRUE)+1, 0)) %>%
          echarts4r$e_color(self$color_palette) %>%
          echarts4r$e_y_axis(
            name = "Bins",
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 16,
              lineHeight = 60
            )
          ) %>%
          echarts4r$e_x_axis(
            name = cols[1],
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 14,
              lineHeight = 60
            )
          ) %>%
          echarts4r$e_toolbox_feature(feature = "saveAsImage") %>% 
          echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      }else{
        p <- data_scatter %>%
          echarts4r$e_charts(x, dispose = FALSE) %>%
          echarts4r$e_scatter(y, legend = FALSE, symbol_size = 5) %>%
          echarts4r$e_x_axis(min = round(min(data_scatter, na.rm = TRUE)-1, 0), max = round(max(data_scatter, na.rm = TRUE)+1, 0)) %>%
          echarts4r$e_y_axis(min = round(min(data_scatter, na.rm = TRUE)-1, 0), max = round(max(data_scatter, na.rm = TRUE)+1, 0)) %>%
          echarts4r$e_color(self$color_palette) %>%
          echarts4r$e_y_axis(
            name = cols[1],
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 16,
              lineHeight = 60
            )
          ) %>%
          echarts4r$e_x_axis(
            name = cols[2],
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 14,
              lineHeight = 60
            )
          ) %>%
          echarts4r$e_toolbox_feature(feature = "saveAsImage") %>% 
          echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      }
      
      return(p)
    },
    plot_correlation_static = function(cor_method = "pearson", single_condition = NULL){
      
      if(self$is_imp){
        data <- self$imputed_data
      }else{
        data <- self$normalized_data
      }
      
      p <- data %>% 
        {if(!is.null(single_condition)) dplyr$filter(., condition == single_condition) else .}%>%
        dplyr$select(gene_names, label, intensity) %>%
        tidyr$pivot_wider(names_from = label, values_from = intensity) %>% 
        dplyr$ungroup() %>% 
        ggcorrm(corr_method = cor_method) +
        utri_heatmap(alpha = 0.5) +
        utri_corrtext(corr_size = FALSE) +
        dia_names(y_pos = 0.15, size = 3) +
        dia_histogram(fill = "white", color = 1) +
        lotri(geom_point(alpha = 0.5, size = 0.8)) +
        scale_fill_viridis_c(direction = -1, end = 0.90, begin = 0.10, option = self$palette)
      
      return(p)
      
    }
  )
)