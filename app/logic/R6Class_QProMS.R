box::use(
  R6[R6Class],
  data.table[fread],
  janitor[make_clean_names, get_dupes],
  tibble[as_tibble, column_to_rownames],
  viridis[viridis],
  magrittr[`%>%`],
  stats[sd],
  dplyr,
  tidyr,
  stringr,
  echarts4r,
  vsn[vsn2, predict],
)

#' @export
QProMS <- R6Class(
  classname = "QProMS",
  public = list(
    ####################
    # Input parameters #
    data = NULL,
    input_type = "max_quant",
    intensity_type = "lfq_intensity_",
    organism = NULL, #questo potrebbe essere tolto visto usiamo solo humans
    expdesign = NULL,
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
    norm_methods = NULL,
    is_norm = FALSE,
    vsn_norm_run_once = FALSE,
    ############################
    # parameters for imputation #
    imputed_data = NULL,
    imp_methods = NULL,
    is_mixed = NULL,
    is_imp = FALSE,
    imp_run_once = FALSE,
    #################
    # parameters For Statistics #
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
      
      self$data <- fread(input = input_path) %>%
        as_tibble(.name_repair = make_clean_names)
      
      self$input_type <- input_type
    },
    define_colors = function(){
      n_of_color <- max(self$expdesign %>% dplyr$count(replicate) %>% dplyr$pull(n))
      self$color_palette <- viridis(n = n_of_color , direction = -1, end = 0.70, begin = 0.30)
    },
    total_missing_data = function(raw = TRUE){
      
      if(raw){
        data <- self$data
      }else{
        data <- self$filtered_data
      }
      
      data %>%
        dplyr$count(bin_intensity) %>%
        dplyr$mutate(missing = paste0(round(n/nrow(data)*100,0), " %")) %>%
        dplyr$filter(bin_intensity == 0) %>%
        dplyr$pull(missing)
    },
    make_expdesign = function(start_with = "lfq_intensity_"){
      ## qui mettere tutti gli if in base all'intensity type
      
      self$data <- self$data %>%
        dplyr$mutate(dplyr$across(dplyr$starts_with(start_with), ~ as.numeric(.)))
      
      self$expdesign <- self$data %>%
        dplyr$select(gene_names, dplyr$starts_with(start_with)) %>%
        tidyr$pivot_longer(!gene_names, names_to = "key", values_to = "intensity") %>%
        dplyr$distinct(key) %>%
        dplyr$mutate(label = stringr$str_remove(key, start_with)) %>%
        dplyr$mutate(condition = stringr$str_remove(label, "_[^_]*$")) %>%
        dplyr$mutate(replicate = stringr$str_remove(label, ".*_"))
      
      self$define_colors()
      
      if(self$input_type == "max_quant"){
        self$pg_preprocessing()
      }
    },
    define_tests = function(){
      conditions <-
        dplyr$distinct(self$expdesign, condition) %>% dplyr$pull(condition)
      
      tests <-
        tidyr$expand_grid(cond1 = conditions, cond2 = conditions) %>%
        dplyr$filter(cond1 != cond2) %>%
        dplyr$mutate(test = paste0(cond1, "_vs_", cond2)) %>%
        dplyr$pull(test)
      
      return(tests)
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
      
      data <- self$data
      expdesign <- self$expdesign
      
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
        dplyr$right_join(self$data, by = "id") %>%
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
        dplyr$mutate(raw_intensity = log2(raw_intensity)) %>%
        dplyr$mutate(raw_intensity = dplyr$na_if(raw_intensity, -Inf)) %>%
        dplyr$mutate(bin_intensity = dplyr$if_else(is.na(raw_intensity), 0, 1)) %>%
        dplyr$select(-key)
      
      self$data <- data_standardized
    },
    data_wrangling = function(valid_val_filter = "alog", valid_val_thr = 0.75, 
                              pep_filter = "peptides", pep_thr, 
                              rev = TRUE, cont = TRUE, oibs = TRUE, rescue_cont = NULL) {
      
      ##############################################################
      #### this function is divided in 2 steps:                 ####
      #### the first apply filer specific to maxquant input.    ####
      #### the second part filer the data base on valid values. ####
      ##############################################################
      
      # setup object parameters
      self$vsn_norm_run_once <- FALSE
      self$imp_run_once <- FALSE
      
      selected_cond <-
        self$expdesign %>% 
        dplyr$distinct(label) %>% 
        dplyr$pull()
      
      data <- self$data %>% 
        dplyr$filter(label %in% selected_cond)
      
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
        echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore", "dataView"))
      
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
        echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore", "dataView"))
      
      return(p)
    },
    plot_distribution = function(){
      
      if(self$is_norm){
        data <- self$normalized_data
      }else{
        data <- self$filtered_data
      }
      
      p <- data %>%
        dplyr$mutate(intensity = round(intensity, 2)) %>%
        dplyr$group_by(condition, label) %>%
        echarts4r$e_charts(renderer = "svg") %>%
        echarts4r$e_boxplot(
          intensity,
          colorBy = "data",
          layout = 'horizontal',
          outliers = FALSE,
          itemStyle = list(borderWidth = 2)
        ) %>%
        echarts4r$e_tooltip(trigger = "item") %>%
        echarts4r$e_color(self$color_palette) %>%
        echarts4r$e_theme("QProMS_theme") %>% 
        echarts4r$e_y_axis(
          name = "Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>% 
        echarts4r$e_toolbox_feature(feature = "saveAsImage")
      
      return(p)
    },
    plot_cv = function(){
      
      # controllare che ci sia il self$filtered_data
      data <- self$filtered_data
      
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
          itemStyle = list(borderWidth = 2)
        ) %>%  
        echarts4r$e_tooltip(trigger = "axis") %>% 
        echarts4r$e_color(self$color_palette)
      
      return(p)
    }
  )
)