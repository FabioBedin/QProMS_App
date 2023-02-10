box::use(
  R6[R6Class],
  data.table[fread],
  janitor[make_clean_names, get_dupes],
  tibble[as_tibble],
  viridis[viridis],
  magrittr[`%>%`],
  dplyr,
  tidyr,
  stringr,
  echarts4r,
)

#' @export
QProMS <- R6Class(
  classname = "QProMS",
  public = list(
    ####################
    # Input parameters #
    data = NULL,
    input_type = NULL,
    intensity_type = NULL,
    organism = NULL,
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
      self$color_palette <- viridis(n = n_of_color , direction = -1, end = 0.75)
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
      
      list_unique_gene_names <- data %>%
        dplyr$select(protein_i_ds, gene_names, id) %>%
        dplyr$mutate(gene_names = stringr$str_extract(gene_names, "[^;]*")) %>%
        ## every protein gorups now have only 1 gene name associated to it
        dplyr$rename(unique_gene_names = gene_names) %>%
        get_dupes(unique_gene_names) %>%
        dplyr$mutate(unique_gene_names = dplyr$case_when(
          unique_gene_names != "" ~ paste0(unique_gene_names, "__",
                                           stringr$str_extract(protein_i_ds, "[^;]*")),
          TRUE ~ stringr$str_extract(protein_i_ds, "[^;]*"))) %>%
        dplyr$select(unique_gene_names, id)
      
      ## update data that now don't have dupe or missing spot
      data_unique <- dplyr$left_join(data, list_unique_gene_names, by = "id") %>%
        dplyr$mutate(gene_names = dplyr$case_when(
          unique_gene_names != "" ~ unique_gene_names, 
          TRUE ~ gene_names)) %>%
        dplyr$select(-unique_gene_names) %>%
        dplyr$mutate(gene_names = stringr$str_extract(gene_names, "[^;]*"))
      
      ### this second part standardize the data in the right format
      
      data_standardized <- data_unique %>% 
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
      
      # store inputs for summary table
      # self$valid_val_filter <- valid_val_filter
      # self$valid_val_thr <- valid_val_thr
      # self$pep_filter <- pep_filter
      # self$pep_thr <- pep_thr
      # self$rev <- rev
      # self$cont <- cont
      # self$oibs <- oibs
      
      
      
      # setup object parameters
      self$vsn_norm_run_once <- FALSE
      self$imp_run_once <- FALSE
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
    }
  )
)