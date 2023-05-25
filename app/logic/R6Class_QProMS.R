box::use(
  R6[R6Class],
  data.table[fread],
  janitor[make_clean_names, get_dupes],
  tibble[tibble, as_tibble, column_to_rownames, rownames_to_column, deframe, enframe, rowid_to_column],
  viridis[viridis],
  magrittr[`%>%`],
  stats[sd, rnorm, prcomp, cor, na.omit, t.test, p.adjust, wilcox.test, aov, setNames, hclust, dist, cutree, qt, median],
  utils[combn],
  htmltools,
  dplyr,
  tidyr,
  stringr,
  purrr[map, map2, reduce, map_dbl, pluck, list_rbind, set_names, possibly, flatten],
  reactable[reactable, colDef],
  echarts4r,
  htmlwidgets[JS],
  vsn[vsn2, predict],
  corrmorant[...],
  ggplot2[scale_fill_viridis_c, geom_point],
  iheatmapr[...],
  clusterProfiler[enrichGO, filter, simplify, gseGO],
  org.Hs.eg.db[org.Hs.eg.db],
  rbioapi[rba_string_interactions_network],
  OmnipathR[get_complex_genes, import_omnipath_complexes],
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
    external_genes_column = NULL,
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
    primary_condition = NULL,
    additional_condition = NULL,
    univariate = NULL,
    univariate_test_type = NULL,
    univariate_paired = NULL,
    stat_table = NULL,
    univariate_alpha = 0.05,
    univariate_p_adj_method = "BH",
    fold_change = 1,
    anova_table = NULL,
    anova_alpha = 0.05,
    anova_p_adj_method = "BH",
    anova_clust_method = "hclust",
    z_score = TRUE,
    anova_manual_order = FALSE,
    anova_col_order = NULL,
    clusters_def = NULL,
    clusters_number = 0,
    #################
    # parameters For ORA #
    ora_result_list = NULL,
    ora_result_list_simplified = NULL,
    ora_table = NULL,
    go_ora_from_statistic = NULL,
    go_ora_tested_condition = NULL,
    go_ora_alpha = 0.05,
    go_ora_p_adj_method = "BH",
    go_ora_term = "BP",
    go_ora_focus = NULL,
    go_ora_top_n = NULL,
    go_ora_simplify_thr = 0.7,
    go_ora_plot_value = "fold_change",
    #######################
    # parameters for GSEA #
    gsea_result_list = NULL,
    gsea_result_list_simplified = NULL,
    gsea_table = NULL,
    go_gsea_tested_condition = NULL,
    go_gsea_alpha = 0.05,
    go_gsea_p_adj_method = "BH",
    go_gsea_term = "BP",
    go_gsea_focus = NULL,
    go_gsea_top_n = NULL,
    go_gsea_common_terms = FALSE,
    go_gsea_simplify_thr = 0.7,
    ##########################
    # parameters for network #
    nodes_table = NULL,
    edges_table = NULL,
    name_for_edges = NULL,
    network_from_statistic = NULL,
    network_focus =  "cluster_1",
    selected_nodes = NULL,
    pdb_database = NULL,
    ###########
    # Methods #
    loading_data = function(input_path, input_type) {
      
      self$raw_data <- fread(input = input_path) %>%
        as_tibble(.name_repair = make_clean_names)
      
      self$input_type <- input_type
    },
    define_colors = function() {
      n_of_color <- max(self$expdesign %>% dplyr$distinct(condition) %>% nrow())
      self$color_palette <- viridis(n = n_of_color , direction = -1, end = 0.90, begin = 0.10, option = self$palette)
    },
    total_missing_data = function(raw = TRUE) {
      
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
    missing_data_type = function(type) {
      
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
    make_expdesign = function(intensity_type = "lfq_intensity_", genes_column = NULL) {
      ## qui mettere tutti gli if in base all'intensity type
      
      self$intensity_type <- intensity_type
      self$external_genes_column <- genes_column
      
      if(self$input_type == "max_quant"){
        
        data <- self$raw_data %>% 
          dplyr$mutate(dplyr$across(dplyr$starts_with(intensity_type), ~ log2(.))) %>%
          dplyr$mutate(dplyr$across(dplyr$starts_with(intensity_type), ~ dplyr$na_if(.,-Inf)))
        
        self$expdesign <- data %>%
          dplyr$select(gene_names, dplyr$starts_with(intensity_type)) %>%
          tidyr$pivot_longer(!gene_names, names_to = "key", values_to = "intensity") %>%
          dplyr$distinct(key) %>%
          dplyr$mutate(label = stringr$str_remove(key, intensity_type)) %>%
          dplyr$mutate(condition = stringr$str_remove(label, "_[^_]*$")) %>%
          dplyr$mutate(replicate = stringr$str_remove(label, ".*_"))
        
      }else{
        if (is.null(genes_column)) {
          stop("Error! Provide a valid column for gene names.")
        } else{
          data <- self$raw_data %>% 
            dplyr$mutate(dplyr$across(dplyr$matches(intensity_type), ~ log2(.))) %>%
            dplyr$mutate(dplyr$across(dplyr$matches(intensity_type), ~ dplyr$na_if(.,-Inf))) %>% 
            dplyr$rename(gene_names := !!genes_column)
          
          self$expdesign <- data %>%
            dplyr$select(gene_names, dplyr$matches(intensity_type)) %>%
            tidyr$pivot_longer(!gene_names, names_to = "key", values_to = "intensity") %>%
            dplyr$distinct(key) %>%
            dplyr$mutate(label = stringr$str_remove(key, intensity_type)) %>%
            dplyr$mutate(condition = "") %>%
            dplyr$mutate(replicate = "")
        }
      }
      
    },
    define_tests = function() {
      conditions <-
        dplyr$distinct(self$expdesign, condition) %>% dplyr$pull(condition)
      
      tests <-
        tidyr$expand_grid(cond1 = conditions, cond2 = conditions) %>%
        dplyr$filter(cond1 != cond2) %>%
        dplyr$mutate(test = paste0(cond1, "_vs_", cond2)) %>%
        dplyr$pull(test)
      
      self$all_test_combination <- tests
    },
    pg_preprocessing = function() {
      ########################################################################
      #### This function prepare the proteing groups in the QProMS format ####
      #### and remove duplicates.                                         ####
      ########################################################################
      
      ### this firts part remove duplicate and missing gene names
      ### in proteinGroups.txt input
      
      ## Indentify all duplicate gene names 
      ## and add after __ the protein iD
      
      expdesign <- self$expdesign
      
      self$define_colors()
      self$define_tests()
      self$primary_condition <- self$all_test_combination[1]
      
      if(self$input_type == "max_quant"){
        data <- self$raw_data %>%
          dplyr$mutate(dplyr$across(dplyr$starts_with(self$intensity_type), ~ log2(.))) %>%
          dplyr$mutate(dplyr$across(dplyr$starts_with(self$intensity_type), ~ dplyr$na_if(.,-Inf)))
        
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
      }else{
        data_standardized <- self$raw_data %>% 
          dplyr$mutate(dplyr$across(dplyr$matches(self$intensity_type), ~ log2(.))) %>%
          dplyr$mutate(dplyr$across(dplyr$matches(self$intensity_type), ~ dplyr$na_if(.,-Inf))) %>% 
          dplyr$rename(gene_names := !!self$external_genes_column) %>% 
          dplyr$select(
            gene_names,
            dplyr$all_of(expdesign$key)
          ) %>%
          tidyr$pivot_longer(
            !gene_names,
            names_to = "key",
            values_to = "raw_intensity"
          ) %>%
          dplyr$inner_join(., expdesign, by = "key") %>%
          dplyr$mutate(bin_intensity = dplyr$if_else(is.na(raw_intensity), 0, 1)) %>%
          dplyr$select(-key)
        
        self$data <- data_standardized
      }
      
      
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
    normalization = function(norm_methods = "None") {
      
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
          wrap = FALSE,
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
    print_stat_table = function(stat_table, test){
      
      table <- stat_table %>% 
        dplyr$select(gene_names, dplyr$starts_with(test)) %>% 
        dplyr$rename_at(dplyr$vars(dplyr$matches(test)), ~ stringr$str_remove(., paste0(test, "_"))) %>% 
        dplyr$arrange(-significant, -fold_change, p_val) %>% 
        dplyr$mutate(dplyr$across(c("p_val", "p_adj"), ~ -log10(.))) %>% 
        dplyr$mutate(dplyr$across(c("fold_change", "p_val", "p_adj"), ~ round(., 2)))
      
      return(table)
    },
    print_anova_table = function() {
      
      table <- self$anova_table %>% 
        dplyr$select(gene_names, p_val, p_adj, significant, cluster) %>%
        dplyr$arrange(p_val,-significant) %>%
        dplyr$mutate(dplyr$across(c("p_val", "p_adj"), ~ -log10(.))) %>%
        dplyr$mutate(dplyr$across(c("p_val", "p_adj"), ~ round(., 2)))
      
      return(table)
    },
    print_ora_table = function(ontology = "BP", groups, value) {
      
      self$ora_table <- map(self$ora_result_list_simplified, ~ pluck(.x, "result")) %>%
        list_rbind(names_to = "group") %>% 
        as_tibble() %>%
        dplyr$filter(ONTOLOGY == ontology) %>%
        dplyr$filter(group %in% groups) %>% 
        tidyr$separate(GeneRatio, into = c("a", "b"), sep = "/", remove = FALSE) %>%
        tidyr$separate(BgRatio, into = c("c", "d"), sep = "/", remove = FALSE) %>%
        dplyr$mutate(fold_change = (as.numeric(a)/as.numeric(b))/(as.numeric(c)/as.numeric(d))) %>% 
        dplyr$select(-c(a,b,c,d)) %>% 
        dplyr$mutate(dplyr$across(c("pvalue", "p.adjust", "qvalue"), ~ -log10(.))) %>%
        dplyr$mutate(dplyr$across(c("pvalue", "p.adjust", "qvalue", "fold_change"), ~ round(., 2))) %>% 
        dplyr$relocate(ID) %>% 
        dplyr$relocate(geneID, .after = dplyr$last_col()) %>% 
        dplyr$relocate(Count, .after = fold_change) 
      
      if(value == "fold_change") {
        self$ora_table <- self$ora_table %>% 
          dplyr$arrange(-fold_change)
      } else {
        self$ora_table <- self$ora_table %>% 
          dplyr$arrange(-pvalue)
      }
      
    },
    print_gsea_table = function(ontology = "BP", groups, only_common) {
      
      self$gsea_table <- map(self$gsea_result_list_simplified, ~ pluck(.x, "result")) %>%
        list_rbind(names_to = "group") %>%
        as_tibble() %>%
        dplyr$filter(ONTOLOGY == ontology) %>%
        dplyr$filter(group %in% groups) %>% 
        dplyr$select(-leading_edge) %>% 
        dplyr$mutate(dplyr$across(c("pvalue", "p.adjust", "qvalue"), ~ -log10(.))) %>%
        dplyr$mutate(dplyr$across(c(
          "pvalue", "p.adjust", "qvalue", "NES", "enrichmentScore"
        ), ~ round(., 2))) %>%
        dplyr$relocate(ID) %>%
        {if(only_common)get_dupes(., Description) %>% 
            dplyr$select(., -dupe_count) else .} %>%
        dplyr$arrange(-pvalue) 
      
    },
    print_nodes = function(isolate_nodes, score_thr) {
      
      edges <- self$edges_table %>% 
        dplyr$filter(score >= score_thr)
      nodes <- self$nodes_table
      
      if(!isolate_nodes) {
        
        edge_source <- edges %>% dplyr$pull(source)
        edge_target <- edges %>% dplyr$pull(target)
        
        list <- c(edge_source, edge_target)
        final_list <- unique(list)
        
        nodes <- nodes %>%
          dplyr$filter(gene_names %in% final_list)
        
      }
      
      nodes_tab <- nodes %>% 
        dplyr$select(c(gene_names, category, p_val, p_adj))
        
      return(nodes_tab)
      
    },
    print_edges = function(score_thr, selected_nodes) {
      
      self$edges_table %>% 
        dplyr$filter(score >= score_thr) %>% 
        {if(length(selected_nodes) != 0)dplyr$filter(., source %in% selected_nodes | target %in% selected_nodes) else .} %>%
        dplyr$select(-c(color, size)) %>% 
        dplyr$mutate(score = dplyr$if_else(complex == "not defined", round(score, 2), 1)) %>% 
        tidyr$separate_rows(complex, sep = ",") %>% 
        dplyr$relocate(complex, .after = database) %>% 
        reactable(
          searchable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          wrap = FALSE,
          paginateSubRows = TRUE,
          height = "auto",
          defaultPageSize = 7,
          # selection = "multiple",
          # onClick = "select",
          groupBy = "source",
          columns = list(
            target = colDef(aggregate = "unique", minWidth = 100, name = "Target"),
            source = colDef(name = "Source"),
            database = colDef(name = "Database"),
            complex = colDef(minWidth = 300, name = "Complex"),
            score = colDef(align = "center", name = "Score")
          )
        )
        
    },
    stat_tidy_vector = function(data, name) {
      
      tidy_vec <- data %>%
        as_tibble(rownames = NA) %>%
        rownames_to_column(var = "gene_names") %>%
        dplyr$rename(!!name := value)
      
      return(tidy_vec)
    },
    stat_t_test_single = function(data, test, fc, alpha, p_adj_method, paired_test, test_type) {
      
      cond_1 <- stringr$str_split(test, "_vs_")[[1]][1]
      cond_2 <- stringr$str_split(test, "_vs_")[[1]][2]
      
      if(test_type == "student"){
        var_equal <- TRUE
      }else{
        var_equal <- FALSE
      }
      
      self$univariate <- TRUE
      
      mat <- data %>%
        dplyr$filter(condition == cond_1 | condition == cond_2) %>%
        dplyr$mutate(label_test = paste(condition, replicate, sep = "_")) %>%
        tidyr$pivot_wider(id_cols = "gene_names",
                           names_from = "label_test",
                           values_from = "intensity") %>%
        column_to_rownames("gene_names") %>%
        dplyr$relocate(dplyr$contains(cond_2), .after = dplyr$last_col()) %>%
        na.omit() %>% 
        as.matrix()
      
      a <- grep(cond_1, colnames(mat))
      b <- grep(cond_2, colnames(mat))
      
      if(test_type == "wilcox"){
        p_values_vec <- apply(mat, 1, function(x) wilcox.test(x[a], x[b], paired = paired_test)$p.value)
      }else{
        p_values_vec <- apply(mat, 1, function(x) t.test(x[a], x[b], paired = paired_test, var.equal = var_equal)$p.value)
      }
      
      
      p_values <- p_values_vec %>%
        self$stat_tidy_vector(name = "p_val")
      
      fold_change <- apply(mat, 1, function(x) mean(x[a]) - mean(x[b])) %>% 
        self$stat_tidy_vector(name = "fold_change")
      
      p_ajusted <- p.adjust(p_values_vec, method = p_adj_method) %>% 
        self$stat_tidy_vector(name = "p_adj")
      
      stat_data <- fold_change %>% 
        dplyr$full_join(., p_values, by = "gene_names") %>% 
        dplyr$full_join(., p_ajusted, by = "gene_names") %>% 
        dplyr$mutate(significant = dplyr$if_else(abs(fold_change) >= fc & p_adj <= alpha, TRUE, FALSE)) %>% 
        dplyr$rename(!!paste0(cond_1, "_vs_", cond_2, "_significant") := significant) %>% 
        dplyr$rename(!!paste0(cond_1, "_vs_", cond_2, "_p_val") := p_val) %>% 
        dplyr$rename(!!paste0(cond_1, "_vs_", cond_2, "_fold_change") := fold_change) %>% 
        dplyr$rename(!!paste0(cond_1, "_vs_", cond_2, "_p_adj") := p_adj)
      
      return(stat_data)
    },
    stat_t_test = function(test, fc = 1, alpha = 0.05, p_adj_method = "BH", paired_test = FALSE, test_type = "student") {
      
      if(!self$is_norm & !self$is_imp){
        data <- self$filtered_data
      }else if(self$is_norm & !self$is_imp){
        data <- self$normalized_data
      }else{
        data <- self$imputed_data
      }
      
      if(length(test) == 1){
        stat_table_single <-
          self$stat_t_test_single(
            data = data,
            test = test,
            fc = fc,
            alpha = alpha,
            p_adj_method = p_adj_method,
            paired_test = paired_test, 
            test_type = test_type
          )
        
        complete_stat_table <- 
          data %>% 
          tidyr$pivot_wider(id_cols = "gene_names",
                             names_from = "label",
                             values_from = "intensity") %>%
          dplyr$left_join(stat_table_single, by = "gene_names")
        
      } else {
        stat_table_map <-
          map(
            .x = test,
            .f = ~ self$stat_t_test_single(
              data = data,
              test = .x,
              fc = fc,
              alpha = alpha,
              p_adj_method = p_adj_method, 
              paired_test = paired_test, 
              test_type = test_type
            )
          ) %>%
          reduce(dplyr$full_join, by = "gene_names")
        
        complete_stat_table <- 
          data %>%
          tidyr$pivot_wider(id_cols = "gene_names",
                             names_from = "label",
                             values_from = "intensity") %>%
          dplyr$left_join(stat_table_map, by = "gene_names")
      }
      
      
      self$stat_table <- complete_stat_table
      
    },
    stat_anova = function(alpha = 0.05, p_adj_method = "BH") {
      
      if(!self$is_norm & !self$is_imp){
        data <- self$filtered_data
      }else if(self$is_norm & !self$is_imp){
        data <- self$normalized_data
      }else{
        data <- self$imputed_data
      }
      
      data <- data %>% 
        dplyr$group_by(gene_names) %>%
        dplyr$filter(!any(is.na(intensity))) %>%
        dplyr$ungroup()
      
      self$univariate <- FALSE # questo forse non servirà più
      
      p_values_vec <- data %>%
        split(.$gene_names) %>%
        map_dbl(~ summary(aov(intensity ~ condition, .x))[[1]][["Pr(>F)"]][[1]]) %>%
        data.frame() %>%
        rownames_to_column(var = "gene_names") %>%
        deframe()
      
      p_values <- p_values_vec %>%
        self$stat_tidy_vector(name = "p_val")
      
      p_ajusted <- p.adjust(p_values_vec, method = p_adj_method) %>% 
        self$stat_tidy_vector(name = "p_adj")
      
      stat_data <- data %>% 
        tidyr$pivot_wider(id_cols = "gene_names", names_from = "label", values_from = "intensity") %>% 
        dplyr$full_join(p_values, by = "gene_names") %>% 
        dplyr$full_join(p_ajusted, by = "gene_names") %>% 
        dplyr$mutate(significant = dplyr$if_else(p_adj <= alpha, TRUE, FALSE)) %>% 
        dplyr$mutate(cluster = "not_defined")
      
      self$anova_table <- stat_data
    },
    go_ora_make_grp_data = function(test) {
      
      groupped_data <- self$stat_table %>%
        dplyr$select(gene_names, dplyr$starts_with(test)) %>%
        dplyr$rename_at(dplyr$vars(dplyr$matches(test)),
                         ~ stringr$str_remove(., paste0(test, "_"))) %>%
        dplyr$filter(significant)
      
      if(nrow(groupped_data) > 0) {
        groupped_data <- groupped_data %>%
          dplyr$mutate(direction = dplyr$if_else(fold_change > 0, paste0(test, "_up"), paste0(test, "_down"))) %>%
          dplyr$select(gene_names, direction) 
      } else {
        groupped_data <- tibble(
          gene_names = c("NO_Significant", "NO_Significant"),
          direction = c(paste0(test, "_up"), paste0(test, "_down"))
        )
      }
      
      return(groupped_data)
      
    },
    go_ora = function(list_from, test, alpha, p_adj_method, background) {
      
      if (list_from == "univariate") {
        groupped_data <-
          map(
            .x = test,
            .f = ~ self$go_ora_make_grp_data(test = .x)) %>% 
          reduce(dplyr$bind_rows) %>% 
          dplyr$group_by(direction)
        
        uni <- self$stat_table %>%
          dplyr$pull(gene_names)
      } else if (list_from == "multivariate"){
        groupped_data <- self$anova_table %>%
          dplyr$filter(significant) %>%
          dplyr$select(gene_names, cluster) %>%
          dplyr$group_by(cluster)
        
        if(nrow(groupped_data) == 0) {
          groupped_data <- tibble(
            gene_names = "NO_Significant",
            cluster = "not_defined"
          )
        }
        
        uni <- self$anova_table %>%
          dplyr$pull(gene_names)
      }
      
      if (!background) {
        uni <- NULL
      }
      
      if (list_from == "nodes") {
        unnamed_gene_lists <- self$selected_nodes %>% list()
        gene_vector <- set_names(unnamed_gene_lists, "nodes")
        uni <- NULL
      } else {
        unnamed_gene_lists <-
          groupped_data %>% dplyr$group_map(~ dplyr$pull(.x, gene_names))
        
        gene_vector <-
          set_names(unnamed_gene_lists, dplyr$group_keys(groupped_data) %>% dplyr$pull())
      }
      
      self$ora_result_list <- map(
        .x = gene_vector,
        .f = possibly(
          ~ enrichGO(
            gene          = .x,
            OrgDb         = org.Hs.eg.db,
            keyType       = 'SYMBOL',
            ont           = "ALL",
            pAdjustMethod = p_adj_method,
            universe      = uni,
            readable      = TRUE
          ) %>% filter(p.adjust < alpha)
        )
      )
      
    },
    go_simplify = function(thr, type) {
      
      if (type == "ora") {
        self$ora_result_list_simplified <- map(
          .x = self$ora_result_list,
          .f = possibly( ~ simplify(.x, cutoff = thr))
        )
      } else {
        self$gsea_result_list_simplified <- map(
          .x = self$gsea_result_list,
          .f = possibly( ~ simplify(.x, cutoff = thr))
        )
      }
      
    },
    go_gsea_rank_vector = function(test) {
      
      cond_1 <- stringr$str_split(test, "_vs_")[[1]][1]
      cond_2 <- stringr$str_split(test, "_vs_")[[1]][2]
      
      # if (!self$is_norm & !self$is_imp) {
      #   data <- self$filtered_data
      # } else if (self$is_norm & !self$is_imp) {
      #   data <- self$normalized_data
      # } else{
      #   data <- self$imputed_data
      # }
      
      gsea_vec <- self$imputed_data %>%
        dplyr$filter(condition == cond_1 | condition == cond_2) %>%
        dplyr$group_by(gene_names, condition) %>%
        dplyr$summarise(mean = mean(intensity)) %>%
        tidyr$pivot_wider(names_from = condition, values_from = mean) %>%
        dplyr$ungroup() %>%
        dplyr$mutate(fold_change = get(cond_1) - get(cond_2)) %>%
        dplyr$arrange(-fold_change) %>%
        dplyr$select(gene_names, fold_change) %>%
        deframe() %>%
        list() 
      
      gsea_list_vec <- set_names(gsea_vec, test)
      
      return(gsea_list_vec)
      
    },
    go_gsea = function(test, alpha, p_adj_method) {
      
      list_of_gesa_vector <- map(
        .x = test,
        .f = ~ self$go_gsea_rank_vector(test = .x)
      ) %>% flatten()
      
      defaultW <- getOption("warn") 
      
      options(warn = -1) 
      
      self$gsea_result_list <- map(
        .x = list_of_gesa_vector,
        .f = possibly(
          ~ gseGO(
            geneList     = .x,
            OrgDb        = org.Hs.eg.db,
            ont          = "ALL",
            keyType      = 'SYMBOL',
            pAdjustMethod = p_adj_method,
            verbose      = FALSE
          ) %>% filter(p.adjust < alpha)
        )
      )
      
      options(warn = defaultW)
      
    },
    make_nodes = function(list_from, focus, direction) {
      
      if (list_from == "univariate") {
        
        nodes_table <- self$stat_table %>%
          dplyr$select(gene_names, dplyr$starts_with(focus)) %>%
          dplyr$rename_at(dplyr$vars(dplyr$matches(focus)), ~ stringr$str_remove(., paste0(focus, "_"))) %>%
          dplyr$filter(significant) %>%
          dplyr$mutate(dplyr$across(c("p_val", "p_adj"), ~ -log10(.))) %>%
          dplyr$mutate(dplyr$across(c("p_val", "p_adj"), ~ round(., 2))) %>% 
          dplyr$mutate(gene_names = stringr$str_extract(gene_names, "[^;_]*")) %>%
          dplyr$distinct(gene_names, .keep_all = TRUE) %>% 
          dplyr$mutate(
            category = dplyr$if_else(fold_change > 0, "up", "down"),
            size = p_val * 5,
            color = dplyr$case_when(
              fold_change > 0 & fold_change <= 1.5 ~ "#fddbc7",
              fold_change > 1.5 &
                fold_change <= 2 ~ "#f4a582",
              fold_change > 2 &
                fold_change <= 2.5 ~ "#d6604d",
              fold_change > 2.5 &
                fold_change <= 3 ~ "#b2182b",
              fold_change > 3 ~ "#67001f",
              fold_change < 0 &
                fold_change >= -1.5 ~ "#d1e5f0",
              fold_change < -1.5 &
                fold_change >= -2 ~ "#92c5de",
              fold_change < -2 &
                fold_change >= -2.5 ~ "#4393c3",
              fold_change < -2.5 &
                fold_change >= -3 ~ "#2166ac",
              fold_change < -3 ~ "#053061",
            )
          )
        
        if (direction == "up") {
          nodes_table <- nodes_table %>% 
            dplyr$filter(category == "up")
        } else if (direction == "down") {
          nodes_table <- nodes_table %>% 
            dplyr$filter(category == "down")
        }
        
      } else {
        
        nodes_table <- self$anova_table %>% 
          dplyr$filter(significant) %>%
          dplyr$mutate(dplyr$across(c("p_val", "p_adj"), ~ -log10(.))) %>%
          dplyr$mutate(dplyr$across(c("p_val", "p_adj"), ~ round(., 2))) %>% 
          dplyr$mutate(gene_names = stringr$str_extract(gene_names, "[^;_]*")) %>%
          dplyr$distinct(gene_names, .keep_all = TRUE) %>% 
          dplyr$filter(cluster %in% focus) %>% 
          dplyr$mutate(
            category = cluster,
            size = p_val * 2
          )
        
      }
      
      self$nodes_table <- nodes_table
      self$name_for_edges <- nodes_table %>%
        dplyr$pull(gene_names)
      
    },
    make_edges = function(source) {
      
      edges_string_table <- NULL
      edges_corum_table <- NULL
      
      if("string" %in% source) {
        
        edges_string_table <-
          self$name_for_edges %>%
          rba_string_interactions_network(species = 9606, verbose = FALSE) %>%
          dplyr$filter(escore != 0, dscore != 0) %>%
          tidyr$unite("stringId", stringId_A:stringId_B, remove = TRUE) %>%
          dplyr$distinct(stringId, .keep_all = TRUE) %>%
          ## string calculation for fisical score
          dplyr$mutate(score1 = (escore - 0.041) * (1 - 0.041)) %>%
          dplyr$mutate(score2 = (dscore - 0.041) * (1 - 0.041)) %>%
          dplyr$mutate(score_combin = 1 - (1 - score1) * (1 - score2)) %>%
          dplyr$mutate(score = score_combin + 0.041 * (1 - score_combin)) %>%
          ## end
          dplyr$select(source = preferredName_A, target = preferredName_B, score) %>%
          dplyr$filter(source != target) %>%
          dplyr$mutate(
            complex = "not defined",
            color = "#999999",
            size = round(score * 10 / 2, 0),
            database = "String"
          )
        
        if (nrow(edges_string_table) == 0) {
          edges_string_table <- NULL
        }
        
      }
      
      if("corum" %in% source) {
        
        raw_corum_table <-
          get_complex_genes(import_omnipath_complexes(resources = "CORUM"),
                            self$name_for_edges,
                            total_match = FALSE) %>%
          unique() %>%
          dplyr$select(name, components_genesymbols) %>%
          tidyr$separate_rows(components_genesymbols, sep = "_") %>%
          dplyr$filter(components_genesymbols %in% self$name_for_edges) %>%
          unique() %>%
          get_dupes(name)
        
        if (nrow(raw_corum_table) != 0) {
          
          expand_nodes <- raw_corum_table %>%
            dplyr$group_by(name) %>%
            dplyr$group_map( ~ dplyr$pull(.x, components_genesymbols))
          
          edges_corum_table <-
            map(.x = expand_nodes, .f = ~ as.data.frame(t(combn(.x, 2)))) %>%
            reduce(dplyr$bind_rows) %>%
            dplyr$rename(target = V1,  source = V2) %>%
            dplyr$left_join(raw_corum_table, by = c("source" = "components_genesymbols")) %>%
            dplyr$select(-dupe_count) %>%
            dplyr$rename(complex = name) %>%
            unique() %>% 
            dplyr$mutate(score = 1, color = "#4daf4a") %>% 
            dplyr$group_by(source, target, color) %>% 
            tidyr$nest() %>% 
            tidyr$unnest_wider(data, names_sep = "_") %>%
            dplyr$ungroup() %>% 
            dplyr$rowwise() %>% 
            dplyr$mutate(
              score = sum(data_score),
              complex = toString(data_complex),
              size = dplyr$if_else(score <= 5, score, 5),
              database = "Corum"
            ) %>% 
            dplyr$select(source, target, complex, score, color, size, database)
          
        } else {
          edges_corum_table <- NULL
        }
        
      }
      
      if (is.null(edges_string_table) & is.null(edges_corum_table)) {
        self$edges_table <- NULL
      } else {
        self$edges_table <- edges_string_table %>%
          dplyr$bind_rows(edges_corum_table)
      }
      
    },
    e_arrange_list = function(list, max_cols = 3) {
      
      plots <- list
      
      total <- length(plots)
      rows <- floor((total/max_cols)+1)
      
      if(total <= max_cols){
        cols <- total
      }else{
        cols <- max_cols
      }
      
      x <- 0
      tg <- htmltools$tagList()
      for (i in 1:rows) {
        r <- htmltools$div(style = "display: flex; justify-content: center; gap: 20px")
        
        for (j in 1:cols) {
          x <- x + 1
          st <- "width: 100%;"
          if (x <= length(plots)) {
            c <- htmltools$div(style = st, plots[[x]])
          } else {
            c <- htmltools$div(style = st)
          }
          r <- htmltools$tagAppendChild(r, c)
        }
        tg <- htmltools$tagAppendChild(tg, r)
      }
      
      tg
      
    },
    plot_protein_counts = function() {
      
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
        echarts4r$e_bar(counts, emphasis = list(focus = "series")) %>%
        echarts4r$e_x_axis(name = "Replicates") %>%
        echarts4r$e_y_axis(name = "Counts") %>%
        echarts4r$e_tooltip(trigger = "item") %>%
        echarts4r$e_grid(containLabel = TRUE) %>%
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
    plot_protein_coverage = function() {
      
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
        echarts4r$e_grid(containLabel = TRUE) %>%
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
    plot_distribution = function() {
      
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
        echarts4r$e_grid(containLabel = TRUE) %>%
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
    plot_cv = function() {
      
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
          name = "Density",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>% 
        echarts4r$e_grid(containLabel = TRUE) %>%
        echarts4r$e_color(self$color_palette) %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      return(p)
    },
    plot_missing_data = function() {
      
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
        echarts4r$e_grid(containLabel = TRUE) %>%
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
    plot_missval_distribution = function() {
      
      if(self$is_norm){
        data <- self$normalized_data
      }else{
        data <- self$filtered_data
      }
      
      color <- viridis(n = 2 , direction = -1, end = 0.90, begin = 0.10, option = self$palette)
      color <- stringr$str_replace(color, pattern = "FF", replacement = "B3")
      br <- pretty(10:40, n = 100)
      
      p <- data %>%
        dplyr$group_by(gene_names) %>%
        dplyr$summarise(mean = mean(intensity, na.rm = TRUE),
                         missval = any(is.na(intensity))) %>%
        dplyr$ungroup() %>%
        dplyr$mutate(missing_value = dplyr$if_else(missval, "Missing", "Valid")) %>%
        dplyr$mutate(missing_value = factor(missing_value, levels = c("Valid", "Missing"))) %>%
        dplyr$group_by(missing_value) %>%
        echarts4r$e_charts(renderer = "svg") %>%
        echarts4r$e_histogram(mean, breaks = br) %>%
        echarts4r$e_x_axis(min = 10, max = 40) %>% 
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
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            lineHeight = 60
          )
        ) %>%
        echarts4r$e_grid(containLabel = TRUE) %>%
        echarts4r$e_color(color) %>% 
        echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore")) %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      return(p)
    },
    plot_imputation = function(data, imp_visualization = FALSE) {
      
      br <- pretty(10:40, n = 100)
      
      alpha_cols <- self$color_palette
      alpha_cols <- stringr$str_replace(alpha_cols, pattern = "FF", replacement = "B3")
      
      p <- data %>%
        dplyr$group_by(condition) %>%
        echarts4r$e_charts(renderer = "svg") %>%
        echarts4r$e_histogram(intensity, breaks = br) %>%
        echarts4r$e_color(alpha_cols) %>%
        echarts4r$e_x_axis(min = 10, max = 40) %>% 
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
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            lineHeight = 60
          )
        ) %>%
        echarts4r$e_grid(containLabel = TRUE) %>%
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
        
      if(imp_visualization){
        imputed_dist <- self$imputed_data %>% 
          dplyr$filter(imputed) %>% 
          dplyr$group_by(condition)
        
        p <- data %>%
          dplyr$group_by(condition) %>%
          echarts4r$e_charts(renderer = "svg") %>%
          echarts4r$e_histogram(intensity, breaks = br) %>%
          echarts4r$e_color(c(alpha_cols, "#bc3754")) %>%
          echarts4r$e_x_axis(min = 10, max = 40) %>%
          echarts4r$e_data(imputed_dist, intensity) %>% 
          echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore")) %>% 
          echarts4r$e_histogram(intensity, name = "Imputed", breaks = br) %>%
          echarts4r$e_x_axis(min = 10, max = 40) %>%
          echarts4r$e_legend(selected = list('Imputed'= FALSE)) %>% 
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
            name = "log2 Intensity",
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 14,
              lineHeight = 60
            )
          ) %>%
          echarts4r$e_grid(containLabel = TRUE) %>%
          echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      }
      
      return(p)
    },
    plot_multiple_distribution_single = function(label_list) {
      br <- pretty(10:40, n = 100)
      color <- viridis(n = 2 , direction = -1, end = 0.90, begin = 0.10, option = self$palette)
      
      p <- self$imputed_data %>%
        dplyr$filter(label == label_list) %>%
        dplyr$mutate(missing_value = dplyr$if_else(imputed, "Imputed", "Valid")) %>%
        dplyr$mutate(missing_value = factor(missing_value, levels = c("Valid", "Imputed"))) %>% 
        dplyr$group_by(missing_value) %>% 
        echarts4r$e_charts(renderer = "svg") %>%
        echarts4r$e_histogram(intensity, breaks = br) %>%
        echarts4r$e_x_axis(min = 10, max = 40) %>%
        echarts4r$e_title(text = label_list) %>%
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
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%
        echarts4r$e_grid(containLabel = TRUE) %>%
        echarts4r$e_color(color) %>%
        echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore")) %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      return(p)
    },
    plot_multiple_distribution = function() {
      
      list <- self$expdesign %>% dplyr$pull(label)
      
      all_hist <- map(
        .x = list,
        .f = ~ self$plot_multiple_distribution_single(label_list = .x)
      )
      
      cols <- max(self$expdesign %>% dplyr$distinct(replicate) %>% nrow())
      
      p <- self$e_arrange_list(all_hist, max_cols = cols)
      
      return(p)
    },
    plot_pca = function(view_3d = FALSE) {
      
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
          echarts4r$e_grid(containLabel = TRUE) %>%
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
          echarts4r$e_grid(containLabel = TRUE) %>%
          echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore")) %>% 
          echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      }
      
      return(p)
    },
    plot_correlation_interactive = function(cor_method = "pearson") {
      
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
          precision = 2,
          inRange = list(color = color)
        ) %>%
        echarts4r$e_theme("QProMS_theme") %>% 
        echarts4r$e_grid(containLabel = TRUE) %>%
        echarts4r$e_toolbox_feature(feature = c("saveAsImage"))
        
      
      return(p)
    },
    plot_correlation_scatter = function(x, y, value) {
      
      if(self$is_imp){
        data <- self$imputed_data
      }else{
        data <- self$normalized_data
      }
      
      data_scatter <- data %>%
        dplyr$filter(label %in% c(x, y)) %>% 
        dplyr$select(gene_names, label, intensity) %>%
        tidyr$pivot_wider(names_from = "label", values_from = "intensity") %>% 
        dplyr$select(gene_names, x = !!x, y = !!y)
      
      min_plot <- round(min(data_scatter %>% dplyr$select(-gene_names), na.rm = TRUE) - 1, 0)
      max_plot <- round(max(data_scatter %>% dplyr$select(-gene_names), na.rm = TRUE) + 1, 0)
      
      if(is.null(value)){
        value <- round(cor(data_scatter$x, data_scatter$y), 2)
      }
      
      if(x == y){
        p <- data_scatter %>% 
          echarts4r$e_charts() %>%
          echarts4r$e_histogram(x) %>% 
          echarts4r$e_x_axis(min = min_plot, max = max_plot) %>%
          echarts4r$e_color(self$color_palette) %>%
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
            name = x,
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 14,
              lineHeight = 60
            )
          ) %>%
          echarts4r$e_grid(containLabel = TRUE) %>%
          echarts4r$e_toolbox_feature(feature = "saveAsImage")
      }else{
        p <- data_scatter %>%
          echarts4r$e_charts(x, dispose = FALSE) %>%
          echarts4r$e_scatter(y, legend = FALSE, symbol_size = 5, bind = gene_names) %>%
          echarts4r$e_x_axis(min = min_plot, max = max_plot) %>%
          echarts4r$e_y_axis(min = min_plot, max = max_plot) %>%
          echarts4r$e_color(self$color_palette) %>%
          echarts4r$e_toolbox_feature(feature = "dataZoom") %>% 
          echarts4r$e_tooltip(
            formatter = JS("
            function(params){
              return('<strong>' + params.name + '</strong>');
            }
          ")
          ) %>%
          echarts4r$e_y_axis(
            name = x,
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 16,
              lineHeight = 60
            )
          ) %>%
          echarts4r$e_x_axis(
            name = y,
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 14,
              lineHeight = 60
            )
          ) %>%
          echarts4r$e_grid(containLabel = TRUE) %>%
          echarts4r$e_title(paste0("correlation: ", value), left = "center") %>%  
          echarts4r$e_toolbox_feature(feature = "saveAsImage") 
      }
      
      return(p)
    },
    plot_multi_scatter = function(selected_cond) {
      
      if(selected_cond == "all"){
        list <- self$expdesign %>% dplyr$pull(label)
      }else{
        list <- self$expdesign %>% dplyr$filter(condition == selected_cond) %>%  dplyr$pull(label)
      }
      
      
      
      list2 <- t(combn(list,2))
      colnames(list2) <- c("x", "y")
      list2 <- as_tibble(list2)
      
      all_scatter <- map2(
        .x = list2$x,
        .y = list2$y,
        .f = ~ self$plot_correlation_scatter(x = .x, y = .y, value = NULL)
      )
      
      cols <- floor(length(all_scatter)/10)
      
      if(cols<3){
        cols <- 3
      }
      
      p <- self$e_arrange_list(all_scatter, max_cols = cols)
      
      return(p)
      
    },
    plot_correlation_static = function(cor_method = "pearson", single_condition = NULL) {
      
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
      
    },
    plot_volcano_single = function(test, highlights_names) {
      
      alpha_cols <- viridis(n = 2 , direction = 1, end = 0.90, begin = 0.10, option = self$palette)
      
      alpha_cols[2] <- stringr$str_replace(alpha_cols[2], pattern = "FF", replacement = "33")
      
      text_color <- alpha_cols[1]
      bg_color <- alpha_cols[2]
      
      table <- self$stat_table %>% 
        dplyr$select(gene_names, dplyr$starts_with(test)) %>% 
        dplyr$rename_at(dplyr$vars(dplyr$matches(test)), ~ stringr$str_remove(., paste0(test, "_")))
      
      min_thr <- table %>% 
        dplyr$filter(significant) %>% 
        dplyr$pull(p_val) %>% 
        max()
      
      left_line <-
        tibble(p_val = c(-log10(min_thr),-log10(min_thr), max(-log10(table$p_val))),
               fold_change = c(min(table$fold_change),-self$fold_change,-self$fold_change))
      
      right_line <-
        tibble(p_val = c(max(-log10(table$p_val)),-log10(min_thr),-log10(min_thr)),
               fold_change = c(self$fold_change, max(table$fold_change), self$fold_change))
      
      p <- table %>%
        dplyr$mutate(color = dplyr$case_when(fold_change > 0 & significant ~ "#67001f", 
                                               fold_change < 0 & significant ~ "#053061",
                                               TRUE ~ "#e9ecef")) %>%
        dplyr$group_by(color) %>%
        dplyr$mutate(fold_change = round(fold_change, 2)) %>%
        dplyr$mutate(p_val = -log10(p_val)) %>%
        dplyr$mutate(p_val = round(p_val, 3)) %>%
        echarts4r$e_chart(fold_change, renderer = "svg", dispose = TRUE, height = "650px") %>%
        echarts4r$e_scatter(p_val, legend = FALSE, bind = gene_names, symbol_size = 5) %>%
        echarts4r$e_tooltip(
          formatter = JS(
            "
      function(params){
        return('<strong>' + params.name +
                '</strong><br />FC: ' + params.value[0] +
                '<br />p.val: ' + params.value[1])
                }
    "
          )
        ) %>%
        echarts4r$e_add_nested("itemStyle", color) %>%
        echarts4r$e_data(left_line, fold_change) %>%
        echarts4r$e_line(
          p_val,
          legend = FALSE,
          color = "#000",
          symbol = "none",
          lineStyle = list(type = "dashed", width = .8)
        ) %>%
        echarts4r$e_data(right_line, fold_change) %>%
        echarts4r$e_line(
          p_val,
          legend = FALSE,
          color = "#000",
          symbol = "none",
          lineStyle = list(type = "dashed", width = .8)
        ) %>%
        echarts4r$e_toolbox() %>%
        echarts4r$e_toolbox_feature(feature = c("saveAsImage", "dataZoom")) %>%
        echarts4r$e_x_axis(
          name = "Fold_change",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 15,
            lineHeight = 50
          )
        ) %>%
        echarts4r$e_y_axis(
          name = "-log(Pvalue)",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 15,
            lineHeight = 50
          )
        ) %>% 
        echarts4r$e_grid(containLabel = TRUE) %>%
        echarts4r$e_title(text = test, left = "center") %>%
        echarts4r$e_group("grp") %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      if (!is.null(highlights_names)) {
        for (name in highlights_names) {
          highlights_name <- table %>%
            dplyr$filter(gene_names == name) %>%
            dplyr$mutate(p_val = -log10(p_val)) %>%
            dplyr$select(xAxis = fold_change,
                          yAxis = p_val,
                          value = gene_names) %>% as.list()
          
          p <- p %>%
            echarts4r$e_mark_point(
              data = highlights_name,
              symbol = "pin",
              symbolSize = 50,
              silent = TRUE,
              label = list(color = text_color, fontWeight = "normal", fontSize = 16),
              itemStyle = list(color = bg_color,  borderColor = bg_color, borderWidth = 0.2)
            )
        }
      }
      
      return(p)
    },
    plot_volcano = function(test, highlights_names = NULL) {
      
      volcanos <- map(
        .x = test,
        .f = ~ self$plot_volcano_single(
          test = .x,
          highlights_names = highlights_names
        )
      )
      p <- self$e_arrange_list(volcanos)
      
      return(p)
    },
    plot_heatmap = function(z_score, clustering_method, n_cluster, manual_order, order) {
      
      mat_base <- self$anova_table %>%
        dplyr$filter(significant) %>%
        dplyr$select(-c(p_val, p_adj, significant, cluster)) %>%
        column_to_rownames("gene_names") %>%
        as.matrix()
      
      if (z_score) {
        mat = t(apply(mat_base, 1, scale))
        colnames(mat) <- colnames(mat_base)
        mat_name <- "z-score"
      } else{
        mat <- mat_base
        mat_name <- "log2(Intensity)"
      }
      
      if (manual_order) {
        col_order <- self$expdesign %>%
          dplyr$arrange(factor(condition, levels = order)) %>%
          dplyr$select(label, condition) %>%
          deframe()
        
        self$anova_col_order <- names(col_order)
        
        col_method = "groups"
      } else {
        col_method = "hclust"
      }
      
      if (n_cluster == 0) {
        n_k <- NULL
      } else {
        n_k <- n_cluster
        alpha_cols <- viridis(n = n_cluster , direction = -1, end = 0.90, begin = 0.10, option = "H")
        alpha_cols <- stringr$str_replace(alpha_cols, pattern = "FF", replacement = "E6")
      }
      
      ht <- main_heatmap(
        mat,
        name = mat_name,
        colors = self$color_palette,
        colorbar_grid = setup_colorbar_grid(y_start = 0.65)) %>%
        add_col_labels() %>%
        add_row_clustering(
          k = n_k,
          method = clustering_method,
          colors = alpha_cols,
          name = "Clusters"
        ) %>%
        add_col_clustering(method = col_method,
                                      groups = col_order,
                                      show_colorbar = FALSE) 
      
      if (n_cluster != 0) {
        clusters <- ht@plots@listData$Clusters@data %>%
          enframe(name = "gene_names", value = "cluster") %>%
          dplyr$mutate(cluster = paste0("cluster_", cluster))
        
        self$anova_table <- self$anova_table %>%
          dplyr$select(-cluster) %>%
          dplyr$left_join(clusters, by = "gene_names") %>%
          dplyr$mutate(cluster = dplyr$if_else(is.na(cluster), "not_defined", cluster))
      }
      
      if (!manual_order) {
        self$anova_col_order <-
          tibble(
            label = ht@shapes@listData$col_dendro@data$labels,
            order = 1:nrow(self$expdesign)
          ) %>%
          dplyr$arrange(factor(order, levels = ht@shapes@listData$col_dendro@data$order)) %>%
          dplyr$pull(label)
      }
      
      return(ht)
      
    },
    plot_protein_profile = function(gene) {
      
      cluster_specific_col <- self$anova_table %>%
        dplyr$filter(gene_names %in% gene) %>%
        dplyr$distinct(cluster) %>%
        dplyr$pull(cluster) %>%
        stringr$str_extract(pattern = "[:digit:]") %>%
        as.numeric()
      
      alpha_cols <- viridis(n = self$clusters_number, direction = -1, end = 0.90, begin = 0.10, option = "H")
      alpha_cols <- stringr$str_replace(alpha_cols, pattern = "FF", replacement = "E6")
      sub_alpha_cols <- alpha_cols[cluster_specific_col] %>% tidyr$replace_na("#22262980")
      
      p <- self$anova_table %>%
        tidyr$pivot_longer(
          !c(gene_names, p_val, p_adj, significant, cluster),
          names_to = "label",
          values_to = "intensity"
        ) %>%
        dplyr$filter(gene_names %in% gene) %>%
        dplyr$mutate(intensity = round(intensity, 2)) %>%
        dplyr$group_by(cluster, gene_names) %>%
        dplyr$arrange(factor(label, levels = self$anova_col_order)) %>%
        echarts4r$e_chart(label, renderer = "svg", dispose = TRUE) %>%
        echarts4r$e_line(intensity, bind = gene_names) %>%
        echarts4r$e_color(sub_alpha_cols) %>%
        echarts4r$e_x_axis(name = "", axisLabel = list(interval = 0, rotate = 45)) %>%
        echarts4r$e_y_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>%
        echarts4r$e_grid(containLabel = TRUE) %>%
        echarts4r$e_tooltip() %>%
        echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore", "dataZoom")) %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      return(p)
      
    },
    plot_cluster_profile_single = function(clust, color_band) {
      
      title <- clust %>%
        stringr$str_to_title() %>%
        stringr$str_replace(pattern = "_", replacement = " ")
      
      p <- self$anova_table %>%
        tidyr$pivot_longer(
          !c(gene_names, p_val, p_adj, significant, cluster),
          names_to = "label",
          values_to = "intensity"
        ) %>%
        dplyr$filter(cluster == clust) %>%
        dplyr$group_by(label) %>%
        dplyr$summarise(
          n = dplyr$n(),
          sd = sd(intensity, na.rm = TRUE),
          median = median(intensity, na.rm = TRUE)
        ) %>%
        dplyr$ungroup() %>%
        dplyr$mutate(
          error = qt(0.975, df = n - 1) * sd / sqrt(n),
          lower_bound = median - error,
          upper_bound = median + error
        ) %>%
        dplyr$arrange(factor(label, levels = self$anova_col_order)) %>%
        echarts4r$e_charts(label, renderer = "svg") %>%
        echarts4r$e_line(median,
               symbol = "none",
               color = "#222629",
               name = "Median") %>%
        echarts4r$e_band2(
          lower_bound,
          upper_bound,
          itemStyle = list(borderWidth = 0),
          name = "95% CI"
        ) %>%
        echarts4r$e_color(color_band) %>%
        echarts4r$e_x_axis(name = "", axisLabel = list(interval = 0, rotate = 45)) %>%
        echarts4r$e_title(text = title) %>%
        echarts4r$e_grid(containLabel = TRUE) %>%
        echarts4r$e_y_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 16,
            lineHeight = 60
          )
        ) %>% 
        echarts4r$e_toolbox_feature(feature = c("saveAsImage", "restore", "dataZoom")) %>% 
        echarts4r$e_group("grp") %>% 
        echarts4r$e_connect_group("grp")
      
      return(p)
      
    },
    plot_cluster_profile = function() {
      
      alpha_cols <- viridis(n = self$clusters_number , direction = -1, end = 0.90, begin = 0.10, option = "H")
      alpha_cols <- stringr$str_replace(alpha_cols, pattern = "FF", replacement = "E6")
      
      n_clust <- tibble(cluster = "cluster_", numb = 1:self$clusters_number) %>% 
        dplyr$mutate(cluster = paste0(cluster, numb)) %>% 
        dplyr$pull(cluster)
      
      profile_plots <- map2(
        .x = n_clust,
        .y = alpha_cols,
        .f = ~ self$plot_cluster_profile_single(clust = .x, color_band = .y)
      )
      
      p <- self$e_arrange_list(profile_plots)
      
      return(p)
    },
    plot_ora_empty = function(val) {
      
      data <- data.frame(x = val, y = "No enrichment terms")
      
      p <- echarts4r$e_chart(data, x, renderer = "svg") %>%
        echarts4r$e_bar(y) %>%
        echarts4r$e_legend(show = FALSE) %>%
        echarts4r$e_draft(text = "No enrichment terms",
                          size = "50px",
                          color = "red") %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      return(p)
      
    },
    plot_ora = function(term, groups, selected, value) {
      
      if(length(groups) == 1 & !is.null(selected)) {
        
        colors <- viridis(n = 3, direction = -1, end = 0.90, begin = 0.10, option = self$palette)
        
        if (term == "BP") {
          color <- colors[1]
        } else if (term == "MF") {
          color <- colors[2]
        } else {
          color <- colors[3]
        }
        
        single_plot_table <- self$ora_table %>%
          dplyr$filter(group == groups)
        
        if(nrow(single_plot_table) == 0) {
          p <- self$plot_ora_empty(val = value)
        } else {
          p <- single_plot_table %>%
            dplyr$filter(ID %in% selected) %>%
            dplyr$rename(value := !!value) %>% 
            dplyr$arrange(value) %>%
            echarts4r$e_chart(ID, renderer = "svg") %>%
            echarts4r$e_bar(value, bind = Description) %>%
            echarts4r$e_flip_coords() %>%
            echarts4r$e_grid(containLabel = TRUE) %>%
            echarts4r$e_color(color) %>%
            echarts4r$e_tooltip(
              formatter = JS(
                "function(params){return('<strong>Size: </strong>' + params.value[0])}"
              )
            ) %>%
            echarts4r$e_x_axis(
              name = value,
              nameLocation = "center",
              nameTextStyle = list(
                fontWeight = "bold",
                fontSize = 16,
                lineHeight = 60
              )
            ) %>%
            echarts4r$e_y_axis(axisLabel = list(fontSize = 0)) %>%
            echarts4r$e_legend(show = FALSE) %>% 
            echarts4r$e_labels(show = TRUE, formatter= '{b}', position = "insideLeft") %>%
            echarts4r$e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
            echarts4r$e_group("ora") %>% 
            echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
        }
      } else if (length(groups) > 1 & !is.null(selected)) {
        color <- viridis(n = length(groups), direction = -1, end = 0.90, begin = 0.10, option = self$palette)
        alpha_cols <- stringr$str_replace(color, pattern = "FF", replacement = "CC")
        
        multiple_plot_table <- self$ora_table %>%
          dplyr$filter(group %in% groups)
        
        if(nrow(multiple_plot_table) == 0) {
          p <- self$plot_ora_empty(val = value)
        } else {
          p <- multiple_plot_table %>%
            dplyr$filter(ID %in% selected) %>%
            dplyr$rename(value := !!value) %>% 
            dplyr$mutate(numeric_id = stringr$str_remove(ID, "GO:")) %>%
            dplyr$mutate(numeric_id = as.numeric(numeric_id)) %>%
            dplyr$mutate(rank_id = round(rank(numeric_id), 0)) %>%
            dplyr$arrange(value) %>%
            dplyr$group_by(group) %>%
            echarts4r$e_charts(group, renderer = "svg") %>%
            echarts4r$e_scatter(rank_id, value, bind = Description, scale_js = "function(data){ return data[2];}") %>%
            echarts4r$e_tooltip(
              formatter = JS(
                "function(params){return('<strong>Size: </strong>' + params.value[2])}"
              )
            ) %>%
            echarts4r$e_color(alpha_cols) %>%
            echarts4r$e_y_axis(interval = 1, axisLabel = list(fontSize = 0)) %>%
            echarts4r$e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>% 
            echarts4r$e_grid(containLabel = TRUE) %>% 
            echarts4r$e_labels(show = TRUE, formatter= '{b}') %>%
            echarts4r$e_group("ora") %>% 
            echarts4r$e_toolbox_feature(feature = c("saveAsImage", "dataView", "dataZoom")) %>% 
            echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
        }
      } else {
        p <- self$plot_ora_empty(val = value)
      }
      
      
      
      return(p)
    },
    plot_ora_network = function(term, groups, selected, val, layout, show_names) {
      
      if (length(groups) == 1 & !is.null(selected)) {
        colors <-
          viridis(
            n = 3,
            direction = -1,
            end = 0.90,
            begin = 0.10,
            option = self$palette
          )
        
        if (term == "BP") {
          color <- colors[1]
        } else if (term == "MF") {
          color <- colors[2]
        } else {
          color <- colors[3]
        }
        
      } else {
        color <-
          viridis(
            n = length(groups),
            direction = -1,
            end = 0.90,
            begin = 0.10,
            option = self$palette
          )
      }
      
      net_table <- self$ora_table %>% 
        dplyr$filter(ID %in% selected) %>% 
        dplyr$rename(value := !!val) %>%
        dplyr$arrange(value) %>% 
        rowid_to_column()
      
      nodes <- net_table %>%
        tidyr$separate_rows(geneID, sep = "/") %>%
        dplyr$select(Description, geneID, group, value) %>%
        tidyr$pivot_longer(!c(group, value), names_to = "category", values_to = "id") %>%
        dplyr$distinct(id, .keep_all = TRUE) %>%
        dplyr$mutate(size = dplyr$if_else(category == "geneID", 10, 25)) %>%
        dplyr$mutate(symbol = dplyr$if_else(category == "geneID", "circle", "diamond")) %>%
        dplyr$select(-category) %>% 
        dplyr$left_join(net_table %>% dplyr$select(rowid, Description), by = c("id" = "Description")) %>% 
        dplyr$arrange(rowid) %>% 
        dplyr$select(-rowid)
      
      edges <- net_table %>%
        tidyr$separate_rows(geneID, sep = "/") %>%
        dplyr$select(source = Description, target = geneID) %>%
        dplyr$mutate(value = 1)
      
      p <- echarts4r$e_charts(renderer = "svg") %>%
        echarts4r$e_graph(
          roam = TRUE,
          layout = layout,
          zoom = 0.8,
          circular = list(rotateLabel = TRUE),
          force = list(
            initLayout = "circular",
            repulsion = 200,
            edgeLength = 50,
            layoutAnimation = FALSE
          ),
          emphasis = list(focus = "adjacency")
        ) %>%
        echarts4r$e_graph_nodes(
          nodes = nodes,
          names = id,
          value = value,
          size = size,
          category = group,
          symbol = symbol
        ) %>%
        echarts4r$e_graph_edges(
          edges = edges,
          source = source,
          target = target,
          value = value,
          size = value
        ) %>%
        echarts4r$e_color(color) %>%
        echarts4r$e_tooltip() %>% 
        echarts4r$e_toolbox_feature(feature = "saveAsImage") %>% 
        echarts4r$e_group("ora") %>% 
        echarts4r$e_connect_group("ora") %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      if (show_names) {
        p <- p %>% 
          echarts4r$e_labels(fontSize = 10) 
      }
      
      return(p)
    },
    plot_ppi_network = function(list_from, score_thr, isolate_nodes, layout, show_names, selected, filtred) {
      
      edges <- self$edges_table %>% 
        dplyr$filter(score >= score_thr)
      nodes <- self$nodes_table
      
      if(!isolate_nodes) {
        
        edge_source <- edges %>% dplyr$pull(source)
        edge_target <- edges %>% dplyr$pull(target)
        
        list <- c(edge_source, edge_target)
        final_list <- unique(list)
        
        nodes <- nodes %>%
          dplyr$filter(gene_names %in% final_list)
        
      }
      
      if (filtred) {
        nodes <- nodes %>%
          dplyr$filter(gene_names %in% selected)
      }
      
      p <- echarts4r$e_charts() %>%
        echarts4r$e_graph(
          roam = TRUE,
          layout = layout,
          zoom = 0.5,
          force = list(
            initLayout = "circular",
            repulsion = 800,
            edgeLength = 150,
            layoutAnimation = FALSE
          ),
          autoCurveness = TRUE,
          # selectedMode = "single",
          emphasis = list(focus = "adjacency")
        ) %>%
        echarts4r$e_graph_nodes(
          nodes = nodes,
          names = gene_names,
          value = p_val,
          size = size,
          category = category,
          legend = FALSE
        ) %>%
        echarts4r$e_graph_edges(
          edges = edges,
          source = source,
          target = target,
          value = score,
          size = size
        ) %>%
        echarts4r$e_tooltip() %>% 
        echarts4r$e_toolbox_feature(feature = "saveAsImage") %>% 
        echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
      
      if (show_names) {
        p <- p %>% 
          echarts4r$e_labels(fontSize = 10) 
      }
      
      for(i in 1:nrow(edges)) {
        p$x$opts$series[[1]]$links[[i]]$lineStyle$color <- edges[i, ]$color
      }
      
      if(list_from == "univariate") {
        for (i in 1:nrow(nodes)) {
          p$x$opts$series[[1]]$data[[i]]$itemStyle$color <- nodes[i, ]$color
        }
      }
      
      if (!is.null(selected) & !filtred) {
        for (i in 1:nrow(nodes)) {
          if (p$x$opts$series[[1]]$data[[i]]$name %in% selected) {
            p$x$opts$series[[1]]$data[[i]]$itemStyle$borderColor <- "#000"
            p$x$opts$series[[1]]$data[[i]]$itemStyle$borderWidth <- 2
            p$x$opts$series[[1]]$data[[i]]$itemStyle$color <- "yellow"
            p$x$opts$series[[1]]$data[[i]]$symbolSize <- 30
            # p$x$opts$series[[1]]$data[[i]]$itemStyle$shadowBlur <- 5
            # p$x$opts$series[[1]]$data[[i]]$itemStyle$shadowColor <- "#000"
          }
        }
      }
      
      return(p)
      
    },
    plot_gsea = function(term, groups, selected) {
      
      if(length(groups) > 0 & !is.null(selected)) {
        
        color <- viridis(n = length(groups), direction = -1, end = 0.90, begin = 0.10, option = self$palette)
        alpha_cols <- stringr$str_replace(color, pattern = "FF", replacement = "CC")
        
        list_nes <- self$gsea_table %>%
          dplyr$filter(ID %in% selected) %>%
          dplyr$group_by(group) %>%
          dplyr$group_map(~ dplyr$pull(.x, NES))
        
        p <- self$gsea_table %>% 
          dplyr$filter(ID %in% selected) %>%
          dplyr$mutate(numeric_id = stringr$str_remove(ID, "GO:")) %>%
          dplyr$mutate(numeric_id = as.numeric(numeric_id)) %>%
          dplyr$mutate(rank_id = round(rank(numeric_id), 0)) %>% 
          dplyr$arrange(pvalue) %>%
          dplyr$group_by(group) %>%
          echarts4r$e_charts(group, renderer = "svg") %>%
          echarts4r$e_scatter(rank_id, pvalue, bind = Description, scale_js = "function(data){ return data[2]*3;}") %>%
          echarts4r$e_tooltip(
            formatter = JS(
              "function(params){return('<strong>Size: </strong>' + params.value[2] + '<br /><strong>NES: </strong>' + params.value[3])}"
            )
          ) %>%
          echarts4r$e_y_axis(interval = 1, axisLabel = list(fontSize = 0)) %>%
          echarts4r$e_grid(containLabel = TRUE) %>%
          echarts4r$e_labels(show = TRUE, formatter = '{b}') %>% 
          echarts4r$e_color(alpha_cols) %>% 
          echarts4r$e_visual_map(NES, inRange = list(color = c("#2166ac", "#f7f7f7", "#b2182b")), precision = 2, bottom = "45%") %>% 
          echarts4r$e_toolbox_feature(feature = c("saveAsImage", "dataView", "dataZoom")) %>% 
          echarts4r$e_show_loading(text = "Loading...", color = "#35608D")
        
        for (n in 1:length(list_nes)) {
          val <- list_nes[[n]]
          for (i in 1:length(list_nes[[n]])) {
            p$x$opts$series[[n]]$data[[i]]$value[4] = val[i]
          }
        }
        
      } else {
        p <- self$plot_ora_empty(val = "pvalue")
      }
      
      return(p)
      
    }
  )
)