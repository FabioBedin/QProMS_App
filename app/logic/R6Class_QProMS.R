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
    }
  )
)