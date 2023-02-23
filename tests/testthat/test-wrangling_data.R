box::use(
  testthat[...],
  dplyr[distinct, pull, `%>%`, filter]
)

box::use(
  app/logic/R6Class_QProMS,
)

pg_for_test <- tibble::tibble(
  gene_names = c("gene1", "gene2;gene2.5", "gene3", "gene4", "gene5", "gene6", "gene8", "gene8", ""),
  protein_i_ds = c("prt1", "prt2", "prt3", "prt4", "prt5", "prt6", "prt7;prt7.5", "prt8", "prt9"),
  id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  peptides = c(0,1,2,3,2,3,1,0,9),
  razor_unique_peptides = c(0,1,2,3,2,1,4,0,9),
  unique_peptides = c(0,1,0,3,1,1,4,0,9),
  reverse = c("", "", "+", "", "", "", "", "", ""),
  potential_contaminant = c("", "", "", "", "", "", "", "+", ""),
  only_identified_by_site = c("", "", "", "", "", "", "", "+", ""),
  lfq_intensity_a_1 = c(2, 2, 2, 2, 2, 2, 2, 2, 0),
  lfq_intensity_a_2 = c(2, 2, 2, 2, 2, 2, 2, 0, 0),
  lfq_intensity_a_3 = c(2, 2, 2, 2, 2, 2, 0, 0, 0),
  lfq_intensity_a_4 = c(2, 2, 2, 2, 2, 0, 0, 0, 0),
  lfq_intensity_b_1 = c(2, 2, 2, 2, 0, 0, 0, 0, 0),
  lfq_intensity_b_2 = c(2, 2, 2, 0, 0, 0, 0, 0, 0),
  lfq_intensity_b_3 = c(2, 2, 0, 0, 0, 0, 0, 0, 0),
  lfq_intensity_b_4 = c(2, 0, 0, 0, 0, 0, 0, 0, 0)
)

object <- R6Class_QProMS$QProMS$new()

object$data <- pg_for_test

object$make_expdesign(start_with = "lfq_intensity_")

test_that("data_wrangling() works with default", {
  
  object$data_wrangling(
    valid_val_filter = object$valid_val_filter,
    valid_val_thr = object$valid_val_thr,
    pep_filter = object$pep_filter,
    pep_thr = object$pep_thr,
    rev = object$rev,
    cont = object$cont,
    oibs = object$oibs
  )
  
  expect_equal(object$filtered_data %>% distinct(gene_names) %>% pull(), c("gene4", "gene5", "gene6"))
})

test_that("data_wrangling() valid val filter works",{
  
  object$data_wrangling(
    valid_val_filter = "alog",
    valid_val_thr = 0.5,
    pep_filter = "peptides",
    pep_thr = 0,
    rev = FALSE,
    cont = FALSE,
    oibs = FALSE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 56)
  
  object$data_wrangling(
    valid_val_filter = "each_grp",
    valid_val_thr = 0.5,
    pep_filter = "peptides",
    pep_thr = 0,
    rev = FALSE,
    cont = FALSE,
    oibs = FALSE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 24)
  
  object$data_wrangling(
    valid_val_filter = "total",
    valid_val_thr = 0.5,
    pep_filter = "peptides",
    pep_thr = 0,
    rev = FALSE,
    cont = FALSE,
    oibs = FALSE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 40)
  
  object$data_wrangling(
    valid_val_filter = "total",
    valid_val_thr = 0,
    pep_filter = "peptides",
    pep_thr = 0,
    rev = FALSE,
    cont = FALSE,
    oibs = FALSE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 72)
  
  object$data_wrangling(
    valid_val_filter = "each_grp",
    valid_val_thr = 1,
    pep_filter = "peptides",
    pep_thr = 0,
    rev = FALSE,
    cont = FALSE,
    oibs = FALSE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 8)
})


test_that("data_wrangling() pep filter works",{
  
  object$data_wrangling(
    valid_val_filter = "alog",
    valid_val_thr = 0.5,
    pep_filter = "peptides",
    pep_thr = 2,
    rev = FALSE,
    cont = FALSE,
    oibs = FALSE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 32)
  
  object$data_wrangling(
    valid_val_filter = "alog",
    valid_val_thr = 0.5,
    pep_filter = "unique",
    pep_thr = 2,
    rev = FALSE,
    cont = FALSE,
    oibs = FALSE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 16)
  
  object$data_wrangling(
    valid_val_filter = "alog",
    valid_val_thr = 0.5,
    pep_filter = "razor",
    pep_thr = 2,
    rev = FALSE,
    cont = FALSE,
    oibs = FALSE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 32)
})


test_that("data_wrangling() rev / cont / oibs filter works",{
  
  object$data_wrangling(
    valid_val_filter = "total",
    valid_val_thr = 0,
    pep_filter = "peptides",
    pep_thr = 0,
    rev = TRUE,
    cont = FALSE,
    oibs = FALSE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 64)
  
  object$data_wrangling(
    valid_val_filter = "total",
    valid_val_thr = 0,
    pep_filter = "peptides",
    pep_thr = 0,
    rev = FALSE,
    cont = TRUE,
    oibs = FALSE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 64)
  
  object$data_wrangling(
    valid_val_filter = "total",
    valid_val_thr = 0,
    pep_filter = "peptides",
    pep_thr = 0,
    rev = FALSE,
    cont = FALSE,
    oibs = TRUE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 64)
  
  object$data_wrangling(
    valid_val_filter = "total",
    valid_val_thr = 0,
    pep_filter = "peptides",
    pep_thr = 0,
    rev = TRUE,
    cont = FALSE,
    oibs = TRUE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 56)
  
  object$data_wrangling(
    valid_val_filter = "total",
    valid_val_thr = 0,
    pep_filter = "peptides",
    pep_thr = 0,
    rev = TRUE,
    cont = TRUE,
    oibs = TRUE
  )
  
  expect_equal(object$filtered_data %>% nrow(), 56)
  
})

