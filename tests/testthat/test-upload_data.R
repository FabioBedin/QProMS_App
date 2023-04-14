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

object$raw_data <- pg_for_test

object$make_expdesign(intensity_type = "lfq_intensity_")

object$pg_preprocessing()

test_that("make_expdesign() works", {
  expect_equal(nrow(object$expdesign), 8)
  expect_equal(colnames(object$expdesign), c("key", "label", "condition", "replicate"))
  expect_equal(object$expdesign %>% distinct(condition) %>% pull(), c("a", "b"))
  expect_equal(object$expdesign %>% distinct(replicate) %>% pull(), c("1", "2", "3", "4"))
  expect_equal(object$expdesign %>% distinct(label) %>% nrow(), 8)
})

test_that("define_colors() works", {
  expect_equal(length(object$color_palette), object$expdesign %>% distinct(condition) %>% nrow())
})

test_that("pg_preprocessing() works", {
  expect_equal(object$data %>% filter(gene_names == "prt9") %>% nrow(), 8)
  expect_equal(object$data %>% colnames() %>% length(), 12)
  expect_equal(sum(object$data$bin_intensity), 36)
  expect_equal(object$data %>% filter(gene_names == "gene8") %>% nrow(), 0)
})