library(testthat)
library(ggplot2)

context("Working with parameter uncertainty")

testFolder <<- ""

test_that("ADVAN1 TRANS1", {
  file <- paste0(testFolder, "parameter_uncertainty/", "example1/", "advan3_trans4.ctl")
  pmxtran <- importNONMEM(file, estimate=TRUE, uncertainty=TRUE)
  model <- pmxtran %>% export(dest="pmxmod")
})