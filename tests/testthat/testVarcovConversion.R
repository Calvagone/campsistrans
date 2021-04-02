library(testthat)
library(ggplot2)

context("Working with variance-covariance matrices")

testFolder <<- "C:/prj/pmxtran/tests/testthat/"

test_that("Loading a simple model with variance-covariance matrix", {
  file <- paste0(testFolder, "parameter_uncertainty/", "example1/", "advan3_trans4.ctl")
  pmxtran <- importNONMEM(file, estimate=TRUE, uncertainty=TRUE)
  model <- pmxtran %>% export(dest="pmxmod")
  varcov <- pmxtran@varcov
  parameters <- pmxtran@params
})