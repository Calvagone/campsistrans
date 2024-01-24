library(testthat)
library(campsismod)
library(ggplot2)

context("Working with variance-covariance matrices")

testFolder <- ""
overwriteNonRegressionFiles <- FALSE

test_that("Import NONMEM model with cov file and export it to CAMPSIS", {
  mapping <- mapping(theta=c(CL=1, V1=2, V2=3, Q=4), omega=c(CL=1, V1=2), sigma=c(PROP=1))
  file <- paste0(testFolder, "parameter_uncertainty/", "example1/", "advan3_trans4.ctl")
  
  # Import with estimates and full uncertainty
  object <- importNONMEM(file, mapping=mapping, estimate=TRUE, uncertainty=TRUE)
  model <- object %>% export(dest="campsis")
  
  # Save CAMPSIS model for non-regression
  file <- paste0(testFolder, "non_regression/", "parameter_uncertainty/", "example1/")
  if (overwriteNonRegressionFiles) {
    model %>% write(file)
  }
  
  # Read non-regression model
  model2 <- read.campsis(file)
  
  expect_equal(model, model2)
})
