library(testthat)
library(campsismod)
library(ggplot2)

context("Working with variance-covariance matrices")

testFolder <-  file.path(getwd(), test_path())
overwriteNonRegressionFiles <- FALSE

test_that("Import NONMEM model with variance-covariance matrix and export it to Campsis", {
  
  modelPath <- file.path(testFolder, "parameter_uncertainty", "example1")
  ctlFile <- file.path(modelPath, "advan3_trans4.ctl")
  extFile <- file.path(modelPath, "advan3_trans4.ext")
  covFile <- file.path(modelPath, "advan3_trans4.cov")
  
  # Import with estimates and full uncertainty
  object <- importNONMEM2(ctlFile=ctlFile, 
                          extFile=extFile, 
                          covFile=covFile)
  
  model <- object %>%
    export(dest="campsis")
  
  # Save CAMPSIS model for non-regression
  file <- file.path(testFolder, "non_regression", "parameter_uncertainty", "example1")
  if (overwriteNonRegressionFiles) {
    model %>% write(file)
  }
  
  # Read non-regression model
  model2 <- read.campsis(file)
  
  expect_equal(model, model2)
})
