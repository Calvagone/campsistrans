library(testthat)
library(campsismod)

context("Test that NONMEM files can be prepared for the qualification")

testFolder <-  file.path(getwd(), test_path())
overwriteNonRegressionFiles <- FALSE

modelPath <- function(folder, filename) {
  return(file.path(testFolder, "ddmore_models", folder, filename))
}

