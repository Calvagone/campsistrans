library(testthat)
library(campsismod)

context("Test the Monolix import on a few models")

testFolder <- "C:/prj/campsistrans/tests/testthat/"
overwriteNonRegressionFiles <- TRUE

modelPath <- function(folder, filename) {
  return(paste0(testFolder, "monolix_models/", folder, "/", filename))
}

nonRegressionFolderPath <- function(folder) {
  return(paste0(testFolder, "non_regression/monolix/", folder, "/"))
}

generateModel <- function(filename, folder) {
  model <- importMonolix(modelPath(folder, filename))
  
  if (overwriteNonRegressionFiles) {
    model %>% write(nonRegressionFolderPath(folder))
  }

  return(model)
}

test_that("Test model 1 can be imported successfully", {
  filename="test_model1.mlxtran"
  folder <- "test_model1"

  model <- generateModel(filename=filename, folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))
})