library(testthat)
library(campsismod)

context("Test the Monolix import on a few models")

testFolder <- "C:/prj/campsistrans/tests/testthat/"
overwriteNonRegressionFiles <- FALSE

nonRegressionFolderPath <- function(folder) {
  return(paste0(testFolder, "non_regression/monolix/", folder, "/"))
}

generateModel <- function(folder) {
  
  # Adding full path
  mlxtranFile <- file.path(testFolder, "monolix_models", folder, "project.mlxtran")
  modelFile <- file.path(testFolder, "monolix_models", folder, "model.txt")
  parametersFile <- file.path(testFolder, "monolix_models", folder, "populationParameters.txt")
  
  model <- importMonolix(mlxtranFile=mlxtranFile, modelFile=modelFile, parametersFile=parametersFile)
  
  # if (overwriteNonRegressionFiles) {
  #   model %>% write(nonRegressionFolderPath(folder))
  # }

  return(model)
}

test_that("Test model 1 can be imported successfully", {
  folder <- "test_model1"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
})

test_that("PK_01 can be imported successfully", {

  folder <- "PK_01"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
})



