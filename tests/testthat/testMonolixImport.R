library(testthat)
library(campsis)

context("Test the Monolix import on a few models")

testFolder <- ""
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
  
  if (overwriteNonRegressionFiles) {
    model %>% write(nonRegressionFolderPath(folder))
  }

  return(model)
}

test_that("Test model 1 can be imported successfully", {
  folder <- "test_model1"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
})

getRemifentanilDataset <- function() {
  dataset <- Dataset() %>%
    add(Infusion(time=0, amount=1439.8, rate=71.99)) %>%
    add(Covariate("AGE", 30.58)) %>%
    add(Covariate("SEX", 1)) %>%
    add(Covariate("LBM", 56.5075))
  return(dataset)
}

validateRemifentanilPK <- function(folder, model) {
  predictions <- read.csv(file.path(testFolder, "monolix_models", folder, "predictions.txt"))
  dataset <- getRemifentanilDataset() %>%
    add(Observations(times=predictions$time))
  results <- simulate(model=model %>% disable("IIV"), dataset=dataset, dest="rxode2")
  expect_equal(predictions$popPred, results$DV, tolerance=1e-4)
}

test_that("PK_01 can be imported successfully", {

  folder <- "PK_01"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_02 can be imported successfully", {

  folder <- "PK_02"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_03 can be imported successfully", {

  folder <- "PK_03"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_04 can be imported successfully", {

  folder <- "PK_04"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_05 can be imported successfully", {

  folder <- "PK_05"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_06 can be imported successfully", {

  folder <- "PK_06"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_07 can be imported successfully", {

  folder <- "PK_07"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_08 can be imported successfully", {

  folder <- "PK_08"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_09 can be imported successfully", {

  folder <- "PK_09"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_10 can be imported successfully", {

  folder <- "PK_10"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_11 can be imported successfully", {

  folder <- "PK_11"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_12 can be imported successfully", {

  folder <- "PK_12"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_13 can be imported successfully", {

  folder <- "PK_13"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_14 can be imported successfully", {

  folder <- "PK_14"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_15 can be imported successfully", {

  folder <- "PK_15"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})

test_that("PK_16 can be imported successfully", {
  
  folder <- "PK_16"
  
  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))
  
  expect_equal(model, nonreg_model)
  validateRemifentanilPK(folder=folder, model=model)
})
