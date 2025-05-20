library(testthat)
library(campsis)

context("Test the Monolix import on a few models")

testFolder <-  file.path(getwd(), test_path())
overwriteNonRegressionFiles <- FALSE

nonRegressionFolderPath <- function(folder) {
  return(file.path(testFolder, "non_regression", "monolix", folder))
}

generateModel <- function(folder, modelFun=function(x) {x}) {
  # Adding full path
  mlxtranFile <- file.path(testFolder, "monolix_models", folder, "project.mlxtran")
  modelFile <- file.path(testFolder, "monolix_models", folder, "model.txt")
  parametersFile <- file.path(testFolder, "monolix_models", folder, "populationParameters.txt")
  
  model <- importMonolix(mlxtranFile=mlxtranFile, modelFile=modelFile, parametersFile=parametersFile) %>%
    modelFun()
  
  
  if (overwriteNonRegressionFiles) {
    model %>% write(nonRegressionFolderPath(folder))
  }

  return(model)
}

getRemifentanilDataset <- function() {
  dataset <- Dataset() %>%
    add(Infusion(time=0, amount=1439.8, rate=71.99)) %>%
    add(Covariate("AGE", 30.58)) %>%
    add(Covariate("SEX", 1)) %>%
    add(Covariate("LBM", 56.5075))
  return(dataset)
}

fixRxODEBug <- function(campsis, model, dataset, dest) {
  if (dest %in% c("RxODE", "rxode2")) {
    if (is(dataset, "dataset")) {
      times <- dataset %>% getTimes()
      
      # If LAG is found in model & time 0 does not exists in observations
      # We remove time 0 from the output
      properties <- model@compartments@properties
      if (!is.null(properties %>% find(LagTime(1))) && !(0 %in% times)) {
        campsis <- campsis %>% dplyr::filter(TIME!=0)
      }
    }
  }
  return(campsis)
}

validateModelImplementation <- function(folder, model, dataset, output, tolerance=1e-4) {
  predictions <- read.csv(file.path(testFolder, "monolix_models", folder, "predictions.txt"))
  dataset <- dataset %>%
    add(Observations(times=predictions$time))
  results <- simulate(model=model %>% disable("IIV"), dataset=dataset, dest="rxode2", outvars=output)
  results <- fixRxODEBug(campsis=results, model=model, dataset=dataset, dest="rxode2")
  expect_equal(predictions$popPred, results[[output]], tolerance=tolerance)
}

test_that("PK_01 can be imported successfully", {

  folder <- "PK_01"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_02 can be imported successfully", {

  folder <- "PK_02"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_03 can be imported successfully", {

  folder <- "PK_03"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_04 can be imported successfully", {

  folder <- "PK_04"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_05 can be imported successfully", {

  folder <- "PK_05"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_06 can be imported successfully", {

  folder <- "PK_06"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_07 can be imported successfully", {

  folder <- "PK_07"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_08 can be imported successfully", {

  folder <- "PK_08"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_09 can be imported successfully", {

  folder <- "PK_09"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_10 can be imported successfully", {

  folder <- "PK_10"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_11 can be imported successfully", {

  folder <- "PK_11"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_12 can be imported successfully", {

  folder <- "PK_12"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_13 can be imported successfully", {

  folder <- "PK_13"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_14 can be imported successfully", {

  folder <- "PK_14"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_15 can be imported successfully", {

  folder <- "PK_15"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("PK_16 can be imported successfully", {

  folder <- "PK_16"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getRemifentanilDataset(), output="DV")
})

test_that("Test model 1 can be imported successfully", {
  folder <- "test_model1"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
})

getWarfarinDataset <- function() {
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=100)) %>%
    add(Covariate("WT", 66.7)) %>%
    add(Covariate("SEX", 1)) %>%
    add(Covariate("AGE", 50))
  return(dataset)
}

test_that("Warfarin PK can be imported successfully", {
  folder <- "warfarin_PK"

  model <- generateModel(folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))

  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getWarfarinDataset(), output="Cc")
})

test_that("Warfarin PKPD IRM can be imported successfully", {
  folder <- "warfarin_PKPD_IRM"
  
  # Slight issue, k is not in the right place.
  # It should be before the PK ODEs. Issue in monolix2rx -> won't solve
  modelFun <- function(model) {
    model <- model %>%
      campsismod::move(x=Equation("k"), to=campsismod::Position(OdeRecord(), after=FALSE))
    return(model)
  }
  
  model <- generateModel(folder=folder, modelFun)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))
  
  expect_equal(model, nonreg_model)
  validateModelImplementation(folder=folder, model=model, dataset=getWarfarinDataset(), output="A_R", tolerance=0.005)
})
