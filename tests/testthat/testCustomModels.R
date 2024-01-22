library(testthat)
library(campsismod)

context("Tests on custom model 1")

testFolder <- ""
overwriteNonRegressionFiles <- FALSE

modelPath <- function(modelDir, modelName) {
  return(paste0(testFolder, "custom_models/", modelDir, "/", modelName))
}

nonRegressionFolderPath <- function(regFolder) {
  return(paste0(testFolder, "non_regression/custom/", regFolder, "/"))
}

generateModel <- function(modelDir, modelName, regFolder, mapping) {
  # Import your NONMEM model using pharmpy
  object <- importNONMEM(modelPath(modelDir, modelName), mapping)
  
  model <- object %>% export(dest="campsis")
  if (overwriteNonRegressionFiles) {
    model %>% write(nonRegressionFolderPath(regFolder))
  }
  return(model)
}

test_that("Model 1", {
  modelDir <- "model1"
  modelName <- "model1.mod"
  regFolder <- modelDir
  

  # Also map OMEGA names because 'SAME' OMEGAS related to IOV not listed in Pharmpy parameter set
  mapping <- mapping(theta=c("CL"=1, "V2"=2, "Q3"=3, "V3"=4, "KA"=5, "F1"=6, "W"=7, "NFOOD"=8, "LAMB"=9, "IMAX"=10, "CLBW"=11, "VDBW"=12, "CL99"=13),
                     omega=1:24,
                     sigma=c("ADD"=1))
  
  model <- generateModel(modelDir=modelDir, modelName=modelName, regFolder=modelDir, mapping=mapping)
  expect_equal(model, campsismod::read.campsis(nonRegressionFolderPath(modelDir)))
})

test_that("Model 1 (auto-mapping)", {
  modelDir <- "model1"
  modelName <- "model1.mod"
  regFolder <- "model1_auto_mapping"
  
  # Also map OMEGA names because 'SAME' OMEGAS related to IOV not listed in Pharmpy parameter set
  mapping <- mapping(omega=1:24, sigma=c("ADD"=1), auto=TRUE)
  
  model <- generateModel(modelDir=modelDir, modelName=modelName, regFolder=regFolder, mapping=mapping)
  expect_equal(model, campsismod::read.campsis(nonRegressionFolderPath(regFolder)))
})

test_that("Model 2 (duplicate variables in model)", {
  modelDir <- "model2"
  modelName <- "model2.mod"
  regFolder <- "model2_auto_mapping"
  
  # Also map OMEGA names because 'SAME' OMEGAS related to IOV not listed in Pharmpy parameter set
  mapping <- mapping(auto=TRUE)
  
  model <- generateModel(modelDir=modelDir, modelName=modelName, regFolder=regFolder, mapping=mapping)

  model <- model %>%
    substituteDuplicates()
})





