library(testthat)
library(campsismod)

context("Test the Monolix import on a few models")

testFolder <- "C:/prj/campsistrans/tests/testthat/"
overwriteNonRegressionFiles <- FALSE

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
  
  # file <- modelPath(folder, filename)
  # mlxtran <- monolix2rx::mlxtran(file=file)
  # mlxtran$MODEL$LONGITUDINAL$LONGITUDINAL$file <- basename(file)
  # rxModel <- monolix2rx::monolix2rx(mlxtran)

  model <- generateModel(filename=filename, folder=folder)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))
  
  expect_equal(model, nonreg_model)
})

test_that("PK_01 can be imported successfully", {
  
  filename="PK_01.mlxtran"
  folder <- "PK_01"
  
  mlxtranFile <- modelPath(folder, filename)
  mlxtranFileStr <- readLines(mlxtranFile)
  
  longitudinalIndex <- which(grepl("\\s*\\[LONGITUDINAL\\]\\s*", mlxtranFileStr))
  filePathIndexes <- which(grepl("\\s*file\\s*=\\s*.*", mlxtranFileStr))
  
  if (length(longitudinalIndex) > 0) {
    filePathIndexes <- filePathIndexes[filePathIndexes > longitudinalIndex]
    modelFilePathIndex <- filePathIndexes[1]
    mlxtranFileStr[modelFilePathIndex] <- "file = 'infusion_2cpt_ClV1QV2.txt'"
  }

  writeLines(mlxtranFileStr, mlxtranFile)
  mlxtranFile <- modelPath(folder, filename)
  
  mlxtran <- monolix2rx::mlxtran(file=mlxtranFile)
  
  modelFile <- modelPath(folder, "infusion_2cpt_ClV1QV2.txt")
  
})



