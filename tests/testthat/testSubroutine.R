library(testthat)
library(ggplot2)

context("Non-regression test on subroutine conversion")

testFolder <<- ""

source(paste0(testFolder, "testUtils.R"))

advanFilename <- function(advan, trans, ext="txt") {
  return(paste0("advan", advan, "_trans", trans, ".", ext))
}

modelPath <- function(advan, trans) {
  return(paste0(testFolder, "models/subroutine/", advanFilename(advan, trans, ext="mod")))
}

nonRegressionFilePath <- function(advan, trans) {
  return(paste0(testFolder, "models/subroutine/non_regression/", advanFilename(advan, trans, ext="txt")))
}

loadAdvanNonRegressionFile <- function(advan, trans) {
  return(loadNonRegressionFile(nonRegressionFilePath(advan, trans)))
}

generateModel <- function(path) {
  model <- importNONMEM(path)
  code <- toRxODE(model)
  toFile(code, nonRegressionFilePath(advan, trans)) # TO DISABLE
  return(code)
}

test_that("ADVAN1 TRANS1", {
  advan <- 1
  trans <- 1
  code <- generateModel(modelPath(advan, trans))
  expect_equal(code, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN1 TRANS2", {
  advan <- 1
  trans <- 2
  code <- generateModel(modelPath(advan, trans))
  expect_equal(code, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN2 TRANS1", {
  advan <- 2
  trans <- 1
  code <- generateModel(modelPath(advan, trans))
  expect_equal(code, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN2 TRANS2", {
  advan <- 2
  trans <- 2
  code <- generateModel(modelPath(advan, trans))
  expect_equal(code, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN3 TRANS1", {
  advan <- 3
  trans <- 1
  code <- generateModel(modelPath(advan, trans))
  expect_equal(code, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN3 TRANS3", {
  advan <- 3
  trans <- 3
  code <- generateModel(modelPath(advan, trans))
  expect_equal(code, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN3 TRANS4", {
  advan <- 3
  trans <- 4
  code <- generateModel(modelPath(advan, trans))
  expect_equal(code, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN3 TRANS5", {
  advan <- 3
  trans <- 5
  code <- generateModel(modelPath(advan, trans))
  expect_equal(code, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN4 TRANS1", {
  # TODO: F=A_CENTRAL is wrong
  advan <- 4
  trans <- 1
  code <- generateModel(modelPath(advan, trans))
  expect_equal(code, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN4 TRANS3", {
  # TODO: F=A_CENTRAL is wrong
  advan <- 4
  trans <- 3
  code <- generateModel(modelPath(advan, trans))
  expect_equal(code, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN4 TRANS4", {
  # TODO: F=A_CENTRAL is wrong
  advan <- 4
  trans <- 4
  code <- generateModel(modelPath(advan, trans))
  expect_equal(code, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN4 TRANS5", {
  # TODO: F=A_CENTRAL is wrong
  advan <- 4
  trans <- 5
  code <- generateModel(modelPath(advan, trans))
  expect_equal(code, loadAdvanNonRegressionFile(advan, trans))
})