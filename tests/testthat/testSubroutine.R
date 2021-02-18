library(testthat)
library(ggplot2)

context("Non-regression test on subroutine conversion")
testFolder <<- ""
testFolder <<- "C:/prj/pmxtran/tests/testthat/"

modelPath <- function(advan, trans) {
  return(paste0(testFolder, "models/subroutine/advan", advan, "_trans", trans, ".mod"))
}

toTmpFile <- function(code, advan, trans) {
  fileConn <- file(paste0(testFolder, "models/subroutine/non_regression/advan", advan, "_trans", trans, ".txt"))
  writeLines(code, fileConn)
  close(fileConn)
}

loadModel <- function(advan, trans) {
  read.table(file=paste0(testFolder, "models/subroutine/non_regression/advan", advan, "_trans", trans, ".txt"), sep="€")[,1]
}

generateModel <- function(advan, trans) {
  model <- importNONMEM(modelPath(advan=advan, trans=trans))
  code <- toRxODE(model)
  toTmpFile(code, advan, trans)
  return(code)
}

test_that("ADVAN1 TRANS1", {
  advan <- 1
  trans <- 1
  code <- generateModel(advan, trans)
  expect_equal(code, loadModel(advan, trans))
})

test_that("ADVAN1 TRANS2", {
  advan <- 1
  trans <- 2
  code <- generateModel(advan, trans)
  expect_equal(code, loadModel(advan, trans))
})

test_that("ADVAN2 TRANS1", {
  advan <- 2
  trans <- 1
  code <- generateModel(advan, trans)
  expect_equal(code, loadModel(advan, trans))
})

test_that("ADVAN2 TRANS2", {
  advan <- 2
  trans <- 2
  code <- generateModel(advan, trans)
  expect_equal(code, loadModel(advan, trans))
})

test_that("ADVAN3 TRANS1", {
  advan <- 3
  trans <- 1
  code <- generateModel(advan, trans)
  expect_equal(code, loadModel(advan, trans))
})

test_that("ADVAN3 TRANS3", {
  advan <- 3
  trans <- 3
  code <- generateModel(advan, trans)
  expect_equal(code, loadModel(advan, trans))
})

test_that("ADVAN3 TRANS4", {
  advan <- 3
  trans <- 4
  code <- generateModel(advan, trans)
  expect_equal(code, loadModel(advan, trans))
})

test_that("ADVAN3 TRANS5", {
  advan <- 3
  trans <- 5
  code <- generateModel(advan, trans)
  expect_equal(code, loadModel(advan, trans))
})

test_that("ADVAN4 TRANS1", {
  # TODO: F=A_CENTRAL is wrong
  advan <- 4
  trans <- 1
  code <- generateModel(advan, trans)
  expect_equal(code, loadModel(advan, trans))
})

test_that("ADVAN4 TRANS3", {
  # TODO: F=A_CENTRAL is wrong
  advan <- 4
  trans <- 3
  code <- generateModel(advan, trans)
  expect_equal(code, loadModel(advan, trans))
})

test_that("ADVAN4 TRANS4", {
  # TODO: F=A_CENTRAL is wrong
  advan <- 4
  trans <- 4
  code <- generateModel(advan, trans)
  expect_equal(code, loadModel(advan, trans))
})

test_that("ADVAN4 TRANS5", {
  # TODO: F=A_CENTRAL is wrong
  advan <- 4
  trans <- 5
  code <- generateModel(advan, trans)
  expect_equal(code, loadModel(advan, trans))
})