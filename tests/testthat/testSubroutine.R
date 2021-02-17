library(testthat)
library(ggplot2)

context("Non-regression test on subroutine conversion")
testFolder <<- ""

modelPath <- function(advan, trans) {
  return(paste0(testFolder, "models/subroutine/advan", advan, "_trans", trans, ".mod"))
}

toTmpFile <- function(code, advan, trans) {
  fileConn <- file(paste0(testFolder, "models/subroutine/non_regression/advan", advan, "_trans", trans, "_tmp.txt"))
  writeLines(code, fileConn)
  close(fileConn)
}

loadModel <- function(advan, trans) {
  read.table(file=paste0(testFolder, "models/subroutine/non_regression/advan", advan, "_trans", trans, ".txt"), sep="€")[,1]
}

generateModel <- function(advan, trans) {
  model <- importNONMEM(modelPath(advan=advan, trans=trans))
  code <- toRxODE(model)
}

test_that("ADVAN3 TRANS4", {
  advan <- 3
  trans <- 4
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