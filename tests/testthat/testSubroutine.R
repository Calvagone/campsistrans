library(testthat)
library(ggplot2)

context("Non-regression test on subroutine conversion")

testFolder <<- ""

advanFilename <- function(advan, trans, ext="txt") {
  return(paste0("advan", advan, "_trans", trans, ".", ext))
}

nonRegressionFilePath <- function(advan, trans) {
  return(paste0(testFolder, "non_regression/subroutine/", advanFilename(advan, trans, ext="txt")))
}

loadAdvanNonRegressionFile <- function(advan, trans) {
  return(pmxmod::read.model(nonRegressionFilePath(advan, trans)))
}

generateModel <- function(advan, trans) {
  pmxtran <- importNONMEM(getNONMEMModelTemplate(advan, trans))
  pmxmod <- toPmxModel(pmxtran)
  pmxmod@model %>% pmxmod::write(nonRegressionFilePath(advan, trans)) # TO DISABLE LATER ON
  return(pmxmod)
}

test_that("ADVAN1 TRANS1", {
  advan <- 1
  trans <- 1
  pmxmod <- generateModel(advan, trans)
  expect_equal(pmxmod@model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN1 TRANS2", {
  advan <- 1
  trans <- 2
  pmxmod <- generateModel(advan, trans)
  expect_equal(pmxmod@model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN2 TRANS1", {
  advan <- 2
  trans <- 1
  pmxmod <- generateModel(advan, trans)
  expect_equal(pmxmod@model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN2 TRANS2", {
  advan <- 2
  trans <- 2
  pmxmod <- generateModel(advan, trans)
  expect_equal(pmxmod@model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN3 TRANS1", {
  advan <- 3
  trans <- 1
  pmxmod <- generateModel(advan, trans)
  expect_equal(pmxmod@model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN3 TRANS3", {
  advan <- 3
  trans <- 3
  pmxmod <- generateModel(advan, trans)
  expect_equal(pmxmod@model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN3 TRANS4", {
  advan <- 3
  trans <- 4
  pmxmod <- generateModel(advan, trans)
  expect_equal(pmxmod@model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN3 TRANS4 with mapping", {
  mapping <- mapping(theta=c("CL"=1, "V1"=2, "V2"=3, "Q"=4),
                     omega=c("CL"=1, "V1"=2, "V2"=3, "Q"=4),
                     sigma=c("PROP"=1))
  pmxtran <- importNONMEM(getNONMEMModelTemplate(3, 4), mapping=mapping)
  pmxmod <- pmxtran %>% toPmxModel()
  expect_equal((pmxmod@parameters %>% getByIndex(Sigma(index=1, index2=1)))@name, "PROP")
})


test_that("ADVAN3 TRANS5", {
  advan <- 3
  trans <- 5
  pmxmod <- generateModel(advan, trans)
  expect_equal(pmxmod@model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN4 TRANS1", {
  advan <- 4
  trans <- 1
  pmxmod <- generateModel(advan, trans)
  expect_equal(pmxmod@model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN4 TRANS3", {
  advan <- 4
  trans <- 3
  pmxmod <- generateModel(advan, trans)
  expect_equal(pmxmod@model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN4 TRANS4", {
  advan <- 4
  trans <- 4
  pmxmod <- generateModel(advan, trans)
  expect_equal(pmxmod@model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN4 TRANS5", {
  advan <- 4
  trans <- 5
  pmxmod <- generateModel(advan, trans)
  expect_equal(pmxmod@model, loadAdvanNonRegressionFile(advan, trans))
})
