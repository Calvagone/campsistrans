library(testthat)
library(campsismod)

context("Test parameters extraction")

testFolder <<- ""

advanFilename <- function(advan, trans, ext="txt") {
  return(paste0("advan", advan, "_trans", trans, ".", ext))
}

modelPath <- function(advan, trans) {
  return(paste0(testFolder, "models/subroutine/", advanFilename(advan, trans, ext="mod")))
}

test_that("Test method retrieveInitialValues", {
  pharmpy <- reticulate::import("pharmpy")
  model <- pharmpy$Model$create_model(getNONMEMModelTemplate(4, 4))
  parset <- model$parameters
  params <- retrieveInitialValues(parset)
  expect_equal(as.character(class(params)), "parameters")
  expect_equal(length(params@list), 11)
})

test_that("Test method select and maxIndex", {
  pharmpy <- reticulate::import("pharmpy")
  model <- pharmpy$Model$create_model(getNONMEMModelTemplate(4, 4))
  parset <- model$parameters
  params <- retrieveInitialValues(parset)
  omegas <- params %>% campsismod::select("omega")
  expect_equal(length(omegas@list), 5)
  
  maxIndex <- omegas %>% maxIndex()
  expect_equal(maxIndex, 5)
})

test_that("Test method getByIndex", {
  pharmpy <- reticulate::import("pharmpy")
  model <- pharmpy$Model$create_model(getNONMEMModelTemplate(1, 2))
  parset <- model$parameters
  params <- retrieveInitialValues(parset)
  
  theta <- params %>% getByIndex(Theta(index=2))
  expect_equal(length(theta), 1)
  expect_equal(theta %>% getNONMEMName(), "THETA(2)")
  
  omega <- params %>% getByIndex(Omega(index=2, index2=2))
  expect_equal(length(omega), 1)
})


