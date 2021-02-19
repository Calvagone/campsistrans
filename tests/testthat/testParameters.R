library(testthat)

context("Test parameters extraction")

testFolder <<- ""

advanFilename <- function(advan, trans, ext="txt") {
  return(paste0("advan", advan, "_trans", trans, ".", ext))
}

modelPath <- function(advan, trans) {
  return(paste0(testFolder, "models/subroutine/", advanFilename(advan, trans, ext="mod")))
}

test_that("Test method initialValues", {
  pharmpy <- reticulate::import("pharmpy")
  model <- pharmpy$Model(modelPath(4,4))
  parset <- model$parameters
  params <- initialValues(parset)
  expect_equal(as.character(class(params)), "parameters")
  expect_equal(length(params@list), 11)
})

test_that("Test method filter and maxIndex", {
  pharmpy <- reticulate::import("pharmpy")
  model <- pharmpy$Model(modelPath(4,4))
  parset <- model$parameters
  params <- initialValues(parset)
  omegas <- params %>% filter(type="omega")
  expect_equal(length(omegas@list), 5)
  
  maxIndex <- params %>% maxIndex(type="omega")
  expect_equal(maxIndex, 5)
})

test_that("Test method getParameter", {
  pharmpy <- reticulate::import("pharmpy")
  model <- pharmpy$Model(modelPath(1,2))
  parset <- model$parameters
  params <- initialValues(parset)
  
  theta <- params %>% getParameter(type="theta", index=as.integer(2))
  expect_equal(length(theta), 1)
  expect_equal(theta %>% getNONMEMName(), "THETA(2)")
  
  omega <- params %>% getParameter(type="theta", index=as.integer(2), index2=as.integer(2))
  expect_equal(length(omega), 1)
})


