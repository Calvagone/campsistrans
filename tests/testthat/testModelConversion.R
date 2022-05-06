library(testthat)

context("Testing a couple of utilities from sript model_conversion.R")

test_that("Methods removeUselessEquations works as expected", {

  model <- CampsisModel()
  
  model <- model %>%
    add(Equation("TVKA", "THETA_KA")) %>%
    add(Equation("ETA_KA", "ETA_KA"))

  model <- model %>% removeUselessEquations()
  
  expect_true(model %>% campsismod::contains(Equation("TVKA")))
  expect_false(model %>% campsismod::contains(Equation("ETA_KA")))
})
