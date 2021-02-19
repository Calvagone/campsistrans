library(testthat)

context("Test nlmixr conversion")

testFolder <<- ""

test_that("nlmixr conversion", {
  pharmpy <- reticulate::import("pharmpy")
  advan <- 4
  trans <- 4
  nmmodel <- pharmpy$Model(paste0(testFolder, "models/subroutine/advan", advan, "_trans", trans, ".mod"))
  nlmixrmodel <- pharmpy$plugins$nlmixr$convert_model(nmmodel)
  
  # See here that pharmy is able to export to nlmixr
  print(nlmixrmodel)
  
  # nlmixr equations (basic test)
  statements <- nlmixrmodel$statements
  expect_equal(length(statements), 11)
})

