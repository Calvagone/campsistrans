library(testthat)
library(campsismod)
library(ggplot2)

context("Testing the auto mapping functionnality")
testFolder <-  file.path(getwd(), test_path())

advanFilename <- function(advan, trans, ext=".txt") {
  return(paste0("advan", advan, "_trans", trans, ext))
}

nonRegressionPharmpyPath <- function(advan, trans) {
  return(file.path(testFolder, "non_regression", "subroutine", "pharmpy", advanFilename(advan, trans, ext="")))
}

generateModel <- function(advan, trans, mapping=NULL) {
  object <- importNONMEM(getNONMEMModelTemplate(advan, trans), mapping=mapping)
  model <- object %>%
    export(dest="campsis") %>%
    delete(Equation("Y"))
  return(model)
}

test_that("ADVAN1 TRANS1 (auto-mapping)", {
  advan <- 1
  trans <- 1
  mapping <- mapping(sigma=c(PROP=1), auto=TRUE)
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, read.campsis(nonRegressionPharmpyPath(advan, trans)))
})

test_that("ADVAN4 TRANS3 (auto-mapping)", {
  advan <- 4
  trans <- 3
  mapping <- mapping(auto=TRUE)
  model <- generateModel(advan, trans, mapping)
  
  expected_model <- read.campsis(nonRegressionPharmpyPath(advan, trans))
  expected_model@parameters@list[[11]]@name <- "RSV"
  expected_model <- expected_model %>% replaceAll("EPS_PROP", "EPS_RSV")
  
  expect_equal(model, expected_model)
})
