library(testthat)
library(campsismod)
library(ggplot2)

context("Testing the auto mapping functionnality")

testFolder <<- ""

advanFilename <- function(advan, trans, ext=".txt") {
  return(paste0("advan", advan, "_trans", trans, ext))
}

nonRegressionFolderPath <- function(advan, trans) {
  return(paste0(testFolder, "non_regression/subroutine/", advanFilename(advan, trans, ext=""), "/"))
}

loadAdvanNonRegressionFile <- function(advan, trans) {
  return(read.campsis(nonRegressionFolderPath(advan, trans)))
}

generateModel <- function(advan, trans, mapping=NULL) {
  object <- importNONMEM(getNONMEMModelTemplate(advan, trans), mapping=mapping)
  model <- object %>% export(dest="campsis")
  return(model)
}

test_that("ADVAN1 TRANS1 (auto-mapping)", {
  advan <- 1
  trans <- 1
  mapping <- mapping(sigma=c(PROP=1), auto=TRUE)
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})
