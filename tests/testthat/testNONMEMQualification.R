library(testthat)
library(ggplot2)

context("Non-regression test on subroutine conversion")

testFolder <<- ""
#testFolder <<- "C:/prj/pmxtran/tests/testthat/"

advanFilename <- function(advan, trans, ext="txt") {
  return(paste0("advan", advan, "_trans", trans, ".", ext))
}

nonRegressionFilePath <- function(advan, trans) {
  return(paste0(testFolder, "non_regression/qualification/", advanFilename(advan, trans, ext="mod")))
}

loadAdvanNonRegressionFile <- function(advan, trans) {
  return(pmxmod::read.model(nonRegressionFilePath(advan, trans)))
}

test_that("ADVAN1 TRANS1", {
  advan <- 1
  trans <- 1

  #debugonce(pmxtran::qualify)
  pmxtran <- importNONMEM(getNONMEMModelTemplate(advan, trans))
  pmxtran <- pmxtran %>% qualify()
  file <- pmxtran %>% write(nonRegressionFilePath(advan, trans)) %>% as.character()
  
  # A file has been well produced
  expect_true(file.exists(file))
})
