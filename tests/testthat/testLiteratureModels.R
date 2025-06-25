library(testthat)
library(campsismod)

context("Test NONMEM import on a few models coming from the literature")

testFolder <-  file.path(getwd(), test_path())
overwriteNonRegressionFiles <- FALSE

modelPath <- function(folder, filename) {
  return(file.path(testFolder, "literature_models", folder, filename))
}

nonRegressionFolderPath <- function(folder) {
  return(file.path(testFolder, "non_regression", "literature", folder))
}

generateModel <- function(filename, folder, mapping=NULL, modelfun=NULL) {
  object <- importNONMEM(modelPath(folder, filename), mapping=mapping, estimate=FALSE)
  
  model <- object %>% export(dest="campsis")
  if (!is.null(modelfun)) {
    model <- modelfun(model)
  }
  
  if (overwriteNonRegressionFiles) {
    model %>% write(nonRegressionFolderPath(folder))
  }
  return(model)
}

test_that("EPS markov model can be imported well", {

  filename="eps_markov_pilla_et_al.ctl"
  folder <- "eps_markov"
  mapping <- mapping(auto=TRUE, theta=c("K12"=1, "K21_K32"=2, "K21_K23_GT16"=3, "K23"=4, "PCB_GT16"=5, "PCB_LT16"=6, "PCB_BACK"=7,
                                        "EFF_HALO"=8, "EFF_PALI"=9, "EFF_ZIPRA"=10, "EFF_JJ681"=11, "COUNTRY_EFF"=12))
  
  model <- generateModel(filename=filename, folder=folder, mapping=mapping)
  nonreg_model <- read.campsis(nonRegressionFolderPath(folder))

  expect_equal(model, nonreg_model)
})