library(testthat)
library(pmxmod)
library(ggplot2)

context("Non-regression test on subroutine conversion")

testFolder <<- ""

advanFilename <- function(advan, trans, ext=".txt") {
  return(paste0("advan", advan, "_trans", trans, ext))
}

nonRegressionFolderPath <- function(advan, trans) {
  return(paste0(testFolder, "non_regression/subroutine/", advanFilename(advan, trans, ext=""), "/"))
}

loadAdvanNonRegressionFile <- function(advan, trans) {
  return(read.pmxmod(nonRegressionFolderPath(advan, trans)))
}

generateModel <- function(advan, trans, mapping=NULL) {
  pmxtran <- importNONMEM(getNONMEMModelTemplate(advan, trans), mapping=mapping)
  model <- pmxtran %>% export(dest="pmxmod")
  #model %>% write(file=nonRegressionFolderPath(advan, trans)) # TO DISABLE LATER ON
  return(model)
}

test_that("ADVAN1 TRANS1", {
  advan <- 1
  trans <- 1
  mapping <- mapping(theta=c(K=1, V=2), omega=c(K=1, V=2), sigma=c(PROP=1))
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN1 TRANS2", {
  advan <- 1
  trans <- 2
  mapping <- mapping(theta=c(CL=1, V=2), omega=c(CL=1, V=2), sigma=c(PROP=1))
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN2 TRANS1", {
  advan <- 2
  trans <- 1
  mapping <- mapping(theta=c(KA=1, K=2, V=3), omega=c(KA=1, K=2, V=3), sigma=c(PROP=1))
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN2 TRANS2", {
  advan <- 2
  trans <- 2
  mapping <- mapping(theta=c(KA=1, CL=2, V=3), omega=c(KA=1, CL=2, V=3), sigma=c(PROP=1))
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN3 TRANS1", {
  advan <- 3
  trans <- 1
  mapping <- mapping(theta=c(K=1, V=2, K12=3, K21=4), omega=c(K=1, V=2, K12=3, K21=4), sigma=c(PROP=1))
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN3 TRANS3", {
  advan <- 3
  trans <- 3
  mapping <- mapping(theta=c(CL=1, V=2, Q=3, VSS=4), omega=c(CL=1, V=2, Q=3, VSS=4), sigma=c(PROP=1))
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN3 TRANS4", {
  advan <- 3
  trans <- 4
  mapping <- mapping(theta=c(CL=1, V1=2, V2=3, Q=4), omega=c(CL=1, V1=2, V2=3, Q=4), sigma=c(PROP=1))
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN3 TRANS5", {
  advan <- 3
  trans <- 5
  mapping <- mapping(theta=c(AOB=1, ALPHA=2, BETA=3), omega=c(AOB=1, ALPHA=2, BETA=3), sigma=c(PROP=1))
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN4 TRANS1", {
  advan <- 4
  trans <- 1
  mapping <- mapping(theta=c(KA=1, K=2, V=3, K23=4, K32=5), omega=c(KA=1, K=2, V=3, K23=4, K32=5), sigma=c(PROP=1))
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN4 TRANS3", {
  advan <- 4
  trans <- 3
  mapping <- mapping(theta=c(KA=1, CL=2, V=3, Q=4, VSS=5), omega=c(KA=1, CL=2, V=3, Q=4, VSS=5), sigma=c(PROP=1))
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN4 TRANS4", {
  advan <- 4
  trans <- 4
  mapping <- mapping(theta=c(KA=1, CL=2, V2=3, V3=4, Q=5), omega=c(KA=1, CL=2, V2=3, V3=4, Q=5), sigma=c(PROP=1))
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})

test_that("ADVAN4 TRANS5", {
  advan <- 4
  trans <- 5
  mapping <- mapping(theta=c(AOB=1, ALPHA=2, BETA=3, KA=4), omega=c(AOB=1, ALPHA=2, BETA=3, KA=4), sigma=c(PROP=1))
  model <- generateModel(advan, trans, mapping)
  expect_equal(model, loadAdvanNonRegressionFile(advan, trans))
})
