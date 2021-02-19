library(testthat)
library(ggplot2)

context("Tests on custom model 1")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

modelPath <- function(number) {
  return(paste0(testFolder, "models/custom/model", number, ".mod"))
}

nonRegressionFilePath <- function(number) {
  return(paste0(testFolder, "models/custom/non_regression/model", number, ".txt"))
}

generateModel <- function(number, mapping) {
  # Import your NONMEM model using pharmpy
  pmxtran <- importNONMEM(modelPath(number), mapping)
  
  pmxmod <- toPmxModel(pmxtran)
  toFile(pmxmod@code, nonRegressionFilePath(number)) # TO DISABLE
  return(pmxmod)
}

test_that("Model 1", {
  number <- 1
  
  # Also map OMEGA names because 'SAME' OMEGAS related to IOV not listed in Pharmpy parameter set
  mapping <- mapping(theta=c("CL"=1, "V2"=2, "Q3"=3, "V3"=4, "KA"=5, "F1"=6, "W"=7, "NFOOD"=8, "LAMB"=9, "IMAX"=10, "CLBW"=11, "VDBW"=12, "CL99"=13),
                     omega={'names<-' (1:24, 1:24)},
                     sigma=c("ADD"=1))
  
  pmxmod <- generateModel(number=number, mapping=mapping)
  expect_equal(pmxmod@code, loadNonRegressionFile(nonRegressionFilePath(number)))
})
