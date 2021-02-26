library(testthat)
library(ggplot2)

context("Tests on custom model 1")

testFolder <<- ""

modelPath <- function(number) {
  return(paste0(testFolder, "custom_models/model", number, ".mod"))
}

nonRegressionFilePath <- function(number) {
  return(paste0(testFolder, "non_regression/custom/model", number, ".txt"))
}

generateModel <- function(number, mapping) {
  # Import your NONMEM model using pharmpy
  pmxtran <- importNONMEM(modelPath(number), mapping)
  
  pmxmod <- toPmxModel(pmxtran)
  pmxmod@model %>% pmxmod::write(nonRegressionFilePath(number)) # TO DISABLE LATER ON
  return(pmxmod)
}

test_that("Model 1", {
  number <- 1
  
  # Also map OMEGA names because 'SAME' OMEGAS related to IOV not listed in Pharmpy parameter set
  mapping <- mapping(theta=c("CL"=1, "V2"=2, "Q3"=3, "V3"=4, "KA"=5, "F1"=6, "W"=7, "NFOOD"=8, "LAMB"=9, "IMAX"=10, "CLBW"=11, "VDBW"=12, "CL99"=13),
                     omega={'names<-' (1:24, 1:24)},
                     sigma=c("ADD"=1))
  
  pmxmod <- generateModel(number=number, mapping=mapping)
  expect_equal(pmxmod@model, pmxmod::read.model(nonRegressionFilePath(number)))
})
