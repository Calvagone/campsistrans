library(testthat)
library(campsismod)

context("Test that NONMEM files can be prepared for the qualification")

testFolder <-  file.path(getwd(), test_path())
overwriteNonRegressionFiles <- FALSE

modelPath <- function(folder, filename) {
  return(file.path(testFolder, "ddmore_models", folder, filename))
}

test_that("Import custom model", {
  
  test_folder <- file.path("C:/Users/nicolas.luyckx.CALVAGONE/Desktop/Pharmpy/")
  path <-  file.path(test_folder, "runPKPMPD007" , "runPKPMPD007_QUAL.mod")
  dataset <- read.csv(file.path(test_folder, "dataset.csv"), header=TRUE, stringsAsFactors=FALSE)
  campsis <- read.campsis(file.path(test_folder, "Model"))
  
  model <- loadCtl(path=path, estimate=TRUE)
  model_ <- updateCtlForQual(model=model, dataset=dataset, campsis=campsis, variables="CONC")
  
  output_folder <- file.path(test_folder, "Export")
  
  writeCtl(model=model_, path=file.path(output_folder, "export.mod"), force=TRUE)
})

