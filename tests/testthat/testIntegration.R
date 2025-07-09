library(testthat)
library(campsismod)
library(campsistrans)
library(campsisqual)

context("Test the NONMEM import feature and its integration with the campsisqual package")

testFolder <-  file.path(getwd(), test_path())
qualFolder <- file.path(testFolder, "integration_tests", "nonmem_qualification")
reportFolder <- file.path(testFolder, "integration_tests", "qualification_reports")
reexecuteNONMEM <- FALSE

test_that("Filgrastim PK/PD model (Krzyzanski et al.) can be imported and simulated well", {

  modelPath <- function(folder, filename) {
    return(normalizePath(file.path(testFolder, "ddmore_models", folder, filename)))
  }
  
  filename <- "Executable_simulated_GCSF_dataset.ctl"
  modelFolder <- "filgrastim"

  # Nonmem2rx import (note: Pharmpy can also be used)
  campsistrans <- importNONMEM2(modelPath(modelFolder, filename))
  
  # Campsis export
  model <- campsistrans %>% export(dest="campsis")
  
  # Remove IPRED/IRES statements
  model <- model %>%
    delete(IfStatement("CMT == 2", Equation("IPRED"))) %>%
    delete(IfStatement("CMT == 2", Equation("IRES"))) %>%
    delete(IfStatement("CMT == 2", Equation("Y"))) %>%
    delete(IfStatement("CMT == 4", Equation("IPRED"))) %>%
    delete(IfStatement("CMT == 4", Equation("IRES"))) %>%
    delete(IfStatement("CMT == 4", Equation("Y")))
  
  dest <- "rxode2"
  covariates <- c("ROUT", "BAS")
  settings <- Settings(Declare(covariates))
  
  getDataset <- function(excludeCMT=4) {
    dataset <- importDataset(file=modelPath(modelFolder, filename), covariates=c("ROUT", "BAS"), etas_zero=TRUE,
                             campsis=campsistrans@campsis, campsis_id=TRUE)
    dataset <- dataset %>% dplyr::filter(CMT!=excludeCMT)
    return(dataset)
  }
  
  datasetZCP <- getDataset(excludeCMT=4) # PK
  ipredZCP <- executeSimulationCtl(file=modelPath(modelFolder, filename), updateInits=FALSE, model=model, dataset=datasetZCP,
                                   variables="ZCP", folder=file.path(qualFolder, paste0(modelFolder, "_zcp")),
                                   reexecuteNONMEM=reexecuteNONMEM)
  
  # 3 PD values at TIME 0, only keep 1...
  datasetZNB <- getDataset(excludeCMT=2) %>% dplyr::distinct(ID, TIME, EVID, .keep_all=TRUE) # PD
  ipredZNB <- executeSimulationCtl(file=modelPath(modelFolder, filename), updateInits=FALSE, model=model, dataset=datasetZNB,
                                   variables="ZNB", folder=file.path(qualFolder, paste0(modelFolder, "_znb")),
                                   reexecuteNONMEM=reexecuteNONMEM)

  # Qualify ZCP
  qual <- qualify(model=model %>% disable("RUV"), ipred=ipredZCP, dest=dest, dataset=datasetZCP, variables="ZCP", settings=settings)
  expect_true(qual %>% passed())

  # Qualify ZNB
  qual <- qualify(model=model %>% disable("RUV"), ipred=ipredZNB, dest=dest, dataset=datasetZNB, variables="ZNB", settings=settings)
  expect_true(qual %>% passed())
})

test_that("Custom model '2cpt_zo_foce_full' can be imported and simulated well", {
  
  modelPath <- function(folder, filename) {
    return(normalizePath(file.path(testFolder, "custom_models", folder, filename)))
  }
  
  filename <- "model.ctl"
  modelFolder <- "2cpt_zo_foce_full"
  
  ctlFile <- modelPath(modelFolder, "model.ctl")
  extFile <- modelPath(modelFolder, "model.ext")
  covFile <- modelPath(modelFolder, "model.cov")
  
  # Nonmem2rx import (note: Pharmpy can also be used)
  campsistrans <- importNONMEM2(ctlFile=ctlFile, extFile=extFile, covFile=covFile)
  
  # Campsis export
  model <- campsistrans %>%
    export(dest="campsis")

  dest <- "rxode2"
  covariates <- c("WT", "METAB")
  settings <- Settings(Declare(covariates))
  dataset <- importDataset(file=ctlFile, covariates=covariates, etas=TRUE, campsis=campsistrans@campsis, campsis_id=TRUE) %>%
    dplyr::mutate(AMT=ifelse(AMT==".", "0", AMT) %>% as.numeric())
  ipred <- importPredictions(file=ctlFile, output="IPRED")
  
  # Qualify
  qual <- qualify(model=model, ipred=ipred, dest=dest, dataset=dataset, variables="IPRED", settings=settings)
  expect_true(qual %>% passed())
})
