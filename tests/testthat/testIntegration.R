library(testthat)
library(campsismod)
library(campsistrans)
library(campsisqual)

context("Test NONMEM import on a few DDMoRE models")

testFolder <-  file.path(getwd(), test_path())
qualFolder <- file.path(testFolder, "integration_tests", "nonmem_qualification")
reportFolder <- file.path(testFolder, "integration_tests", "qualification_reports")
reexecuteNONMEM <- FALSE

modelPath <- function(folder, filename) {
  return(normalizePath(file.path(testFolder, "ddmore_models", folder, filename)))
}

test_that("Filgrastim PK/PD model (Krzyzanski et al.) can be simulated well", {

  filename <- "Executable_simulated_GCSF_dataset.ctl"
  modelFolder <- "filgrastim"

  # Pharmpy import
  campsistrans <- importNONMEM(modelPath(modelFolder, filename), mapping(auto=TRUE), estimate=FALSE)
  
  # Campsis export
  model <- campsistrans %>% export(dest="campsis")
  
  # Remove IPRED/IRES statements
  model <- model %>% delete(IfStatement("CMT == 2", Equation("IPRED")))
  model <- model %>% delete(IfStatement("CMT == 2", Equation("IRES")))
  model <- model %>% delete(IfStatement("CMT == 2", Equation("Y")))
  model <- model %>% delete(IfStatement("CMT == 4", Equation("IPRED")))
  model <- model %>% delete(IfStatement("CMT == 4", Equation("IRES")))
  model <- model %>% delete(IfStatement("CMT == 4", Equation("Y")))
  
  dest <- "rxode2"
  covariates <- c("ROUT", "BAS")
  settings <- Settings(Declare(covariates))
  
  getDataset <- function(excludeCMT=4) {
    dataset <- importDataset(campsistrans, covariates=c("ROUT", "BAS"), etas_zero=TRUE, campsis_id=TRUE)
    dataset <- dataset %>% dplyr::filter(CMT!=excludeCMT)
    return(dataset)
  }
  
  datasetZCP <- getDataset(excludeCMT=4) # PK
  # 3 PD values at TIME 0, only keep 1...
  datasetZNB <- getDataset(excludeCMT=2) %>% dplyr::distinct(ID, TIME, EVID, .keep_all=TRUE) # PD
  
  # Qualify ZCP
  qual <- campsistrans %>%
    qualify(model=model %>% disable("RUV"), dest=dest, dataset=datasetZCP, variables="ZCP",
            outputFolder=file.path(qualFolder, paste0(modelFolder, "_zcp")), reexecuteNONMEM=reexecuteNONMEM, settings=settings)
  expect_true(qual %>% passed())
  # if (exportReport) {
  #   qual %>% write(file=paste0(reportFolder, "qualification_", modelFolder, "_zcp_", dest, ".pdf"))
  # }
  
  # Qualify ZNB
  qual <- campsistrans %>%
    qualify(model=model %>% disable("RUV"), dest=dest, dataset=datasetZNB, variables="ZNB",
            outputFolder=file.path(qualFolder, paste0(modelFolder, "_znb")), reexecuteNONMEM=reexecuteNONMEM, settings=settings)
  expect_true(qual %>% passed())
  # if (exportReport) {
  #   qual %>% write(file=paste0(reportFolder, "qualification_", modelFolder, "_znb_", dest, ".pdf"))
  # }
})
