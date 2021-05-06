library(testthat)
library(pmxmod)

context("Tests on custom model 1")

testFolder <- "C:/prj/pmxtran/tests/testthat/"
overwriteNonRegressionFiles <- FALSE

modelPath <- function(filename) {
  return(paste0(testFolder, "ddmore_models/", filename))
}

nonRegressionFolderPath <- function(folder) {
  return(paste0(testFolder, "non_regression/ddmore/", folder, "/"))
}

generateModel <- function(filename, folder, mapping) {
  pmxtran <- importNONMEM(modelPath(filename), mapping=mapping, estimate=FALSE)
  
  model <- pmxtran %>% export(dest="pmxmod")
  
  if (overwriteNonRegressionFiles) {
    model %>% write(nonRegressionFolderPath(folder))
  }
  return(model)
}

test_that("Rifampin", {
  # DDMODEL00000280
  # Pharmacokinetics of rifampin in tuberculosis patients
  
  filename="Executable_real_TB_Rifampicin_PK_Wilkins_2008.mod"
  folder <- "rifampin"
  mapping <- mapping(omega=1:17) # Explicitely tell pmxtran there are 17 OMEGA's
  model <- generateModel(filename=filename, folder=folder, mapping=mapping)
  expect_equal(model, read.pmxmod(nonRegressionFolderPath(folder)))
})
