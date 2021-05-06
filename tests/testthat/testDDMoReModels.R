library(testthat)
library(pmxmod)

context("Test NONMEM import on a few DDMoRE models")

testFolder <- ""
overwriteNonRegressionFiles <- TRUE

modelPath <- function(filename) {
  return(paste0(testFolder, "ddmore_models/", filename))
}

nonRegressionFolderPath <- function(folder) {
  return(paste0(testFolder, "non_regression/ddmore/", folder, "/"))
}

generateModel <- function(filename, folder, mapping=NULL) {
  pmxtran <- importNONMEM(modelPath(filename), mapping=mapping, estimate=FALSE)
  
  model <- pmxtran %>% export(dest="pmxmod")
  
  if (overwriteNonRegressionFiles) {
    model %>% write(nonRegressionFolderPath(folder))
  }
  return(model)
}

test_that("Rifampin PK can be imported well", {
  # DDMODEL00000280
  # Pharmacokinetics of rifampin in tuberculosis patients
  
  filename="Executable_real_TB_Rifampicin_PK_Wilkins_2008.mod"
  folder <- "rifampin"
  mapping <- mapping(omega=1:17) # Explicitely tell pmxtran there are 17 OMEGA's
  model <- generateModel(filename=filename, folder=folder, mapping=mapping)
  expect_equal(model, read.pmxmod(nonRegressionFolderPath(folder)))
})

test_that("Paracetamol PK (in newborns) can be imported well", {
  # DDMODEL00000271
  # Paracetamol and metabolite PK in newborns
  
  filename="Executable_ParacetamolInNewborns.mod"
  folder <- "paracetamol"
  model <- generateModel(filename=filename, folder=folder)
  expect_equal(model, read.pmxmod(nonRegressionFolderPath(folder)))
})

test_that("Midazolam PK (in newborns) can be imported well", {
  # DDMODEL00000250
  # Midazolam PK in obese adults and adolescents
  
  filename="Executable_Midazolam_PK.mod"
  folder <- "midazolam"
  model <- generateModel(filename=filename, folder=folder)
  expect_equal(model, read.pmxmod(nonRegressionFolderPath(folder)))
})
