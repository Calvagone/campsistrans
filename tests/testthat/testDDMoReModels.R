library(testthat)
library(campsismod)

context("Test NONMEM import on a few DDMoRE models")

testFolder <- ""
overwriteNonRegressionFiles <- FALSE

modelPath <- function(filename) {
  return(paste0(testFolder, "ddmore_models/", filename))
}

nonRegressionFolderPath <- function(folder) {
  return(paste0(testFolder, "non_regression/ddmore/", folder, "/"))
}

generateModel <- function(filename, folder, mapping=NULL) {
  object <- importNONMEM(modelPath(filename), mapping=mapping, estimate=FALSE)
  
  model <- object %>% export(dest="campsis")
  
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
  mapping <- mapping(omega=1:17) # Explicitely tell campsistrans there are 17 OMEGA's
  model <- generateModel(filename=filename, folder=folder, mapping=mapping)
  expect_equal(model, read.campsis(nonRegressionFolderPath(folder)))
  
  # NOTE THAT ODE:
  # if (T >= TDOS) DADT(1)=-A_1*KA + (KTR + X)*(PD + X)*exp(-KTR*(T - TDOS) - L + NN*log(KTR*(T - TDOS) + X))
  # IS NOT IMPORTED CORRECTLY...
  # As a consequence, NONMEM auto-detection is incorrect: [F] A_2=F1 (only 1 compartment is detected)
})

test_that("Paracetamol PK (in newborns) can be imported well", {
  # DDMODEL00000271
  # Paracetamol and metabolite PK in newborns
  
  filename="Executable_ParacetamolInNewborns.mod"
  folder <- "paracetamol"
  model <- generateModel(filename=filename, folder=folder)
  expect_equal(model, read.campsis(nonRegressionFolderPath(folder)))
})

test_that("Midazolam PK (in newborns) can be imported well", {
  # DDMODEL00000250
  # Midazolam PK in obese adults and adolescents
  
  filename="Executable_Midazolam_PK.mod"
  folder <- "midazolam"
  model <- generateModel(filename=filename, folder=folder)
  expect_equal(model, read.campsis(nonRegressionFolderPath(folder)))
})
