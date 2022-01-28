library(testthat)
library(campsismod)

context("Test NONMEM import on a few DDMoRE models")

testFolder <<- ""
overwriteNonRegressionFiles <- FALSE

modelPath <- function(folder, filename) {
  return(paste0(testFolder, "ddmore_models/", folder, "/", filename))
}

nonRegressionFolderPath <- function(folder) {
  return(paste0(testFolder, "non_regression/ddmore/", folder, "/"))
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

test_that("Rifampin PK can be imported well", {
  # DDMODEL00000280
  # Pharmacokinetics of rifampin in tuberculosis patients
  
  filename="Executable_real_TB_Rifampicin_PK_Wilkins_2008.mod"
  folder <- "rifampin"
  mapping <- mapping(omega=1:17) # Explicitely tell campsistrans there are 17 OMEGA's
  
  model <- generateModel(filename=filename, folder=folder, mapping=mapping)
  nonreg_model <- suppressWarnings(read.campsis(nonRegressionFolderPath(folder)))
  
  # NOTE THAT ODE:
  # if (T >= TDOS) DADT(1)=-A_1*KA + (KTR + X)*(PD + X)*exp(-KTR*(T - TDOS) - L + NN*log(KTR*(T - TDOS) + X))
  # IS NOT IMPORTED CORRECTLY...
  # As a consequence, NONMEM auto-detection is incorrect: [F] A_2=F1 (only 1 compartment is detected)
  
  # Furthemore this ODE, is read as a unknown statement by campsismod (as variable is incorrect)
  # For this test, we delete this 'ODE' on both sides
  ode <- model@model %>% getByName("ODE")
  nonreg_ode <- nonreg_model@model %>% getByName("ODE")
  
  ode@statements@list <- ode@statements@list %>% purrr::discard(~is(.x, "if_statement") && .x@condition == "T >= TDOS")
  nonreg_ode@statements@list <- nonreg_ode@statements@list %>% purrr::discard(~is(.x, "unknown_statement"))
  
  expect_equal(model %>% campsismod::replace(ode), nonreg_model %>% campsismod::replace(nonreg_ode))
})

test_that("Paracetamol PK (in newborns) can be imported well", {
  # DDMODEL00000271
  # Paracetamol and metabolite PK in newborns
  
  filename="Executable_ParacetamolInNewborns.mod"
  folder <- "paracetamol"

  # Note: when updating Pharmpy from 0.30.1 to 0.43.0
  # I had to rename (CENTRAL,DEFDOSE) into (COMP1)
  # Otherwise, there was a bug in Pharmpy (file advan.py, line 201, lhs_sum = dadt_dose.expression)
  model <- generateModel(filename=filename, folder=folder)
  expect_equal(model, read.campsis(nonRegressionFolderPath(folder)))
})

test_that("Midazolam PK (in newborns) can be imported well", {
  # DDMODEL00000250
  # Midazolam PK in obese adults and adolescents
  
  filename <- "Executable_Midazolam_PK.mod"
  folder <- "midazolam"
  model <- generateModel(filename=filename, folder=folder)
  expect_equal(model, read.campsis(nonRegressionFolderPath(folder)))
})

test_that("Filgrastim PK/PD model (Krzyzanski et al.) can be imported well", {
  # DDMODEL00000077
  # Krzyzanski_2010_Filgastrim_PKPD
  
  filename <- "Executable_simulated_GCSF_dataset_modified.ctl"
  folder <- "filgrastim"
  
  mapping <- mapping(theta=c(FF=1, KA1=2, FR=3, D2=4, KEL=5, VD=6, KD=7, KINT=8, KSI=9, KOFF=10, KMT=11, KBB1=12, KTT=13, NB0=14, SC1=15, SM1=16, SM2=17, SM3=18),
                     omega=c(NB0=1, KEL=2, VD=3, KA1=4, KSI=5, SC1=6, SM1=7, SM2=8))
  
  modelfun <- function(model) {
    model <- model %>%
      delete(IfStatement("CMT == 2", Equation("IPRED")))%>%
      delete(IfStatement("CMT == 2", Equation("IRES"))) %>%
      delete(IfStatement("CMT == 2", Equation("Y"))) %>%
      delete(IfStatement("CMT == 4", Equation("IPRED"))) %>%
      delete(IfStatement("CMT == 4", Equation("IRES"))) %>%
      delete(IfStatement("CMT == 4", Equation("Y")))
    return(model)
  }
  model <- generateModel(filename=filename, folder=folder, mapping=mapping, modelfun=modelfun)
  expect_equal(model, read.campsis(nonRegressionFolderPath(folder)))
})
