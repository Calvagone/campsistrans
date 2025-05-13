library(testthat)
library(campsismod)

context("Test NONMEM import on a few DDMoRE models")

testFolder <-  file.path(getwd(), test_path())
overwriteNonRegressionFiles <- TRUE

modelPath <- function(folder, filename) {
  return(file.path(testFolder, "ddmore_models", folder, filename))
}

nonRegressionPharmpyPath <- function(folder) {
  return(file.path(testFolder, "non_regression", "ddmore", folder, "pharmpy"))
}

nonRegressionNonmem2rxPath <- function(folder) {
  return(file.path(testFolder, "non_regression", "ddmore", folder, "nonmem2rx"))
}

generateModel <- function(filename, folder, mapping=NULL, modelfun=NULL, suppressWarnings=TRUE, unknownStatements=FALSE) {
  expr <- expression(importNONMEM(modelPath(folder, filename), mapping=mapping, estimate=FALSE, copy_dir=TRUE, rem_rate=TRUE))
  if (suppressWarnings) {
    object <- suppressWarnings(eval(expr))
  } else {
    object <- importNONMEM(eval(expr))
  }
  
  model <- object %>% export(dest="campsis")
  if (!is.null(modelfun)) {
    model <- modelfun(model)
  }
  
  if (overwriteNonRegressionFiles) {
    model %>% write(nonRegressionPharmpyPath(folder))
  }
  
  # Generate unknown statements by writing/reading the model
  if (unknownStatements) {
    dir <- tempdir()
    if (!dir.exists(dir)) dir.create(dir)
    model %>% campsismod::write(dir)
    model <- suppressWarnings(read.campsis(dir))
  }
  return(model)
}

generateModel2 <- function(filename, folder, ctlExt="mod", extExt="ext", covExt="cov", unknownStatements=FALSE) {
  fullPath <- modelPath(folder, filename)
  dir <- dirname(fullPath)
  
  ctl <- list.files(dir, pattern=sprintf("*\\.%s$", ctlExt), full.names=TRUE)
  ext <- list.files(dir, pattern=sprintf("*\\.%s$", extExt), full.names=TRUE)
  cov <- list.files(dir, pattern=sprintf("*\\.%s$", covExt), full.names=TRUE)
  
  ext <- if (length(ext)==0) NULL else ext
  cov <- if (length(cov)==0) NULL else cov
  
  model <- importNONMEM2(ctlFile=ctl, extFile=ext, covFile=cov)
  
  if (overwriteNonRegressionFiles) {
    model %>% write(nonRegressionNonmem2rxPath(folder))
  }
  
  # Generate unknown statements by writing/reading the model
  if (unknownStatements) {
    dir <- tempdir()
    if (!dir.exists(dir)) dir.create(dir)
    model %>% campsismod::write(dir)
    model <- suppressWarnings(read.campsis(dir))
  }
  
  return(model)
}

# test_that("Rifampin PK can be imported well", {
#   # DDMODEL00000280
#   # Pharmacokinetics of rifampin in tuberculosis patients
# 
#   filename="Executable_real_TB_Rifampicin_PK_Wilkins_2008.mod"
#   folder <- "rifampin"
#   mapping <- mapping(omega=1:17) # Explicitely tell campsistrans there are 17 OMEGA's
# 
#   model <- generateModel(filename=filename, folder=folder, mapping=mapping)
#   nonreg_model <- suppressWarnings(read.campsis(nonRegressionPharmpyPath(folder)))
# 
#   # NOTE THAT ODE:
#   # if (T >= TDOS) DADT(1)=-A_1*KA + (KTR + X)*(PD + X)*exp(-KTR*(T - TDOS) - L + NN*log(KTR*(T - TDOS) + X))
#   # IS NOT IMPORTED CORRECTLY...
#   # As a consequence, NONMEM auto-detection is incorrect: [F] A_2=F1 (only 1 compartment is detected)
# 
#   # Furthemore this ODE, is read as a unknown statement by campsismod (as variable is incorrect)
#   # For this test, we delete this 'ODE' on both sides
#   ode <- model@model %>% getByName("ODE")
#   nonreg_ode <- nonreg_model@model %>% getByName("ODE")
# 
#   ode@statements@list <- ode@statements@list %>% purrr::discard(~is(.x, "if_statement") && .x@condition == "t >= TDOS")
#   nonreg_ode@statements@list <- nonreg_ode@statements@list %>% purrr::discard(~is(.x, "unknown_statement"))
# 
#   expect_equal(model %>% campsismod::replace(ode), nonreg_model %>% campsismod::replace(nonreg_ode))
# })
# 
# test_that("Rifampin PK can be imported well (no omega mapping)", {
#   # DDMODEL00000280
#   # Pharmacokinetics of rifampin in tuberculosis patients
# 
#   filename="Executable_real_TB_Rifampicin_PK_Wilkins_2008.mod"
#   folder <- "rifampin_no_omega_mapping"
# 
#   model <- suppressWarnings(generateModel(filename=filename, folder=folder, mapping=mapping(auto=TRUE)))
#   if (overwriteNonRegressionFiles) {
#     model %>% write(nonRegressionPharmpyPath(folder))
#   }
#   nonreg_model <- suppressWarnings(read.campsis(nonRegressionPharmpyPath(folder)))
# 
#   ode <- model@model %>% getByName("ODE")
#   nonreg_ode <- nonreg_model@model %>% getByName("ODE")
# 
#   ode@statements@list <- ode@statements@list %>% purrr::discard(~is(.x, "if_statement") && .x@condition == "t >= TDOS")
#   nonreg_ode@statements@list <- nonreg_ode@statements@list %>% purrr::discard(~is(.x, "unknown_statement"))
# 
#   expect_equal(model %>% campsismod::replace(ode), nonreg_model %>% campsismod::replace(nonreg_ode))
# })
# 
# test_that("Paracetamol PK (in newborns) can be imported well", {
#   # DDMODEL00000271
#   # Paracetamol and metabolite PK in newborns
# 
#   filename <- "Executable_ParacetamolInNewborns.mod"
#   folder <- "paracetamol"
# 
#   # Note: when updating Pharmpy from 0.30.1 to 0.43.0
#   # I had to rename (CENTRAL,DEFDOSE) into (COMP1)
#   # Otherwise, there was a bug in Pharmpy (file advan.py, line 201, lhs_sum = dadt_dose.expression)
#   model1 <- generateModel(filename=filename, folder=folder)
#   expect_equal(model1, read.campsis(nonRegressionPharmpyPath(folder)))
# 
#   # Same with nonmem2rx
#   model2 <- generateModel2(filename=filename, folder=folder)
#   expect_equal(model2, read.campsis(nonRegressionNonmem2rxPath(folder)))
# })
# 
# test_that("Midazolam PK (in newborns) can be imported well", {
#   # DDMODEL00000250
#   # Midazolam PK in obese adults and adolescents
# 
#   filename <- "Executable_Midazolam_PK.mod"
#   folder <- "midazolam"
#   model <- generateModel(filename=filename, folder=folder)
#   expect_equal(model, read.campsis(nonRegressionPharmpyPath(folder)))
#   
#   # Same with nonmem2rx
#   model2 <- generateModel2(filename=filename, folder=folder)
#   expect_equal(model2, read.campsis(nonRegressionNonmem2rxPath(folder)))
# 
# })
# 
# test_that("Filgrastim PK/PD model (Krzyzanski et al.) can be imported well", {
#   # DDMODEL00000077
#   # Krzyzanski_2010_Filgastrim_PKPD
# 
#   filename <- "Executable_simulated_GCSF_dataset.ctl"
#   folder <- "filgrastim"
# 
#   mapping <- mapping(theta=c(FF=1, KA1=2, FR=3, D2=4, KEL=5, VD=6, KD=7, KINT=8, KSI=9, KOFF=10, KMT=11, KBB1=12, KTT=13, NB0=14, SC1=15, SM1=16, SM2=17, SM3=18),
#                      omega=c(NB0=1, KEL=2, VD=3, KA1=4, KSI=5, SC1=6, SM1=7, SM2=8))
# 
#   modelfun <- function(model) {
#     model <- model %>%
#       delete(IfStatement("CMT == 2", Equation("IPRED")))%>%
#       delete(IfStatement("CMT == 2", Equation("IRES"))) %>%
#       delete(IfStatement("CMT == 2", Equation("Y"))) %>%
#       delete(IfStatement("CMT == 4", Equation("IPRED"))) %>%
#       delete(IfStatement("CMT == 4", Equation("IRES"))) %>%
#       delete(IfStatement("CMT == 4", Equation("Y")))
#     return(model)
#   }
#   model <- generateModel(filename=filename, folder=folder, mapping=mapping, modelfun=modelfun)
#   expect_equal(model, read.campsis(nonRegressionPharmpyPath(folder)))
# 
#   # Same with nonmem2rx
#   model2 <- generateModel2(filename=filename, folder=folder, ctlExt="ctl", unknownStatements=TRUE)
#   expect_equal(model2, suppressWarnings(read.campsis(nonRegressionNonmem2rxPath(folder))))
# })
# 
# test_that("Colistin Meropenem can be imported well", {
#   # DDMODEL00000173
#   # Import non perfect because of unknow statements (-> DADT in conditional statements)
# 
#   filename <- "ColistinMeropenem_Interaction_original_simulated.mod"
#   folder <- "colistin_meropenem"
# 
#   mapping <- mapping(auto=TRUE)
# 
#   model <- generateModel(filename=filename, folder=folder, mapping=mapping, unknownStatements=TRUE)
#   expect_equal(model, suppressWarnings(read.campsis(nonRegressionPharmpyPath(folder))))
#   
#   # Same with nonmem2rx
#   # Not working: Error: syntax/parsing errors:
#   # syntax error: 2+ single population parameters in a single mu-referenced expression: 'theta1', 'theta2'
#   # model2 <- generateModel2(filename=filename, folder=folder)
# })
#
test_that("Likert pain count can be imported well", {
  # DDMODEL00000194

  filename <- "Executable_likert_pain_count.mod"
  folder <- "likert_pain_count"

  mapping <- mapping(auto=TRUE)

  model1 <- generateModel(filename=filename, folder=folder, mapping=mapping)
  expect_equal(model1, suppressWarnings(read.campsis(nonRegressionPharmpyPath(folder))))
  
  # Same with nonmem2rx
  model2 <- generateModel2(filename=filename, folder=folder, unknownStatements=TRUE)
  expect_equal(model2, suppressWarnings(read.campsis(nonRegressionNonmem2rxPath(folder))))
})
# 
# test_that("Biomarker GIST can be imported well", {
#   # DDMODEL00000197
# 
#   filename <- "Executable_Biomarker_GIST.mod"
#   folder <- "biomarker_gist"
# 
#   mapping <- mapping(auto=TRUE)
# 
#   model <- generateModel(filename=filename, folder=folder, mapping=mapping)
# 
#   expect_equal(model, suppressWarnings(read.campsis(nonRegressionPharmpyPath(folder))))
# })
# 
# test_that("TGI GIST can be imported well", {
#   # DDMODEL00000198
# 
#   filename <- "Executable_TGI_GIST.mod"
#   folder <- "tgi_gist"
# 
#   mapping <- mapping(auto=TRUE)
# 
#   model <- generateModel(filename=filename, folder=folder, mapping=mapping)
# 
#   expect_equal(model, suppressWarnings(read.campsis(nonRegressionPharmpyPath(folder))))
# })
# 
# test_that("HFS model can be imported well", {
#   # DDMODEL00000214
# 
#   filename <- "Executable_HFSmodel.mod"
#   folder <- "hfs_model"
# 
#   mapping <- mapping(auto=TRUE)
# 
#   model <- generateModel(filename=filename, folder=folder, mapping=mapping)
# 
#   expect_equal(model, suppressWarnings(read.campsis(nonRegressionPharmpyPath(folder))))
# })
# 
# test_that("Pimasertib can be imported well", {
#   # DDMODEL00000215
# 
#   filename <- "Executable_Pimasertib_AeDropout.mod"
#   folder <- "pimasertib"
# 
#   mapping <- mapping(auto=TRUE)
# 
#   model <- generateModel(filename=filename, folder=folder, mapping=mapping)
# 
#   expect_equal(model, suppressWarnings(read.campsis(nonRegressionPharmpyPath(folder))))
# })
# 
# test_that("SLD model can be imported well", {
#   # DDMODEL00000217
#   # Super strange: I had to add this DUMMY_EQ=0 equation in $DES to make it work
# 
#   filename <- "Executable_SLD.mod"
#   folder <- "sld_model"
# 
#   mapping <- mapping(auto=TRUE)
# 
#   model <- generateModel(filename=filename, folder=folder, mapping=mapping)
# 
#   expect_equal(model, suppressWarnings(read.campsis(nonRegressionPharmpyPath(folder))))
# })
# 
# test_that("OS model can be imported well", {
#   # DDMODEL00000218
# 
#   filename <- "Executable_OS.mod"
#   folder <- "os_model"
# 
#   mapping <- mapping(auto=TRUE)
# 
#   model <- generateModel(filename=filename, folder=folder, mapping=mapping)
# 
#   expect_equal(model, suppressWarnings(read.campsis(nonRegressionPharmpyPath(folder))))
# })
# 
# test_that("BDQ M2 popPK model can be imported well", {
#   # DDMODEL00000219
# 
#   filename <- "Executable_BDQ_M2_PK_plus_WT_ALB_in_MDR-TB_patients.mod"
#   folder <- "bdq_m2_poppk"
# 
#   mapping <- mapping(auto=TRUE)
# 
#   modelfun <- function(model) {
#     model <- model %>%
#       add(Omega(name="BOV F SAME", index=7, index2=7, value=0)) %>%
#       add(Omega(name="BOV MAT SAME", index=9, index2=9, value=0))
#     model <- model %>%
#       campsismod::sort()
#     return(model)
#   }
# 
#   model <- generateModel(filename=filename, folder=folder, mapping=mapping, modelfun=modelfun)
# 
#   expect_equal(model, suppressWarnings(read.campsis(nonRegressionPharmpyPath(folder))))
# })
# 
# test_that("CPHPC model can be imported well", {
#   # DDMODEL00000262
# 
#   filename <- "Executable_simulated_CPHPC_dataset.ctl"
#   folder <- "cphpc_model"
# 
#   mapping <- mapping(auto=TRUE)
# 
#   model <- generateModel(filename=filename, folder=folder, mapping=mapping)
# 
#   expect_equal(model, suppressWarnings(read.campsis(nonRegressionPharmpyPath(folder))))
# })

