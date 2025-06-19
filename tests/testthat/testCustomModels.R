library(testthat)
library(campsismod)

context("Tests on custom model 1")

testFolder <-  file.path(getwd(), test_path())
overwriteNonRegressionFiles <- FALSE

modelPath <- function(modelDir, modelName) {
  return(file.path(testFolder, "custom_models", modelDir, modelName))
}

nonRegressionFolderPath <- function(regFolder) {
  return(file.path(testFolder, "non_regression", "custom", regFolder))
}

generateModel <- function(modelDir, modelName, regFolder, mapping, ...) {
  # Import your NONMEM model using pharmpy
  object <- importNONMEM(file=modelPath(modelDir, modelName), mapping=mapping, ...)
  
  model <- object %>% export(dest="campsis")
  if (overwriteNonRegressionFiles) {
    model %>% campsismod::write(nonRegressionFolderPath(regFolder))
  }
  return(model)
}

test_that("Model 1", {
  modelDir <- "model1"
  modelName <- "model1.mod"
  regFolder <- modelDir


  # Also map OMEGA names because 'SAME' OMEGAS related to IOV not listed in Pharmpy parameter set
  mapping <- mapping(theta=c("CL"=1, "V2"=2, "Q3"=3, "V3"=4, "KA"=5, "F1"=6, "W"=7, "NFOOD"=8, "LAMB"=9, "IMAX"=10, "CLBW"=11, "VDBW"=12, "CL99"=13),
                     omega=1:24,
                     sigma=c("ADD"=1))

  model <- generateModel(modelDir=modelDir, modelName=modelName, regFolder=modelDir, mapping=mapping, copy_dir=TRUE, rem_rate=TRUE)
  expect_equal(model, campsismod::read.campsis(nonRegressionFolderPath(modelDir)))
})

test_that("Model 1 (auto-mapping)", {
  modelDir <- "model1"
  modelName <- "model1.mod"
  regFolder <- "model1_auto_mapping"

  # Also map OMEGA names because 'SAME' OMEGAS related to IOV not listed in Pharmpy parameter set
  mapping <- mapping(omega=1:24, sigma=c("ADD"=1), auto=TRUE)

  model <- generateModel(modelDir=modelDir, modelName=modelName, regFolder=regFolder, mapping=mapping, copy_dir=TRUE, rem_rate=TRUE)
  expect_equal(model, campsismod::read.campsis(nonRegressionFolderPath(regFolder)))
})

test_that("Model 2 (duplicate variables in model)", {
  modelDir <- "model2"
  modelName <- "model2.mod"
  regFolder <- "model2_auto_mapping"

  # Also map OMEGA names because 'SAME' OMEGAS related to IOV not listed in Pharmpy parameter set
  mapping <- mapping(auto=TRUE)

  model <- generateModel(modelDir=modelDir, modelName=modelName, regFolder=regFolder, mapping=mapping, copy_dir=TRUE, rem_rate=TRUE)
  expect_equal(model, campsismod::read.campsis(nonRegressionFolderPath(regFolder)))
})

test_that("Duplicate equations are well replaced", {

  regFolder <- "duplicate_equation_names"

  model <- CampsisModel() %>%
    add(Equation("MAIN", "5")) %>%
    add(Equation("ODE", "10"), pos=campsismod::Position(OdeRecord())) %>%
    add(Equation("ERROR", "15"), pos=campsismod::Position(ErrorRecord()))

  model@model@list[[1]]@statements@list <- model@model@list[[1]]@statements@list %>%
    append(Equation("MAIN", "MAIN+1")) %>%
    append(Equation("MAIN", "MAIN+1"))

  model@model@list[[2]]@statements@list <- model@model@list[[2]]@statements@list %>%
    append(Ode("A_1", "0")) %>%
    append(Equation("MAIN", "MAIN+1")) %>%
    append(Equation("ODE", "ODE+1")) %>%
    append(Equation("ODE", "ODE+1"))

  model@model@list[[3]]@statements@list <- model@model@list[[3]]@statements@list %>%
    append(Equation("ODE", "ODE+1")) %>%
    append(Equation("ODE", "ODE+1")) %>%
    append(Equation("ODE_", "99")) %>% # ODE_ already exists
    append(Equation("ASSIGN", "ODE_")) %>% # And is used
    append(Equation("ERROR", "ERROR+1")) %>%
    append(Equation("ERROR", "ERROR+1"))

  # TODO: when #74 is solved in campsismod
  # Renaming should work
  model <- model %>%
    updateCompartments() %>%
    add(InitialCondition(compartment=1, rhs="ODE*MAIN*ERROR"))

  model <- substituteDuplicateEquationNames(model)

  if (overwriteNonRegressionFiles) {
    model %>% write(nonRegressionFolderPath(regFolder))
  }

  expect_equal(model, campsismod::read.campsis(nonRegressionFolderPath(regFolder)))
})

test_that("Model 3 (remove ABBREVIATED REPLACE)", {
  modelDir <- "model3"
  modelName <- "model3.mod"
  regFolder <- "model3"
  
  mapping <- mapping(auto=TRUE)
  
  model <- generateModel(modelDir=modelDir, modelName=modelName, regFolder=regFolder, mapping=mapping, copy_dir=TRUE, rem_rate=TRUE)
  expect_equal(model, campsismod::read.campsis(nonRegressionFolderPath(regFolder)))
  
  expect_equal(removeAbbreviatedReplaceFromString("BEFORE\n$ABBREVIATED REPLACE HELLO\n$AFTER"), "BEFORE\n\n$AFTER")
})





