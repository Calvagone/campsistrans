library(testthat)
library(campsismod)

context("Tests on custom model 1")

testFolder <-  file.path(getwd(), test_path())
overwriteNonRegressionFiles <- TRUE

modelPath <- function(modelDir, modelName) {
  return(file.path(testFolder, "custom_models", modelDir, modelName))
}

nonRegressionFolderPath <- function(regFolder) {
  return(file.path(testFolder, "non_regression", "custom", regFolder))
}

test_that("Custom model 1 can be imported well", {
  # Note: with nonmem2rx, 'same' column is not imported
  modelDir <- "model1"
  modelName <- "model1.mod"
  regFolder <- modelDir

  object <- importNONMEM2(modelPath(modelDir, modelName))
  model <- object %>%
    export(dest="campsis")
  
  if (overwriteNonRegressionFiles) {
    model %>% write(nonRegressionFolderPath(regFolder))
  }
  
  expect_equal(model, read.campsis(nonRegressionFolderPath(regFolder)))
})

test_that("Custom model 2 can be imported well (duplicate variables in model)", {
  modelDir <- "model2"
  modelName <- "model2.mod"
  regFolder <- modelDir

  object <- importNONMEM2(modelPath(modelDir, modelName))
  model <- object %>%
    export(dest="campsis")
  
  if (overwriteNonRegressionFiles) {
    model %>% write(nonRegressionFolderPath(regFolder))
  }
  
  expect_equal(model, read.campsis(nonRegressionFolderPath(regFolder)))
})

# test_that("Duplicate equations are well replaced", {
# 
#   regFolder <- "duplicate_equation_names"
# 
#   model <- CampsisModel() %>%
#     add(Equation("MAIN", "5")) %>%
#     add(Equation("ODE", "10"), pos=campsismod::Position(OdeRecord())) %>%
#     add(Equation("ERROR", "15"), pos=campsismod::Position(ErrorRecord()))
# 
#   model@model@list[[1]]@statements@list <- model@model@list[[1]]@statements@list %>%
#     append(Equation("MAIN", "MAIN+1")) %>%
#     append(Equation("MAIN", "MAIN+1"))
# 
#   model@model@list[[2]]@statements@list <- model@model@list[[2]]@statements@list %>%
#     append(Ode("A_1", "0")) %>%
#     append(Equation("MAIN", "MAIN+1")) %>%
#     append(Equation("ODE", "ODE+1")) %>%
#     append(Equation("ODE", "ODE+1"))
# 
#   model@model@list[[3]]@statements@list <- model@model@list[[3]]@statements@list %>%
#     append(Equation("ODE", "ODE+1")) %>%
#     append(Equation("ODE", "ODE+1")) %>%
#     append(Equation("ODE_", "99")) %>% # ODE_ already exists
#     append(Equation("ASSIGN", "ODE_")) %>% # And is used
#     append(Equation("ERROR", "ERROR+1")) %>%
#     append(Equation("ERROR", "ERROR+1"))
# 
#   # TODO: when #74 is solved in campsismod
#   # Renaming should work
#   model <- model %>%
#     updateCompartments() %>%
#     add(InitialCondition(compartment=1, rhs="ODE*MAIN*ERROR"))
# 
#   model <- substituteDuplicateEquationNames(model)
# 
#   if (overwriteNonRegressionFiles) {
#     model %>% write(nonRegressionFolderPath(regFolder))
#   }
# 
#   expect_equal(model, campsismod::read.campsis(nonRegressionFolderPath(regFolder)))
# })
# 
# test_that("Model 3 (remove ABBREVIATED REPLACE)", {
#   modelDir <- "model3"
#   modelName <- "model3.mod"
#   regFolder <- "model3"
#   
#   mapping <- mapping(auto=TRUE)
#   
#   model <- generateModel(modelDir=modelDir, modelName=modelName, regFolder=regFolder, mapping=mapping, copy_dir=TRUE, rem_rate=TRUE)
#   expect_equal(model, campsismod::read.campsis(nonRegressionFolderPath(regFolder)))
#   
#   expect_equal(removeAbbreviatedReplaceFromString("BEFORE\n$ABBREVIATED REPLACE HELLO\n$AFTER"), "BEFORE\n\n$AFTER")
# })
# 
# 



