library(testthat)
library(campsismod)

context("Test that NONMEM files can be prepared for the qualification")

testFolder <-  file.path(getwd(), test_path())
overwriteNonRegressionFiles <- FALSE

modelPath <- function(folder, filename) {
  return(file.path(testFolder, "ddmore_models", folder, filename))
}

test_that("Rifampin PK can be imported well (pharmpy/nonmem2rx)", {
  installPharmpy(UpdatedPharmpyConfig())
  pharmpy <- importPharmpyPackage(UpdatedPharmpyConfig())
  
  

  
  model <- pharmpy$modeling$read_model(path)
  
  # Print model code
  pharmpy$modeling$print_model_code(model)
  #pharmpy$modeling$convert_model(model, "nlmixr")
  
  # Retrieve the parameter estimates
  results <- pharmpy$tools$read_modelfit_results(path)
  parameter_estimates <- results$parameter_estimates

  # Access the initial parameters
  params <- model$parameters
  
  # Replace initial estimates with the parameter estimates
  params <- params$set_initial_estimates(as.list(parameter_estimates))
  
  # Fix all parameters
  params <- params$set_fix(as.list(parameter_estimates))
  
  # Replace initial estimates with the parameter estimates
  # model <- pharmpy$modeling$parameters$set_initial_estimates(model, reticulate::r_to_py(as.list(parameter_estimates)))
  
  model <- pharmpy$modeling$unconstrain_parameters(model=model, parameter_names=model$parameters$names)
  
  # Replace in original model
  model <- model$replace(parameters=params)

  # Not working......
  model <- model$update_source()
  pharmpy$modeling$print_model_code(model)
  
  # Write model
  pharmpy$modeling$write_model(model=model, path="C:/Users/nicolas.luyckx.CALVAGONE/Desktop/Pharmpy/Export/export.mod", force=TRUE)
  
  
  # To continue
  pharmpy$model$external$nonmem$records$data_record

  reticulate::dict(parameter_estimates)
})
