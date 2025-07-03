library(testthat)
library(rxode2)

context("Test conversion from Pharmpy to nlxmir")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))

test_that("Conversion to nlxmir works as expected (Pharmpy)", {
  if (skipPharmpyTests()) return(TRUE)
  
  campsistrans <- importNONMEM(getNONMEMModelTemplate(4, 4), mapping=mapping(auto=TRUE))
  pharmpy <- importPharmpyPackage()
  
  # See here that pharmy is able to export to nlmixr
  model <- campsistrans@model
  estimationStep <- pharmpy$modeling$estimation_steps$EstimationStep(method="FOCE")
  steps <- pharmpy$model$ExecutionSteps(list(estimationStep))
  model <- model$replace(execution_steps=steps)

  mod <- pharmpy$modeling$convert_model(model, "nlmixr")
  code <- reticulate::py_capture_output(pharmpy$modeling$print_model_code(mod))
  
  code <- strsplit(code, "\n")[[1]]
  
  # Remove last line
  code <- code[!grepl("^fit <- nlmixr2", code)]  # Remove nlmixr2 fit line
  
  # Remove generated error block
  code <- code[!grepl("\\s+SIGMA_1_1 ", code)]
  code <- code[!grepl("\\s+CONC_ERR <- CONC\\*\\(EPS_1 \\+ 1\\)", code)]
  code <- code[!grepl("\\s+Y <- CONC_ERR", code)]
  code <- code[!grepl("\\s+add_error <- 0", code)]
  code <- code[!grepl("\\s+prop_error <- 0", code)]
  code <- code[!grepl("\\s+Y ~ add\\(add_error\\) \\+ prop\\(prop_error\\)", code)]
  
  # Evaluate code
  expr <- eval(parse(text=code))
  rxmod <- rxode2::rxode2(expr())

  expect_true("rxUi" %in% class(rxmod))
})

