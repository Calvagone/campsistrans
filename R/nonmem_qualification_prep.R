
#' Standardise dataset (dataframe) received from CAMPSIS.
#' This process will be implemented later on in CAMPSIS (using export dest='NONMEM').
#' 
#' @param dataset dataframe or tibble
#' @return a standardised dataset
#' @importFrom dplyr mutate_at
#' @importFrom tibble add_column as_tibble
#' @export
standardiseDataset <- function(dataset) {
  # A couple of checks
  if (!is(dataset, "data.frame")) {
    stop("dataset must be a data frame")
  }
  if (!is(dataset, "tbl_df")) {
    dataset <- dataset %>% tibble::as_tibble()
  }
  
  # Add DV column if not present
  if (!("DV" %in% colnames(dataset))) {
    dataset <- dataset %>% tibble::add_column(DV=".", .after="MDV")
  }
  
  # Export to character dataframe
  dataset <- dataset %>% dplyr::mutate_at(.vars=colnames(dataset), .funs=as.character)
  
  # Replace NAs by dots
  dataset <- dataset %>% dplyr::mutate_at(.vars=colnames(dataset), .funs=~ifelse(is.na(.x), ".", .x))
  
  return(dataset)
}

#' Export campsistrans object to NONMEM control stream for qualification.
#' If NONMEM results are provided, they will replace the previous original values in the control stream.
#' OMEGA and SIGMA initial values will then be fixed and set to 0.
#' ETA arrays in control stream will be replaced by ETA covariates (e.g. ETA(1) -> ETA_1)
#' 
#' @param x campsistrans object
#' @param dataset dataframe or tibble
#' @param variables variables to output (note: ID, ARM, TIME, EVID, MDV, DV, AMT, CMT, DOSENO are output by default)
#' @param compartments compartment indexes to output, numeric vector
#' @param outputFolder output folder to export the qualification control stream and CSV dataset
#' @return the updated campsistrans object
#' @importFrom tibble add_column as_tibble
#' @export
prepareNONMEMFiles <- function(x, dataset, variables, compartments=NULL, outputFolder) {
  
  # Load module Pharmpy
  pharmpy <- reticulate::import("pharmpy")
  
  # Standardise NONMEM dataset
  dataset <- standardiseDataset(dataset)
  

  
  # Update source
  # This will update the 'model' with the new values from the generic model
  pharmpyModel$update_source()
  

  
  # Update ERROR record
  compartmentNames <- NULL
  if (length(compartments) > 0) {
    oldError <- pharmpyModel$control_stream$get_records("ERROR")
    if (length(oldError) == 0) {
      stop("No ERROR record available")
    }
    statements <- oldError[[1]]$statements["_statements"]
    copy <- list()
    for (index in (seq_along(statements))) {
      statement <- statements[[index]]
      copy <- c(copy, statement)
    }
    for (compartment in compartments) {
      compartmentName <- paste0("A_", compartment)
      compartmentNames <- c(compartmentNames, compartmentName)
      equation <- pharmpy$statements$Assignment(compartmentName, paste0("A(", compartment, ")"))
      copy <- c(copy, equation)
    }
    oldError[[1]]$statements <- pharmpy$statements$ModelStatements(copy)
  }
  

  
  # Remove COVARIANCE record ($SIMULATE: CAN'T USE ONLYSIMULATION WITH $EST, $COV, $NONP)
  covariance <- pharmpyModel$control_stream$get_records("COVARIANCE")
  pharmpyModel$control_stream$remove_records(covariance)

  # Remove TABLE record
  table <- pharmpyModel$control_stream$get_records("TABLE")
  pharmpyModel$control_stream$remove_records(table)

  # Preparing variables to output
  variablesDataset <- colnames(dataset)
  defaultVariables <- c("ID", "ARM", "TIME", "EVID", "MDV", "DV", "AMT", "CMT", "DOSENO")
  defaultVariables <- defaultVariables[defaultVariables %in% variablesDataset]
  allVariables <- unique(c(defaultVariables, variables, compartmentNames))

  # Create TABLE record
  table <- pharmpyModel$control_stream$append_record(paste0("$TABLE ", paste0(allVariables, collapse=" "),
                                                             " FILE=output.tab ONEHEADER NOAPPEND NOPRINT\n"))

  # Make ETA's as covariates
  pharmpyModel <- updateETAinNONMEMRecord(pharmpyModel, "PRED", x@campsis@parameters)
  pharmpyModel <- updateETAinNONMEMRecord(pharmpyModel, "PK", x@campsis@parameters)
  pharmpyModel <- updateETAinNONMEMRecord(pharmpyModel, "ERROR", x@campsis@parameters)
  
  # Write NONMEM dataset
  write.csv(dataset, file=paste0(outputFolder, "/", "dataset.csv"), quote=FALSE, row.names=FALSE)
  
  # Write qualification control stream
  ctl <- as.character(pharmpyModel$control_stream)
  fileConn <- file(paste0(outputFolder, "/", "model.mod"))
  writeLines(text=ctl, fileConn)
  close(fileConn)
  
  return(paste0(outputFolder, "/", "model.mod"))
}

updateControlStream <- function(model, control_stream) {
  # Replace control stream with updated one
  internals <- model$internals$replace(control_stream=control_stream)
  model <- model$replace(
    internals = internals
  )
  
  # Update source
  model <- model$update_source()
  
  return(model)
}

#' Update control stream for simulation.
#' 
#' @param model_path path to original control stream
#' @param estimate reuse estimated parameters from the model fit, default is TRUE
#' @param dataset simulation dataset, data frame
#' @param output_folder output folder to export the qualification control stream and CSV dataset
#' @param variables variables to output (note: ID, ARM, TIME, EVID, MDV, DV, AMT, CMT, DOSENO are output by default)
#' @param compartments compartment indexes to output, numeric vector
#' @return the updated campsistrans object
#' @export
updateControlStreamForSimulation <- function(model_path, estimate=TRUE, dataset, output_folder, variables, compartments=NULL) {
  model_path <-  "C:/Users/nicolas.luyckx.CALVAGONE/Desktop/Pharmpy/runPKPMPD007/runPKPMPD007_QUAL.mod"
  dataset <- read.csv("C:/Users/nicolas.luyckx.CALVAGONE/Desktop/Pharmpy/dataset.csv", header=TRUE, stringsAsFactors=FALSE)
  test_folder <- file.path("C:/Users/nicolas.luyckx.CALVAGONE/Desktop/Pharmpy/")
  output_folder <- file.path(test_folder, "Export")
  campsis <- read.campsis(file.path(test_folder, "Model"))
  estimate <- TRUE
  pharmpy <- importPharmpyPackage(UpdatedPharmpyConfig())
  variables <- "CONC"

  model <- pharmpy$modeling$read_model(model_path)
  
  # Remove ESTIMATION record and add SIMULATION
  simulationStep <- pharmpy$modeling$estimation_steps$SimulationStep()
  steps <- pharmpy$model$ExecutionSteps(list(simulationStep))
  model <- model$replace(execution_steps=steps)

  # Access the initial parameters
  params <- model$parameters
  
  # Retrieve the parameter estimates
  if (estimate) {
    results <- pharmpy$tools$read_modelfit_results(model_path)
    parameter_estimates <- results$parameter_estimates
    
    # Replace initial estimates with the parameter estimates
    params <- params$set_initial_estimates(as.list(parameter_estimates))
  }

  # Fix all parameters for simulation (not mandatory)
  fix <- as.list(params$names) # Value is not important, just the names
  names(fix) <- params$names
  params <- params$set_fix(fix)

  # Don't know why but this is needed, otherwise issues when updating initial parameters
  # with final parameter estimates
  model <- pharmpy$modeling$unconstrain_parameters(model=model, parameter_names=params$names)

  # OMEGA and SIGMA initial values to 0
  rvs <- model$random_variables$parameter_names
  rvsParams <- as.list(rep(0, length(rvs)))
  names(rvsParams) <- rvs
  params <- params$set_initial_estimates(rvsParams)
  
  # Replace in original model
  model <- model$replace(parameters=params)

  # Access control stream
  control_stream <- model$internals$control_stream
  
  # Make ETA's as covariates
  control_stream <- updateETAinNONMEMRecord(control_stream, "PRED", campsis@parameters, pharmpy)
  control_stream <- updateETAinNONMEMRecord(control_stream, "PK", campsis@parameters, pharmpy)
  control_stream <- updateETAinNONMEMRecord(control_stream, "ERROR", campsis@parameters, pharmpy)
  
  # Table
  tables <- control_stream$get_records("TABLE")
  
  # Remove all tables from control stream
  control_stream <- control_stream$remove_records(tables)
  
  # Prepare single TABLE to output
  variablesDataset <- colnames(dataset)
  defaultVariables <- c("ID", "ARM", "TIME", "EVID", "MDV", "DV", "AMT", "CMT", "DOSENO")
  defaultVariables <- defaultVariables[defaultVariables %in% variablesDataset]
  allVariables <- unique(c(defaultVariables, variables))
  tableStr <- sprintf("$TABLE %s FILE=output.tab ONEHEADER NOAPPEND NOPRINT\n", paste0(allVariables, collapse=" "))
  table <- pharmpy$model$external$nonmem$records$factory$create_record(tableStr)
  control_stream <- control_stream$insert_record(table)
  
  # Replace control stream with updated one
  model <- updateControlStream(model, control_stream)
  cat(model$code, sep = "\n")

  # OK till here
  
  # Replace INPUT record
  datainfo <- model$datainfo$create(separator=",",
                                    path=file.path(output_folder, "dataset.csv"),
                                    columns=colnames(dataset))
  datainfo <- datainfo$set_dv_column("DV")
  
  model <- model$replace(datainfo=datainfo)
  model <- model$replace(dataset=dataset)
  

  # Write model
  pharmpy$modeling$write_model(model=model, path=file.path(output_folder, "export.mod"), force=TRUE)
  
}

#' Update ETA's in NONMEM record.
#' 
#' @param control_stream pharmpy model
#' @param recordType record type to adapt
#' @param params CAMPSIS parameters
#' @param pharmpy pharmpy package
#' @importFrom reticulate import iterate py_has_attr
#' @export
#' 
updateETAinNONMEMRecord <- function(control_stream, recordType, params, pharmpy) {
  record <- control_stream$get_records(recordType)

  if (record %>% length() > 0) {
    record_ <- record[[1]]
    
    # Statements
    statements <- record_$statements["_statements"]
    replacementStatements <- list()
    
    # Replace all ETA's
    for (index in (seq_along(statements))) {
      statement <- statements[[index]]

      # Only if expression is present
      if (reticulate::py_has_attr(statement, name="expression")) {
        free_symbols <- reticulate::iterate(statement$expression$free_symbols)
        
        for (symbolIndex in seq_along(free_symbols)) {
          freeSymbol <- free_symbols[[symbolIndex]]
          symbol_chr <- as.character(freeSymbol)
          type <- getNMParameterType(symbol_chr)
          
          if (!is.null(type) && type$type=="ETA") {
            replacementSymbol <- pharmpy$basic$Expr$symbol(nameParameter(type, params))
            statement <- statement$replace(expression=replaceSymbol(statement$expression, freeSymbol, replacementSymbol))
          }
        }
        # After ETA's replacement, if left = right (e.g. ETA_CL=ETA_CL)
        # -> the current statement is omitted
        if (statement$symbol %>% as.character() != statement$expression %>% as.character()) {
          replacementStatements <- c(replacementStatements, statement)
        }
      } else {
        replacementStatements <- c(replacementStatements, statement)
      }
      
    }
    record_ <- record_$update_statements(replacementStatements)
    control_stream <- control_stream$replace_records(record, list(record_))
  }
  return(control_stream)
}

#' Set OMEGA and SIGMA initial values to 0 and fix them.
#' 
#' @param model Pharmpy model
#' @return the updated campsistrans object
#' @importFrom purrr map
#' @importFrom campsismod getNONMEMName
#' @export
#' 
omegaSigmaToZero <- function(model) {
  parset <- model$parameters
  pharmpyList <- retrieveInitialValues(parset)
  pharmpyList@list %>% purrr::map(.f=function(parameter) {
    name <- parameter %>% campsismod::getNONMEMName()
    if (as.character(class(parameter)) != "theta" && length(parset$inits[[name]]) > 0) {
      parset$inits[[name]] <<- 0
      parset$fix[[name]] <<- TRUE
    }
  })
  return(x)
}

