
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

updateCtl <- function(model, control_stream) {
  # Replace control stream with updated one
  internals <- model$internals$replace(control_stream=control_stream)
  model <- model$replace(
    internals = internals
  )
  
  # Update source
  model <- model$update_source()
  
  return(model)
}

#' Load control stream with Pharmpy.
#' 
#' @param path path to original control stream
#' @param estimate reuse estimated parameters from the model fit, default is TRUE
#' @return the pharmpy model
#' @export
loadCtl <- function(path, estimate) {
  pharmpy <- importPharmpyPackage(UpdatedPharmpyConfig())
  model <- pharmpy$modeling$read_model(path)
  
  # Retrieve the parameter estimates
  if (estimate) {
    results <- pharmpy$tools$read_modelfit_results(path)
    parameter_estimates <- results$parameter_estimates
    
    # Replace initial estimates with the parameter estimates
    parameters <- model$parameters
    parameters <- parameters$set_initial_estimates(as.list(parameter_estimates))
    
    # Replace parameters in original model
    model <- model$replace(parameters=parameters)
  }
  
  return(model)
}

#' Prepare simulation control stream.
#' 
#' @param campsistrans Pharmpy model
#' @param dataset simulation dataset, data frame
#' @param campsis Campsis model, used for mapping ETAs to covariates
#' @param variables variables to output
#' @return the updated Campsistrans object
#' @export
prepareSimulationCtl <- function(campsistrans, dataset, variables) {
  pharmpy <- importPharmpyPackage(UpdatedPharmpyConfig())
  model <- campsistrans@model
  campsis <- campsistrans@campsis

  # Remove ESTIMATION record and add SIMULATION
  simulationStep <- pharmpy$modeling$estimation_steps$SimulationStep()
  steps <- pharmpy$model$ExecutionSteps(list(simulationStep))
  model <- model$replace(execution_steps=steps)
  
  # Update ETAs
  updatedStatements <- setEtasAsCovariates(statements=model$statements, params=campsis@parameters, pharmpy=pharmpy)
  
  # Replace statements and dataset
  model <- model$replace(statements=updatedStatements, dataset=dataset)

  # Fix all parameters for simulation (not mandatory)
  parameters <- model$parameters
  fix <- as.list(parameters$names) # Value is not important, just the names
  names(fix) <- parameters$names
  parameters <- parameters$set_fix(fix)

  # Don't know why but this is needed, otherwise issues when updating initial parameters
  # with final parameter estimates
  model <- pharmpy$modeling$unconstrain_parameters(model=model, parameter_names=parameters$names)

  # OMEGA and SIGMA initial values to 0
  rvs <- model$random_variables$parameter_names
  rvsParams <- as.list(rep(0, length(rvs)))
  names(rvsParams) <- rvs
  parameters <- parameters$set_initial_estimates(rvsParams)
  
  # Replace parameters in original model
  model <- model$replace(parameters=parameters)
  
  # Access control stream
  control_stream <- model$internals$control_stream
  
  # Table
  tables <- control_stream$get_records("TABLE")
  
  # Remove all tables from control stream
  control_stream <- control_stream$remove_records(tables)
  
  # Prepare single TABLE to output
  variablesDataset <- colnames(dataset)
  defaultVariables <- c("ID", "TIME", "EVID")
  defaultVariables <- defaultVariables[defaultVariables %in% variablesDataset]
  allVariables <- unique(c(defaultVariables, variables))
  tableStr <- sprintf("$TABLE %s FILE=output.tab ONEHEADER NOAPPEND NOPRINT\n", paste0(allVariables, collapse=" "))
  table <- pharmpy$model$external$nonmem$records$factory$create_record(tableStr)
  control_stream <- control_stream$insert_record(table)
  
  # Replace control stream with updated one
  model <- updateCtl(model, control_stream)

  # Update Campsistrans
  campsistrans@model <- model
  return(campsistrans)
}

#' Write simulation control stream.
#' 
#' @param campsistrans Campsistrans object
#' @param folder the path to the qualification folder
#' @return the path to the qualification folder
#' @export
writeSimulationCtl <- function(campsistrans, folder) {
  model <- campsistrans@model
  pharmpy <- importPharmpyPackage(UpdatedPharmpyConfig())

  # Create dir if not existing
  if (!dir.exists(folder)) {
    dir.create(folder, recursive=TRUE)
  }
  
  # Clear directory
  do.call(file.remove, list(list.files(folder, full.names=TRUE)))
  
  # Write NONMEM dataset
  write.csv(model$dataset, file=file.path(folder, "dataset.csv"), quote=FALSE, row.names=FALSE)
  
  # Update dataset path
  datainfo <- model$datainfo$create(separator=",",
                                    path=file.path(folder, "dataset.csv"),
                                    columns=model$dataset %>% colnames())
  datainfo <- datainfo$set_dv_column("DV")
  datainfo <- datainfo$set_id_column("ID")
  model <- model$replace(datainfo=datainfo)

  # Last call to update source
  model <- model$update_source()
  
  pharmpy$modeling$write_model(model=model, path=file.path(folder, "model.mod"), force=TRUE)
  
  return(folder)
}


#' Execute NONMEM. PsN will be called automatically by R. 
#' Prepared control stream 'model.mod' is executed automatically and NONMEM results
#' are returned in the form of a data frame.
#' 
#' @param folder qualification folder where the control stream is
#' @param reexecuteNONMEM force re-execute NONMEM if results already exist
#' @param ctl_name control stream name, default is 'model.mod'
#' @export
executeNONMEM <- function(folder, reexecuteNONMEM=T, ctl_name="model.mod") {
  tabFile <- paste0(folder, "/", "output.tab")
  if (!file.exists(tabFile) || reexecuteNONMEM) {
    system("cmd.exe", input=paste0("cd ","\"", folder, "\"", " & ", "execute ", ctl_name))
    unlink(paste0(folder, "/", "modelfit_dir1"), recursive=TRUE)
  }
  nonmem <- read.nonmem(tabFile)[[1]] %>% as.data.frame()
  return(nonmem)
}

#' Prepare simulation control stream and execute it with NONMEM.
#' 
#' @param campsistrans campsistrans object
#' @param dataset simulation dataset, data frame
#' @param variables variables to output (note: ID, ARM, TIME, EVID, MDV, DV, AMT, CMT, DOSENO are output by default)
#' @param folder where to execute the simulation control stream
#' @param reexecuteNONMEM force re-execute NONMEM
#' @return a data frame with NONMEM results
#' @export
#' 
executeSimulationCtl <- function(campsistrans, dataset, variables, folder, reexecuteNONMEM=T) {
  if (reexecuteNONMEM) {
    campsistrans_ <- prepareSimulationCtl(campsistrans=campsistrans, dataset=dataset, variables=variables)
    writeSimulationCtl(campsistrans=campsistrans_, folder=folder)
  }
  
  results <- executeNONMEM(folder=folder, reexecuteNONMEM=reexecuteNONMEM)
  return(results)
}

#' Set ETAs as covariates in Pharmpy model.
#' 
#' @param statements pharmpy model
#' @param params Campsis parameters
#' @param pharmpy pharmpy package
#' @importFrom reticulate iterate py_has_attr
#' @export
#' 
setEtasAsCovariates <- function(statements, params, pharmpy) {
  replacementStatements <- list()

  # Replace all ETA's
  for (index in (seq_along(statements) - 1)) {
    statement <- statements[[index]]
    
    # Only if expression is present
    if (reticulate::py_has_attr(statement, name="expression")) {
      free_symbols <- reticulate::iterate(statement$expression$free_symbols)
      
      for (symbolIndex in seq_along(free_symbols)) {
        freeSymbol <- free_symbols[[symbolIndex]]
        symbol_chr <- as.character(freeSymbol)
        # print(symbol_chr)
        type <- getPharmpyParameterType(symbol_chr)
        
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
  
  statements_ <- pharmpy$model$Statements(replacementStatements)
  return(statements_)
}

