
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
#' @return the updated campsistrans object
#' @export
loadCtl <- function(path, estimate) {
  pharmpy <- importPharmpyPackage(UpdatedPharmpyConfig())
  model <- pharmpy$modeling$read_model(path)
  estimate <- TRUE
  
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
}

#' Update control stream for qualification
#' 
#' @param model Pharmpy model
#' @param dataset simulation dataset, data frame
#' @param campsis Campsis model, used for mapping ETAs to covariates
#' @param variables variables to output (note: ID, ARM, TIME, EVID, MDV, DV, AMT, CMT, DOSENO are output by default)
#' @param compartments compartment indexes to output, numeric vector
#' @return the updated campsistrans object
#' @export
updateCtlForQual <- function(model, dataset, campsis, variables, compartments=NULL) {

  pharmpy <- importPharmpyPackage(UpdatedPharmpyConfig())

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
  defaultVariables <- c("ID", "TIME", "MDV")
  defaultVariables <- defaultVariables[defaultVariables %in% variablesDataset]
  allVariables <- unique(c(defaultVariables, variables))
  tableStr <- sprintf("$TABLE %s FILE=output.tab ONEHEADER NOAPPEND NOPRINT\n", paste0(allVariables, collapse=" "))
  table <- pharmpy$model$external$nonmem$records$factory$create_record(tableStr)
  control_stream <- control_stream$insert_record(table)
  
  # Replace control stream with updated one
  model <- updateCtl(model, control_stream)
  # cat(model$code, sep = "\n")

  return(model)
}

#' Write control stream to files.
#' 
#' @param model Pharmpy model
#' @param path where to write the control stream
#' @param force if TRUE, overwrite existing file
#' @return the path to the written control stream
#' @export
writeCtl <- function(model, path, force) {
  pharmpy <- importPharmpyPackage(UpdatedPharmpyConfig())
  dir <- dirname(path)
  
  # # Remove any DROP directive in INPUT
  # control_stream <- model$internals$control_stream
  # inputs <- control_stream$get_records("INPUT")
  # input <- inputs[[1]]
  # inputStr <- "$INPUT"
  # for (option in input$all_options) {
  #   inputStr <- inputStr %>% append(option$key)
  # }
  # updatedInput <- pharmpy$model$external$nonmem$records$factory$create_record(paste(inputStr, collapse=" "))
  # control_stream <- control_stream$remove_records(inputs)
  # control_stream <- control_stream$insert_record(input)
  # model <- updateCtl(model, control_stream)
  
  # Update dataset path
  datainfo <- model$datainfo$create(separator=",",
                                    path=file.path(dir, "dataset.csv"),
                                    columns=model$dataset %>% colnames())
  datainfo <- datainfo$set_dv_column("DV")
  datainfo <- datainfo$set_id_column("ID")
  model <- model$replace(datainfo=datainfo)

  # Last call to update source
  model <- model$update_source()
  
  pharmpy$modeling$write_model(model=model, path=path, force=force)
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
        print(symbol_chr)
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

