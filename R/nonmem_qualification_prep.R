
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
#' @importFrom dplyr filter pull
#' @importFrom campsismod getNameInModel getByIndex maxIndex Omega write
#' @importFrom tibble add_column as_tibble
#' @export
prepareNONMEMFiles <- function(x, dataset, variables, compartments=NULL, outputFolder) {
  
  # Load module Pharmpy
  pharmpy <- reticulate::import("pharmpy")
  
  # Standardise NONMEM dataset
  dataset <- standardiseDataset(dataset)
  
  # Pharmpy model
  pharmpyModel <- x@model[[1]]
  
  # First update all initial estimates of a model from its own results
  if (x@estimate) {
    pharmpyModel <- pharmpy$modeling$update_inits(pharmpyModel)
  }

  # OMEGA and SIGMA initial values to 0
  x <- omegaSigmaToZero(x)
  
  # Update source
  # This will update the 'model' with the new values from the generic model
  pharmpyModel$update_source()
  
  # Replace INPUT record
  oldInput <- pharmpyModel$control_stream$get_records("INPUT")
  if (length(oldInput) == 0) {
    stop("No INPUT record available")
  }
  colnamesDatasetStr <- paste0(colnames(dataset), collapse=" ")
  input <- pharmpy$plugins$nonmem$nmtran_parser$create_record(paste0("$INPUT ", colnamesDatasetStr , "\n"))
  pharmpyModel$control_stream$replace_records(oldInput, list(input))
  
  # Replace DATA record
  oldData <- pharmpyModel$control_stream$get_records("DATA")
  if (length(oldData) == 0) {
    stop("No DATA record available")
  }
  data <- pharmpy$plugins$nonmem$nmtran_parser$create_record("$DATA dataset.csv IGNORE=I\n")
  pharmpyModel$control_stream$replace_records(oldData, list(data))
  
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
  
  # Remove ESTIMATION record
  estimation <- pharmpyModel$control_stream$get_records("ESTIMATION")
  pharmpyModel$control_stream$remove_records(estimation)
  
  # Remove COVARIANCE record ($SIMULATE: CAN'T USE ONLYSIMULATION WITH $EST, $COV, $NONP)
  covariance <- pharmpyModel$control_stream$get_records("COVARIANCE")
  pharmpyModel$control_stream$remove_records(covariance)
  
  # Remove SIMULATION record
  simulation <- pharmpyModel$control_stream$get_records("SIMULATION")
  pharmpyModel$control_stream$remove_records(simulation)
  
  # Remove TABLE record
  table <- pharmpyModel$control_stream$get_records("TABLE")
  pharmpyModel$control_stream$remove_records(table)
  
  # Create SIMULATION record
  simulation <- pharmpyModel$control_stream$append_record("\n$SIMULATION (1234) ONLYSIM NSUB=1\n")
  
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
  
  # Replace control stream and update source
  #pharmpyModel$control_stream <- ctl
  #pharmpyModel$update_source()
  
  # Write NONMEM dataset
  write.csv(dataset, file=paste0(outputFolder, "/", "dataset.csv"), quote=FALSE, row.names=FALSE)
  
  # NOT WORKING
  # pharmpyModel$update_source() # NOT A GOOD IDEA (restore initial ETA's)
  # pharmpy$modeling$write_model(model=pharmpyModel, path=paste0(outputFolder, "/", "model.mod"), force=TRUE) # NOT WORKING, SKIP NEW ETA NAMES
  
  # Write qualification control stream
  ctl <- as.character(pharmpyModel$control_stream)
  fileConn <- file(paste0(outputFolder, "/", "model.mod"))
  writeLines(text=ctl, fileConn)
  close(fileConn)
  
  return(paste0(outputFolder, "/", "model.mod"))
}

#' Update ETA's in NONMEM record.
#' 
#' @param ctl NONMEM control stream
#' @param recordType record type to adapt
#' @param params CAMPSIS parameters
#' @importFrom reticulate import iterate py_has_attr
#' @export
#' 
updateETAinNONMEMRecord <- function(pharmpyModel, recordType, params) {
  record <- pharmpyModel$control_stream$get_records(recordType)

  if (record %>% length() > 0) {
    record_ <- record[[1]]
    
    # Statements
    statements <- record_$statements["_statements"]
    sympy <- reticulate::import("sympy")
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
            replacementSymbol <- sympy$symbols(nameParameter(type, params))
            statement$expression <- replaceSymbol(statement$expression, freeSymbol, replacementSymbol)
          }
        }
      }
      replacementStatements <- c(replacementStatements, statement)
    }
    record_$statements <- replacementStatements
    pharmpyModel$control_stream$replace_records(record, list(record_))
  }
  return(pharmpyModel)
}

#' Set OMEGA and SIGMA initial values to 0 and fix them.
#' 
#' @param x campsistrans object
#' @return the updated campsistrans object
#' @importFrom purrr map
#' @importFrom campsismod getNONMEMName
#' @export
#' 
omegaSigmaToZero <- function(x) {
  parset <- x@model[[1]]$parameters
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
