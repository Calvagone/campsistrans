
#' Export PMX model to NONMEM control stream for typical profile simulation (PRED).
#' If NONMEM results are provided, they will replace the previous original values in the control stream.
#' OMEGA and SIGMA initial values will then be fixed and set to 0.
#' 
#' @param model PMX model
#' @return the updated PMX model
#' @export
toNONMEMPred <- function(model) {
  
  # First update all initial estimates of a model from its own results 
  model$model$update_inits()
  
  # OMEGA and SIGMA initial values to 0
  model <- omegaSigmaToZero(model)
  
  return(model)
}

#' Export PMXtran object to NONMEM control stream for population simulation (POP).
#' If NONMEM results are provided, they will replace the previous original values in the control stream.
#' OMEGA and SIGMA initial values will then be fixed and set to 0.
#' ETA arrays in control stream will be replaced by ETA covariates (e.g. ETA(1) -> ETA_1)
#' 
#' @param pmxtran PMXtran object
#' @return the updated PMXtran object
#' @importFrom dplyr filter pull
#' @export
toNONMEMPop <- function(pmxtran) {
  
  # First update all initial estimates of a model from its own results 
  pmxtran$model$update_inits()
  
  # OMEGA and SIGMA initial values to 0
  pmxtran <- omegaSigmaToZero(pmxtran)
  
  # Update ETA's (as covariates)
  pmxtran <- updateETAinNONMEMRecord(pmxtran, "PK")
  pmxtran <- updateETAinNONMEMRecord(pmxtran, "ERROR")
  
  return(pmxtran)
}

#' Update ETA's in NONMEM record.
#' 
#' @param pmxtran PMXtran object
#' @param record_type record type, e.g. "PK"
#' @importFrom reticulate import iterate py_has_attr
#' @export
updateETAinNONMEMRecord <- function(pmxtran, record_type) {
  pyModel <- pmxtran$model
  ctl <- pyModel$control_stream
  record <- ctl$get_records(record_type)[[1]]
  
  # Statements
  statements <- record$statements
  sympy <- reticulate::import("sympy")
  replacementStatements <- list()
  
  # Replace all ETA's
  for (index in (seq_along(statements)-1)) {
    statement <- statements[[index]]
    
    # Only if expression is present
    if (reticulate::py_has_attr(statement, name="expression")) {
      free_symbols <- reticulate::iterate(statement$expression$free_symbols)
      
      for (symbolIndex in seq_along(free_symbols)) {
        freeSymbol <- free_symbols[[symbolIndex]]
        symbol_chr <- as.character(freeSymbol)
        type <- getNMParameterType(symbol_chr)
        
        if (!is.null(type) && type$type=="ETA") {
          replacementSymbol <- sympy$symbols(nameParameter(type, pmxtran$params))
          statement$expression <- replaceSymbol(statement$expression, freeSymbol, replacementSymbol)
        }
      }
    }
    replacementStatements <- c(replacementStatements, statement)
  }
  
  record$statements <- replacementStatements
  return(pmxtran)
}

#' Set OMEGA and SIGMA initial values to 0 and fix them.
#' 
#' @param model PMX model
#' @return the updated PMX model
#' @importFrom dplyr filter pull
#' @export
omegaSigmaToZero <- function(pmxtran) {
  parset <- pmxtran$model$parameters
  pharmpyList <- initialValues(parset)
  pharmpyList@list %>% purrr::map(.f=function(parameter) {
    name <- parameter %>% pmxmod::getNONMEMName()
    if (length(parset$inits[[name]]) > 0) {
      parset$inits[[name]] <<- 0
      parset$fix[[name]] <<- TRUE
    }
  })
  
  return(pmxtran)
}
