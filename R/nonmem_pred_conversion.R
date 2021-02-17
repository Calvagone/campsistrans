
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

#' Export PMX model to NONMEM control stream for population simulation (POP).
#' If NONMEM results are provided, they will replace the previous original values in the control stream.
#' OMEGA and SIGMA initial values will then be fixed and set to 0.
#' ETA arrays in control stream will be replaced by ETA covariates (e.g. ETA(1) -> ETA_1)
#' 
#' @param model PMX model
#' @return the updated PMX model
#' @importFrom dplyr filter pull
#' @export
toNONMEMPop <- function(model) {
  
  # First update all initial estimates of a model from its own results 
  model$model$update_inits()
  
  # OMEGA and SIGMA initial values to 0
  model <- omegaSigmaToZero(model)
  
  # Pharmy model
  pyModel <- model$model
  ctl <- pyModel$control_stream
  
  updateETAinNONMEMRecord(ctl, "PK")
  updateETAinNONMEMRecord(ctl, "ERROR")
  
  return(model)
}

#' Update ETA's in NONMEM record.
#' 
#' @param ctl Pharmpy control stream
#' @param record_type record type, e.g. "PK"
#' @importFrom reticulate import iterate py_has_attr
#' @export
updateETAinNONMEMRecord <- function(ctl, record_type) {
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
          replacementSymbol <- sympy$symbols(nameParameter(type, model$params))
          statement$expression <- replaceSymbol(statement$expression, freeSymbol, replacementSymbol)
        }
      }
    }
    replacementStatements <- c(replacementStatements, statement)
  }
  
  record$statements <- replacementStatements
}

#' Set OMEGA and SIGMA initial values to 0 and fix them.
#' 
#' @param model PMX model
#' @return the updated PMX model
#' @importFrom dplyr filter pull
#' @export
omegaSigmaToZero <- function(model) {
  parset <- model$model$parameters
  
  subTable <- model$params$table %>% dplyr::filter(type == "OMEGA" | type == "SIGMA") 
  names <- subTable %>% dplyr::pull(nm_name)
  
  # Set OMEGA and SIGMA initial values to 0
  # And fix these parameters
  a_ply(names, .margins=1, .fun=function(name) {
    if (length(parset$inits[[name]]) > 0) {
      parset$inits[[name]] <- 0
      parset$fix[[name]] <- TRUE
    }
  })
  
  return(model)
}
