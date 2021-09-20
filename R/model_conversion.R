
#_______________________________________________________________________________
#----                               export                                  ----
#_______________________________________________________________________________

#' @importFrom reticulate iterate
#' @importFrom campsismod autoDetectNONMEM updateCompartments
#' 
setMethod("export", signature = c("campsistrans", "character"), definition = function(object, dest, ...) {
  # pmxmod is accepted
  if (!(dest %in% c("campsis", "pmxmod"))) {
    stop("dest must be 'campsis'")
  }
  
  pharmpyModel <- object@model[[1]]
  statements <- reticulate::iterate(pharmpyModel$statements)
  parameters <- object@params
  
  model <- CodeRecords()
  
  emptyRecord <- MainRecord()
  record <- pharmpyModel$control_stream$get_records("PK")
  model <- addconvertRecord(model, record, emptyRecord, parameters)
  
  emptyRecord <- MainRecord()
  record <- pharmpyModel$control_stream$get_records("PRED")
  model <- addconvertRecord(model, record, emptyRecord, parameters)
  
  emptyRecord <- OdeRecord()
  record <- pharmpyModel$control_stream$get_records("DES")
  if (length(record)==0) {
    system <- statements %>%
      purrr::keep(~("pharmpy.statements.CompartmentalSystem" %in% class(.x)))
    if (length(system) > 0) {
      model@list <- c(model@list, convertCompartmentSystem(system[[1]]))
    }
  } else {
    model <- addconvertRecord(model, record, emptyRecord, parameters)
  }
  
  emptyRecord <- ErrorRecord()
  record <- pharmpyModel$control_stream$get_records("ERROR")
  model <- addconvertRecord(model, record, emptyRecord, parameters)
  
  # Variance-covariance conversion (NONMEM -> CAMPSIS)
  parameters@varcov <- object@varcov %>% convertVarcov(parameters)
  retValue <- new("campsis_model", model=model, parameters=parameters)
  
  # Update compartments list before returning the CAMPSIS model
  retValue <- retValue %>% campsismod::updateCompartments()
  
  # Auto-detect compartment properties from NONMEM special variables
  retValue <- retValue %>% campsismod::autoDetectNONMEM()
  
  return(retValue)
})

#' Add record to the specified CAMPSIS model.
#' 
#' @param model specified CAMPSIS model
#' @param record record to add
#' @param emptyRecord empty code record, already instantiated with the right type
#' @param parameters parameters
#' @return updated CAMPSIS model
addconvertRecord <- function(model, record, emptyRecord, parameters) {
  if (length(record) > 0) {
    model <- model %>% add(convertRecord(record, emptyRecord, parameters))
  }
  return(model)
}

#' SymPy statement conversion to CAMPSIS model.
#' 
#' @param statement SymPy statement
#' @param parameters parameters
#' @return C code
#' @importFrom reticulate iterate
#' @export
convertStatement <- function(statement, parameters) {
  
  symbol <- statement$symbol
  symbol_chr <- as.character(symbol)
  expression <- statement$expression
  
  free_symbols <- reticulate::iterate(expression$free_symbols)
  
  for (symbolIndex in seq_along(free_symbols)) {
    freeSymbol <- free_symbols[[symbolIndex]]
    expression <- replaceSymbolAuto(expression, freeSymbol, parameters)
  }
  
  dadtPattern <- "^DADT\\(.*\\)$"
  isODE <- grepl(pattern=dadtPattern, x=symbol_chr, ignore.case=TRUE)
  
  if ("sympy.functions.elementary.piecewise.Piecewise" %in% class(expression)) {
    return(convertPiecewise(symbol, expression))
  
  } else if (isODE){
    cmtNumber <- extractValueInParentheses(symbol_chr)
    return(Ode(paste0("A_", cmtNumber), printSymPy(expression)))

  } else {
    return(Equation(symbol_chr, printSymPy(expression)))
  }
}

#' SymPy piecewise conversion to CAMPSIS model.
#' 
#' @param symbol SymPy statement symbol
#' @param piecewise SymPy piecewise
#' @return C code
#' @export
convertPiecewise <- function(symbol, piecewise) {
  symbol_chr <- as.character(symbol)
  
  exprCondPair <- piecewise$args[[1]]
  
  if (! ("sympy.functions.elementary.piecewise.ExprCondPair" %in% class(exprCondPair))) {
    stop(paste("Class can only be", class(exprCondPair), "for now..."))  
  } 
  
  expression <- exprCondPair$args[[1]]
  condition <- exprCondPair$args[[2]]
  
  return(IfStatement(printSymPy(condition), Equation(symbol_chr, printSymPy(expression))))
}

#' NONMEM record (pharmpy) to CAMPSIS model.
#' 
#' @param records one or more NONMEM record
#' @param emptyRecord empty code record, already instantiated with the right type
#' @param parameters parameters
#' @return a CAMPSIS record
#' @export
convertRecord <- function(records, emptyRecord, parameters) {
  retValue <- emptyRecord
  
  for (record in records) {
    if (! ("pharmpy.plugins.nonmem.records.code_record.CodeRecord" %in% class(record))) {
      stop("Not a DES record")  
    }
    # Retrieve statements list in R
    statements <- record$statements["_statements"]
    
    # Retrieve all equations
    for (index in (seq_along(statements))) {
      statement <- statements[[index]]
      retValue <- retValue %>% add(convertStatement(statement, parameters))
    }
  }
  return(retValue)
}

#' Pharmpy compartment system conversion to PMX model.
#' 
#' @param system Pharmpy compartment system
#' @importFrom campsismod OdeRecord
#' @return ODE record (CAMPSIS domain)
#' @export
convertCompartmentSystem <- function(system) {
  
  explicitOdes <- system$to_explicit_odes()
  odes <- explicitOdes[[1]]
  
  cptNames <- NULL
  odeRecord <- OdeRecord()
  
  # Collect all compartment names first
  for (index in seq_along(odes)) {
    ode <- odes[[index]]
    cptNames <- c(cptNames, retrieveCompartmentName(ode$lhs))
  }
  
  # Retrieve all equations
  for (index in seq_along(odes)) {
    ode <- odes[[index]]
    cptName <- retrieveCompartmentName(ode$lhs)
    
    equation <- as.character(ode$rhs)
    for (name in cptNames) {
      equation <- gsub(paste0(name, "\\(t\\)"), name, equation)
    }
    
    odeRecord <- odeRecord %>% add(Ode(cptName, equation))
  }
  
  # Add F equation
  central <- system$find_central()
  odeRecord <- odeRecord %>% add(Equation("F", paste0("A_", central$name, "/S", central$index)))
  return(odeRecord)
}
