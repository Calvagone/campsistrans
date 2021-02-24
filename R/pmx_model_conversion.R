
#' PMXtran model conversion to PMX model.
#' 
#' @param pmxtran PMXtran model
#' @return PMX model
#' @importFrom assertthat assert_that
#' @export
toPmxModel <- function(pmxtran) {
  assertthat::assert_that(inherits(pmxtran, "pmx_tran"),
                          msg="pmxtran is not a PMXtran object")
  statements <- reticulate::iterate(pmxtran$model$statements)
  parameters <- pmxtran$params
  code <- NULL
  
  recordType <- "PK"
  record <- pmxtran$model$control_stream$get_records(recordType)
  code <- c(code, recordToPmxModel(record, recordType, parameters))
  
  recordType <- "PRED"
  record <- pmxtran$model$control_stream$get_records(recordType)
  code <- c(code, recordToPmxModel(record, recordType, parameters))
  
  recordType <- "DES"
  record <- pmxtran$model$control_stream$get_records(recordType)
  if (length(record)==0) {
    system <- statements %>%
      purrr::keep(~("pharmpy.statements.CompartmentalSystem" %in% class(.x)))
    if (length(system) > 0) {
      tmp_code <- compartmentSystemToPmxModel(system[[1]])
      code <- c(code, tmp_code)
    }
  } else {
    code <- c(code, recordToPmxModel(record, recordType, parameters))
  }
  
  recordType <- "ERROR"
  record <- pmxtran$model$control_stream$get_records(recordType)
  code <- c(code, recordToPmxModel(record, recordType, parameters))
  
  retValue <- new("pmx_model", code=code, parameters=pmxtran$params)
  
  return(retValue)
}

#' SymPy statement conversion to PMX model.
#' 
#' @param statement SymPy statement
#' @param parameters parameters
#' @return C code
#' @importFrom reticulate iterate
#' @export
statementToPmxModel <- function(statement, parameters) {
  
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
    return(piecewiseToPmxModel(symbol, expression))
  
  } else if (isODE){
    cmtNumber <- extractValueInParentheses(symbol_chr)
    return(paste0("d/dt(", "A_", cmtNumber, ")=", printSymPy(expression)))

  } else {
    return(paste0(symbol_chr, "=", printSymPy(expression)))
  }
}

#' SymPy piecewise conversion to PMX model.
#' 
#' @param symbol SymPy statement symbol
#' @param piecewise SymPy piecewise
#' @return C code
#' @export
piecewiseToPmxModel <- function(symbol, piecewise) {
  symbol_chr <- as.character(symbol)
  
  exprCondPair <- piecewise$args[[1]]
  
  if (! ("sympy.functions.elementary.piecewise.ExprCondPair" %in% class(exprCondPair))) {
    stop(paste("Class can only be", class(exprCondPair), "for now..."))  
  } 
  
  expression <- exprCondPair$args[[1]]
  condition <- exprCondPair$args[[2]]
  
  return(paste0("if (", printSymPy(condition), ") ", symbol_chr, "=", printSymPy(expression)))
}

#' NONMEM record (pharmpy) to PMX model.
#' 
#' @param records one or more NONMEM record
#' @param recordType type of record
#' @return C code
#' @export
recordToPmxModel <- function(records, recordType, parameters) {
  if (length(records)==0) {
    return(NULL)
  }

  code <- paste0("[", recordType, "]")
  
  for (record in records) {
    if (! ("pharmpy.plugins.nonmem.records.code_record.CodeRecord" %in% class(record))) {
      stop("Not a DES record")  
    }
    statements <- record$statements
    
    # Retrieve all equations
    for (index in (seq_along(statements) - 1)) {
      statement <- statements[[index]]
      code <- c(code, statementToPmxModel(statement, parameters))
    }
  }
  
  return(code)
}

#' Pharmpy compartment system conversion to PMX model.
#' 
#' @param system Pharmpy compartment system
#' @return C code
#' @export
compartmentSystemToPmxModel <- function(system) {
  
  explicitOdes <- system$to_explicit_odes()
  odes <- explicitOdes[[1]]
  
  cptNames <- NULL
  code <- "[DES]"
  
  # Useful link
  # https://github.com/sympy/sympy/blob/master/sympy/core/function.py
  # lhs$free_symbols
  # lhs$is_Derivative
  # lhs$variables
  # lhs['_wrt_variables']
  # lhs$expr_free_symbols
  # lhs$canonical_variables
  # lhs$diff
  # freeSymbols <- ode$free_symbols
  
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
    
    code <- c(code, paste0("d/dt(", cptName, ")=", equation))
  }
  
  return(code)
}