
#' PMXtran model conversion to PMX model.
#' 
#' @param pmxtran PMXtran model
#' @return PMX model
#' @importFrom assertthat assert_that
#' @importFrom pmxmod getName CodeRecords DesRecord ErrorRecord PkRecord PredRecord
#' @export
toPmxModel <- function(pmxtran) {
  assertthat::assert_that(inherits(pmxtran, "pmxtran"),
                          msg="pmxtran is not a PMXtran object")
  statements <- reticulate::iterate(pmxtran$model$statements)
  parameters <- pmxtran$params
  
  model <- CodeRecords()
  
  emptyRecord <- PkRecord()
  record <- pmxtran$model$control_stream$get_records(emptyRecord %>% pmxmod::getName())
  model <- addRecordToPmxModel(model, record, emptyRecord, parameters)

  emptyRecord <- PredRecord()
  record <- pmxtran$model$control_stream$get_records(emptyRecord %>% pmxmod::getName())
  model <- addRecordToPmxModel(model, record, emptyRecord, parameters)
  
  emptyRecord <- DesRecord()
  record <- pmxtran$model$control_stream$get_records(emptyRecord %>% pmxmod::getName())
  if (length(record)==0) {
    system <- statements %>%
      purrr::keep(~("pharmpy.statements.CompartmentalSystem" %in% class(.x)))
    if (length(system) > 0) {
      model@list <- c(model@list, compartmentSystemToPmxModel(system[[1]]))
    }
  } else {
    model <- addRecordToPmxModel(model, record, emptyRecord, parameters)
  }
  
  emptyRecord <- ErrorRecord()
  record <- pmxtran$model$control_stream$get_records(emptyRecord %>% pmxmod::getName())
  model <- addRecordToPmxModel(model, record, emptyRecord, parameters)
  
  retValue <- new("pmx_model", model=model, parameters=pmxtran$params)
  
  return(retValue)
}

addRecordToPmxModel <- function(model, record, emptyRecord, parameters) {
  if (length(record) > 0) {
    model@list <- c(model@list, recordToPmxModel(record, emptyRecord, parameters))
  }
  return(model)
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
#' @param emptyRecord empty code record, already instantiated with the right type
#' @param parameters parameters
#' @return a PMX record
#' @export
recordToPmxModel <- function(records, emptyRecord, parameters) {
  code <- NULL
  
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
  
  # Filling the empty record
  record <- emptyRecord
  record@code <- code
  return(record)
}

#' Pharmpy compartment system conversion to PMX model.
#' 
#' @param system Pharmpy compartment system
#' @importFrom pmxmod DesRecord
#' @return DES record (PMX domain)
#' @export
compartmentSystemToPmxModel <- function(system) {
  
  explicitOdes <- system$to_explicit_odes()
  odes <- explicitOdes[[1]]
  
  cptNames <- NULL
  code <- NULL
  
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
  
  # Add F equation
  central <- system$find_central()
  code <- c(code, paste0("F=", "A_", central$name, "/S", central$index))
  
  return(DesRecord(code=code))
}