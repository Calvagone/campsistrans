
#_______________________________________________________________________________
#----                               export                                  ----
#_______________________________________________________________________________

#' Export CAMPSIS model.
#' 
#' @param pharmpyModel Pharmpy model
#' @param parameters parameters (before auto mapping)
#' @param varcov varcov imported with Pharmpy
#' @param mapping initial mapping object
#' @return the CAMPSIS model
#' @importFrom reticulate iterate
#' @importFrom campsismod autoDetectNONMEM updateCompartments
#' 
exportCampsisModel <- function(pharmpyModel, parameters, varcov, mapping) {
  statements <- reticulate::iterate(pharmpyModel$statements)

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
  
  # Instantiate initial CAMPSIS model
  retValue <- new("campsis_model", model=model, parameters=parameters)
  
  # Update compartments list before returning the CAMPSIS model
  retValue <- retValue %>% campsismod::updateCompartments()
  
  # Auto-detect compartment properties from NONMEM special variables
  retValue <- retValue %>% campsismod::autoDetectNONMEM()
  
  # Move initial conditions
  retValue <- retValue %>% moveInitialConditions()
  
  # Auto-rename parameters
  retValue <- retValue %>% autoRenameParameters(mapping=mapping)
  
  # Store variance-covariance matrix according to the new parameters
  retValue@parameters@varcov <- varcov %>% convertVarcov(retValue@parameters)
  
  return(retValue)
}

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
  explicitSystem <- system$to_explicit_system()
  odes <- explicitSystem$odes
  
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
  central <- system$central_compartment
  centralIndex <- which(system$names==central$name)
  odeRecord <- odeRecord %>% add(Equation("F", paste0("A_", central$name, "/S", centralIndex)))
  return(odeRecord)
}

#' Move initial conditions from MAIN to INIT section.
#' 
#' @param model CAMPSIS model
#' @importFrom campsismod Equation InitialCondition
#' @return updated CAMPSIS model
moveInitialConditions <- function(model) {
  for (compartment in model@compartments@list) {
    index <- compartment@index
    initialValueNM <- Equation(paste0("A_0(", index, ")"))
    equation <- model %>% find(initialValueNM)
    if (!is.null(equation)) {
      model <- model %>% add(InitialCondition(compartment=index, rhs=equation@rhs))
      model <- model %>% delete(equation)
    }
  }
  return(model)
}

#' Remove the piecewise statements added by Pharmpy.
#' 
#' @param model CAMPSIS model
#' @return updated CAMPSIS model
#' @importFrom campsismod replace
#' @export
removePiecewiseStatements <- function(model) {
  compartments <- model@compartments
  for (compartment in compartments@list) {
    ode <- model %>% find(Ode(paste0("A_", compartment@name)))
    ode@rhs <- gsub(pattern=" \\+ Piecewise\\(.*", replacement="", ode@rhs)
    model <- model %>% campsismod::replace(ode)
  }
  return(model)
}

