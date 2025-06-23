
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
#' @export
#' 
exportCampsisModel <- function(pharmpyModel, parameters, varcov, mapping) {
  statements <- reticulate::iterate(pharmpyModel$statements)
  records <- CodeRecords()
  
  emptyRecord <- MainRecord()
  record <- pharmpyModel$internals$control_stream$get_pred_pk_record()
  records <- addconvertRecord(records, record, emptyRecord, parameters)
  
  emptyRecord <- OdeRecord()
  record <- pharmpyModel$internals$control_stream$get_des_record()
  if (length(record)==0) {
    system <- statements %>%
      purrr::keep(~("pharmpy.model.statements.CompartmentalSystem" %in% class(.x)))
    if (length(system) > 0) {
      records@list <- c(records@list, convertCompartmentSystem(pharmpyModel))
    }
  } else {
    records <- addconvertRecord(records, record, emptyRecord, parameters)
  }
  
  emptyRecord <- ErrorRecord()
  record <- pharmpyModel$internals$control_stream$get_error_record()
  records <- addconvertRecord(records, record, emptyRecord, parameters)
  
  # Instantiate initial CAMPSIS model
  retValue <- new("campsis_model", model=records, parameters=parameters)
  
  # Update compartments list before returning the CAMPSIS model
  retValue <- retValue %>% campsismod::updateCompartments()
  
  # Auto-detect compartment properties from NONMEM special variables
  retValue <- retValue %>% campsismod::autoDetectNONMEM()
  
  # Move initial conditions
  retValue <- retValue %>% moveInitialConditions()
  
  # Auto-rename parameters
  if (mapping$auto) {
    retValue <- retValue %>%
      autoRenameParameters()
  }

  # Get rid of useless equations
  retValue <- retValue %>% removeUselessEquations()
  
  # Store variance-covariance matrix according to the new parameters
  retValue@parameters@varcov <- varcov %>% convertVarcov(retValue@parameters)
  
  # Replace NONMEM simulation time T by Campsis simulation time t
  retValue <- retValue %>%
    replaceAll("T", "t")
  
  return(retValue)
}

#' Add record to the specified CAMPSIS model.
#' 
#' @param model specified CAMPSIS model
#' @param record record to add
#' @param emptyRecord empty code record, already instantiated with the right type
#' @param parameters parameters
#' @return updated CAMPSIS model
#' @export
addconvertRecord <- function(model, record, emptyRecord, parameters) {
  if (length(record) > 0) {
    campsisRecord <- convertRecord(record, emptyRecord, parameters)
    model <- model %>%
      add(campsisRecord)
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
  
  if (expression$is_piecewise()) {
    exprCondPair <- expression$args[[1]]
    expression <- exprCondPair[[1]]
    condition <- exprCondPair[[2]]
    return(IfStatement(printSymPy(condition, simplify=TRUE),
                       Equation(symbol_chr, printSymPy(expression, simplify=FALSE))))

  } else if (expression %>% as.character() %>% startsWith("forward(")) {
    what <- expression$args[[1]]
    condition <- expression$args[[2]]
    return(IfStatement(printSymPy(condition, simplify=TRUE),
                       Equation(symbol_chr, printSymPy(what, simplify=FALSE))))
    
  } else if (isODE){
    cmtNumber <- extractValueInParentheses(symbol_chr)
    # browser()
    return(Ode(paste0("A_", cmtNumber), printSymPy(expression, simplify=FALSE)))

  } else {
    equation <- Equation(symbol_chr, printSymPy(expression, simplify=FALSE))
    return(checkNewindStatement(equation))
  }
}

#' Check if the equation is a newind statement.
#' E.g. if Equation("OCB", "first(CB, ID)"),
#' we return an if-statement.
#' 
#' @param equation equation to check
#' @return IfStatement or Equation
#' 
checkNewindStatement <- function(equation) {
  newindPattern <- "^first\\((.+), ID\\)$"
  
  if (grepl(pattern=newindPattern, x=equation@rhs)) {
    rhs_ <- gsub(pattern=newindPattern, replacement="\\1", x=equation@rhs)
    return(IfStatement("NEWIND != 2", 
                       Equation(equation@lhs, rhs_)))
  } else {
    return(equation)
  }
} 

#' NONMEM record (pharmpy) to CAMPSIS model.
#' 
#' @param record one or more NONMEM record
#' @param emptyRecord empty code record, already instantiated with the right type
#' @param parameters parameters
#' @return a CAMPSIS record
#' @export
convertRecord <- function(record, emptyRecord, parameters) {
  retValue <- emptyRecord
  
  if (! ("pharmpy.model.external.nonmem.records.code_record.CodeRecord" %in% class(record))) {
    stop("Not a DES record")  
  }
  # Retrieve statements list in R
  statements <- record$statements
  
  # Retrieve all equations
  for (index in (seq_along(statements) - 1)) {
    # print(index)
    statement <- statements[[index]]
    campsisStatement <- convertStatement(statement, parameters)
    
    # Don't add statement using Campsis add function (on model) since it checks for duplicates
    # Use append on list
    retValue@statements@list <- retValue@statements@list %>%
      append(campsisStatement)
  }
  
  return(retValue)
}

#' Pharmpy compartment system conversion to PMX model.
#' 
#' @param model Pharmpy model
#' @importFrom campsismod OdeRecord
#' @return ODE record (CAMPSIS domain)
#' @export
convertCompartmentSystem <- function(model) {
  # s
  # explicitSystem <- system$to_explicit_system()
  ode_system <- model$statements$ode_system
  odes <- ode_system$eqs
  
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
  central <- ode_system$central_compartment
  centralIndex <- which(ode_system$compartment_names==central$name)
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
    equation <- model %>% campsismod::find(initialValueNM)
    if (!is.null(equation)) {
      model <- model %>%
        add(InitialCondition(compartment=index, rhs=equation@rhs)) %>%
        delete(equation)
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
    ode <- model %>% campsismod::find(Ode(paste0("A_", compartment@name)))
    ode@rhs <- gsub(pattern=" \\+ Piecewise\\(.*", replacement="", ode@rhs)
    model <- model %>% campsismod::replace(ode)
  }
  return(model)
}

#' Remove useless equations (e.g. ETA_CL=ETA_CL). This can be useful when
#' the auto-mapping is used.
#' 
#' @param model CAMPSIS model
#' @return updated CAMPSIS model
#' @importFrom campsismod replace
#' @export
removeUselessEquations <- function(model) {
  main <- model %>% campsismod::find(MainRecord())
  if (!is.null(main)) {
    model <- model %>% campsismod::replace(discardUselessEquations(main))
  }
  ode <- model %>% campsismod::find(OdeRecord())
  if (!is.null(ode)) {
    model <- model %>% campsismod::replace(discardUselessEquations(ode))
  }
  error <- model %>% campsismod::find(ErrorRecord())
  if (!is.null(error)) {
    model <- model %>% campsismod::replace(discardUselessEquations(error))
  }
  return(model)
}

discardUselessEquations <- function(record) {
  record@statements@list <- record@statements@list %>% purrr::discard(.p=function(statement) {
    if (is(statement, "equation")) {
      if (statement@lhs==statement@rhs) {
        return(TRUE)
      }
    }
    return(FALSE)
  })
  return(record)
}

