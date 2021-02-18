
#' Pharmpy model conversion to RxODE code.
#' 
#' @param model PMX model
#' @return RxODE code
#' @importFrom assertthat assert_that
#' @importFrom reticulate iterate
#' @export
toRxODE <- function(model) {
  assertthat::assert_that(inherits(model, "pmx_model"),
                          msg="model is not a PMX model")
  statements <- model$model$statements
  code <- c()
  
  # Careful, index in python starts at 0
  for (index in (seq_along(statements)-1)) {
    statement <- statements[[index]]

    if ("pharmpy.statements.CompartmentalSystem" %in% class(statement)) {
      tmp_code <- compartmentSystemToRxODE(statement)
      code <- c(code, tmp_code)
       
    
    } else if ("pharmpy.statements.Assignment" %in% class(statement)){
      tmp_code <- statementToRxODE(statement, model$params)
      code <- c(code, tmp_code)

    } else {
      cat(paste("Unknown class", class(statement)))  
    }
    
  }
  
  return(code)
}

#' SymPy statement conversion to RxODE code.
#' 
#' @param statement SymPy statement
#' @param params parameters definition table
#' @return RxODE code
#' @importFrom reticulate iterate
#' @export
statementToRxODE <- function(statement, params) {
  
  symbol <- statement$symbol
  symbol_chr <- as.character(symbol)
  expression <- statement$expression
  
  free_symbols <- reticulate::iterate(expression$free_symbols)
  
  for (symbolIndex in seq_along(free_symbols)) {
    freeSymbol <- free_symbols[[symbolIndex]]
    expression <- replaceSymbolAuto(expression, freeSymbol, params)
  }
  
  if ("sympy.functions.elementary.piecewise.Piecewise" %in% class(expression)) {
    return(piecewiseToRxODE(symbol, expression))
  } else {
    return(paste0(symbol_chr, "=", printSymPy(expression)))
  }
}

#' SymPy piecewise conversion to RxODE code.
#' 
#' @param symbol SymPy statement symbol
#' @param piecewise SymPy piecewise
#' @return RxODE code
#' @export
piecewiseToRxODE <- function(symbol, piecewise) {
  symbol_chr <- as.character(symbol)
  
  exprCondPair <- piecewise$args[[1]]
  
  if (! ("sympy.functions.elementary.piecewise.ExprCondPair" %in% class(exprCondPair))) {
    stop(paste("Class can only be", class(exprCondPair), "for now..."))  
  } 
  
  expression <- exprCondPair$args[[1]]
  condition <- exprCondPair$args[[2]]

  return(paste0("if (", printSymPy(condition), ") ", symbol_chr, "=", printSymPy(expression)))
}

#' Pharmpy compartment system conversion to RxODE code.
#' 
#' @param system Pharmpy compartment system
#' @return RxODE code
#' @export
compartmentSystemToRxODE <- function(system) {
  
  explicitOdes <- system$to_explicit_odes()
  odes <- explicitOdes[[1]]
  
  cptNames <- NULL
  code <- c()
  
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
    
    code <- c(code, paste0("d/dt(", cptName, ") = ", equation))
  }
  
  return(code)
}

#' Access estimate and initial values from model.
#' 
#' @param model PMX model
#' @param nmtype NONMEM type: THETA, OMEGA or SIGMA
#' @param estimate if TRUE, estimated values are used, if FALSE, initial values are used
#' @return named vector with the requested parameters
#' @export
rxodeParams <- function(model, nmtype, estimate=TRUE) {
  
  assertthat::assert_that(inherits(model, "pmx_model"),
                          msg="model is not a PMX model")
  table <- model$params$table
  parameters <- table %>% dplyr::filter(type==nmtype & (is.na(diag) | diag))
  
  if (nmtype=="OMEGA") {
    names <- paste0("ETA", "_", parameters %>% dplyr::pull(suffix))
  } else if(nmtype=="SIGMA") {
    names <- paste0("EPS", "_", parameters %>% dplyr::pull(suffix))
  } else {
    names <- paste0(nmtype, "_", parameters %>% dplyr::pull(suffix))
  }
  
  
  if (estimate) {
    if ("estimate" %in% colnames(table)) {
      retValue <- parameters %>% dplyr::pull("estimate")
    } else {
      stop("Parameters table does not contain estimated parameters")
    }
  } else {
    if ("initial_value" %in% colnames(table)) {
      retValue <- parameters %>% dplyr::pull("initial_value")  
    } else {
      stop("Parameters table does not contain initial values")
    }
  }
  
  names(retValue) <- names
  return(retValue)
}

#' Get the THETA vector for RxODE.
#' 
#' @param model PMX model
#' @param estimate if TRUE, estimated values are used, if FALSE, initial values are used
#' @return named vector with THETA values
#' @export
rxodeTheta <- function(model, estimate=TRUE) {
  return(rxodeParams(model=model, nmtype="THETA", estimate=estimate))
}

#' Get the IIV matrix (omega) for RxODE.
#' 
#' @param model PMX model
#' @param estimate if TRUE, estimated values are used, if FALSE, initial values are used
#' @return named matrix with OMEGA values
#' @export
rxodeOmega <- function(model, estimate=TRUE) {
  table <- model$params$table
  diag <- rxodeParams(model=model, nmtype="OMEGA", estimate=estimate)
  diagMatrix <- toDiagonalMatrix(diag)
  
  # Adding ETA correlations to the matrix
  corrEtas <- table %>% dplyr::filter(type=="OMEGA" & (!is.na(diag) & !diag))
  for (rowIndex in seq_len(nrow(corrEtas))) {
    row <- corrEtas[rowIndex,]
    primary_index <- row %>% dplyr::pull(primary_index)
    secondary_index <- row %>% dplyr::pull(secondary_index)
    if (estimate) {
      value <- row %>% dplyr::pull("estimate")
    } else {
      value <- row %>% dplyr::pull(initial_value)
    }
    
    # Matrix is symmetric
    diagMatrix[paste0("ETA_", primary_index), paste0("ETA_", secondary_index)] <- value
    diagMatrix[paste0("ETA_", secondary_index), paste0("ETA_", primary_index)] <- value
  }
  
  return(diagMatrix)
}

#' Get the RUV matrix (sigma) for RxODE.
#' No correlations are possible for now.
#' 
#' @param model PMX model
#' @param estimate if TRUE, estimated values are used, if FALSE, initial values are used
#' @return named matrix with SIGMA values
#' @export
rxodeSigma <- function(model, estimate=TRUE) {
  diag <- rxodeParams(model=model, nmtype="SIGMA", estimate=estimate)
  diagMatrix <- toDiagonalMatrix(diag)
  return(diagMatrix)
}