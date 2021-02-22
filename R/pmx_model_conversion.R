
#' PMXtran model conversion to PMX model.
#' 
#' @param pmxtran PMXtran model
#' @return PMX model
#' @importFrom assertthat assert_that
#' @export
toPmxModel <- function(pmxtran) {
  assertthat::assert_that(inherits(pmxtran, "pmx_tran"),
                          msg="pmxtran is not a PMXtran object")
  statements <- pmxtran$model$statements
  code <- c()
  
  # Careful, index in python starts at 0
  for (index in (seq_along(statements)-1)) {
    statement <- statements[[index]]
    
    if ("pharmpy.statements.CompartmentalSystem" %in% class(statement)) {
      tmp_code <- compartmentSystemToPmxModel(statement)
      code <- c(code, tmp_code)
      
      
    } else if ("pharmpy.statements.Assignment" %in% class(statement)){
      tmp_code <- statementToPmxModel(statement, pmxtran$params)
      code <- c(code, tmp_code)
      
    } else {
      cat(paste("Unknown class", class(statement)))  
    }
    
  }
  
  retValue <- new("pmx_model", code=code, parameters=pmxtran$params)
  
  return(retValue)
}

#' SymPy statement conversion to PMX model.
#' 
#' @param statement SymPy statement
#' @param params parameters definition table
#' @return C code
#' @importFrom reticulate iterate
#' @export
statementToPmxModel <- function(statement, params) {
  
  symbol <- statement$symbol
  symbol_chr <- as.character(symbol)
  expression <- statement$expression
  
  free_symbols <- reticulate::iterate(expression$free_symbols)
  
  for (symbolIndex in seq_along(free_symbols)) {
    freeSymbol <- free_symbols[[symbolIndex]]
    expression <- replaceSymbolAuto(expression, freeSymbol, params)
  }
  
  if ("sympy.functions.elementary.piecewise.Piecewise" %in% class(expression)) {
    return(piecewiseToPmxModel(symbol, expression))
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

#' Pharmpy compartment system conversion to PMX model.
#' 
#' @param system Pharmpy compartment system
#' @return C code
#' @export
compartmentSystemToPmxModel <- function(system) {
  
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