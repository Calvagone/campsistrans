
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
      
    } else if ("pharmpy.statements.ODESystem" %in% class(statement)) {
      des <- pmxtran$model$control_stream$get_records("DES")[[1]]
      tmp_code <- desToPmxModel(des)
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
  
  dadtPattern <- "^DADT\\(.*\\)$"
  isODE <- grepl(pattern=dadtPattern, x=symbol_chr, ignore.case=TRUE)
  
  if ("sympy.functions.elementary.piecewise.Piecewise" %in% class(expression)) {
    return(piecewiseToPmxModel(symbol, expression))
  
  } else if (isODE){
    cmtNumber <- extractValueInParentheses(symbol_chr)
    return(paste0("d/dt(", "A_", cmtNumber, ") = ", printSymPy(expression)))

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

#' DES block (non processed) to PMX model.
#' 
#' @param statements SymPy statements (not processed)
#' @return C code
#' @export
desToPmxModel <- function(desRecord) {
  
  if (! ("pharmpy.plugins.nonmem.records.code_record.CodeRecord" %in% class(desRecord))) {
    stop("Not a DES record")  
  }
  statements <- desRecord$statements
  code <- NULL
  
  # Retrieve all equations
  for (index in (seq_along(statements) - 1)) {
    statement <- statements[[index]]
    code <- c(code, statementToPmxModel(statement))
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