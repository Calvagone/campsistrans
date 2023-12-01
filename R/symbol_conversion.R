
#' Replace all occurrences of the given symbol according to the parameters definition table.
#' 
#' @param expression SymPy expression
#' @param symbol SymPy symbol to be replaced
#' @param parameters parameters
#' @return the updated expression
#' @importFrom reticulate import
#' @export
replaceSymbolAuto <- function(expression, symbol, parameters) {
  sympy <- reticulate::import("sympy")
  symbol_chr <- as.character(symbol)
  
  type <- getNMParameterType(symbol_chr)
  
  if (is.null(type)) {
    # Do nothing
  } else {
    replacementSymbol <- tryCatch(
      expr=sympy$symbols(nameParameter(type, parameters)),
      error = function(e) {
        warning(e$message)
        return(NULL)
      })
    if (!is.null(replacementSymbol)) {
      expression <- replaceSymbol(expression, symbol, replacementSymbol)
    }
  }
  return(expression)
}

#' Replace all occurrences of the given symbol by the replacement symbol.
#' 
#' @param expression SymPy expression
#' @param symbol SymPy symbol to be replaced
#' @param replacementSymbol SymPy replacement symbol
#' @return the updated expression
#' @importFrom reticulate dict
#' @export
replaceSymbol <- function(expression, symbol, replacementSymbol) {
  expression <- expression$xreplace(rule=reticulate::dict(symbol=replacementSymbol))
  return(expression)
}

#' Name the given parameter type according to the parameters table.
#' 
#' @param type parameter type
#' @param parameters parameters
#' @return a pretty parameter name
#' @importFrom campsismod getNameInModel getByIndex Theta Omega Sigma
#' @export
nameParameter <- function(type, parameters) {
  vec <- NULL
  paramType <- type$type
  index <- type$index
  
  if (type$type=="THETA") {
    pType <- "theta"
    param <- parameters %>% campsismod::getByIndex(Theta(index=index))
    
  } else if (type$type=="ETA") {
    pType <- "omega"
    param <- parameters %>% campsismod::getByIndex(Omega(index=index, index2=index))
    
  } else if (type$type=="EPS") {
    pType <- "sigma"
    param <- parameters %>% campsismod::getByIndex(Sigma(index=index, index2=index))
  
  } else if (type$type=="A") {
    return(paste0("A_", index))
    
  } else {
    stop("Type must be THETA, ETA or EPS")
  }
  
  if (length(param)==0) {
    stop(paste0("No parameter found for type ", pType, " and index ", index))
  }
  
  return(param %>% campsismod::getNameInModel())
}

#' Retrieve compartment name based on left hand side expression.
#' 
#' @param lhs left hand side expression of the derivative
#' @return the compartment name
#' @export
retrieveCompartmentName <- function(lhs) {
  lhs_chr <- as.character(lhs)
  cptName <- sub("^Derivative\\(", "", lhs_chr)
  cptName <- sub("\\(t\\), t\\)$", "", cptName)
  return(cptName)
}

#' Diagonal vector to matrix utility method.
#' 
#' @param diagVector the diagonal vector
#' @return a matrix
#' @export
toDiagonalMatrix <- function(diagVector) {
  names <- names(diagVector)
  retValue <- diag(diagVector, nrow=length(names))
  rownames(retValue) <- names
  colnames(retValue) <- names
  retValue <- as.matrix(retValue)
  return(retValue)
}
