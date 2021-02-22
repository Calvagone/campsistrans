
#' Import NONMEM control stream and results using Pharmpy.
#' 
#' @param x path to NONMEM control stream
#' @param mapping a possible PMX mapping object
#' @param estimate if TRUE, estimated values are imported, if FALSE, initial values are used
#' @return a PMXtran object
#' @importFrom reticulate import
#' @export
importNONMEM <- function(x, mapping=NULL, estimate=FALSE) {
  pharmpy <- reticulate::import("pharmpy")
  model <- pharmpy$Model(x)
  
  retValue <- structure(list(
    model=model,
    params=params(model, mapping=mapping, estimate=estimate)
  ), class="pmx_tran")
  
  return(retValue)
}


#' Replace all occurrences of the given symbol according to the parameters definition table.
#' 
#' @param expression SymPy expression
#' @param symbol SymPy symbol to be replaced
#' @param params parameters definition table
#' @return the updated expression
#' @importFrom reticulate import
#' @export
replaceSymbolAuto <- function(expression, symbol, params) {
  sympy <- reticulate::import("sympy")
  symbol_chr <- as.character(symbol)
  type <- getNMParameterType(symbol_chr)
  
  if (is.null(type)) {
    # Do nothing
  } else {
    replacementSymbol <- sympy$symbols(nameParameter(type, params))
    expression <- replaceSymbol(expression, symbol, replacementSymbol)
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
#' @param params parameters
#' @return a pretty parameter name
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter pull
#' @importFrom pmxmod getName getParameter
#' @export
nameParameter <- function(type, params) {
  vec <- NULL
  paramType <- type$type
  index <- type$index
  
  if (type$type=="THETA") {
    pType <- "theta"
    param <- params %>% pmxmod::getParameter(type=pType, index=as.integer(index))
    
  } else if (type$type=="ETA") {
    pType <- "omega"
    param <- params %>% pmxmod::getParameter(type=pType, index=as.integer(index), index2=as.integer(index))
    
  } else if (type$type=="EPS") {
    pType <- "sigma"
    param <- params %>% pmxmod::getParameter(type=pType, index=as.integer(index), index2=as.integer(index))
    
  } else {
    stop("Type must be THETA, ETA or EPS")
  }
  
  if (length(param)==0) {
    stop(paste0("No parameter found for type ", pType, " and index ", index))
  }

  return(param %>% pmxmod::getName())
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

#' Write PMXtran object to NONMEM control stream file.
#' 
#' @param pmxtran PMXtran object
#' @param path path to desired control stream file
#' @export
modelToNONMEM <- function(pmxtran, path) {
  assertthat::assert_that(inherits(pmxtran, "pmx_tran"),
                          msg="not a pmxtran object")
  if (file.exists(path)) {
    file.remove(path)
  }
  pmxtran$model$write(path)
}
