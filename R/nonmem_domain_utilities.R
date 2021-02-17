
#' Is NONMEM parameter method.
#' 
#' @param str string
#' @return a logical value
#' @export
isNMParameter <- function(str) {
  return(isNMThetaParameter(str) | isNMEtaParameter(str) | isNMErrorParameter(str))
}

#' Is NONMEM THETA parameter method (THETA Fortran array).
#' 
#' @param str string
#' @return a logical value
#' @export
isNMThetaParameter <- function(str) {
  return(isNMArrayParameter(str, "THETA"))
}

#' Is NONMEM ETA parameter method (ETA Fortran array).
#' 
#' @param str string
#' @return a logical value
#' @export
isNMEtaParameter <- function(str) {
  return(isNMArrayParameter(str, "ETA"))
}

#' Is NONMEM OMEGA parameter method (OMEGA Fortran double array).
#' 
#' @param str string
#' @return a logical value
#' @export
isNMOmegaParameter <- function(str) {
  return(isNMDoubleArrayParameter(str, "OMEGA"))
}

#' Is NONMEM SIGMA parameter method (SIGMA Fortran double array).
#' 
#' @param str string
#' @return a logical value
#' @export
isNMSigmaParameter <- function(str) {
  return(isNMDoubleArrayParameter(str, "SIGMA"))
}

#' Is NONMEM ERROR parameter method (EPS or ERR Fortran array).
#' 
#' @param str string
#' @return a logical value
#' @export
isNMErrorParameter <- function(str) {
  return(isNMArrayParameter(str, "ERR") | isNMArrayParameter(str, "EPS"))
}

#' Is NONMEM/Fortran double array parameter.
#' 
#' @param str string
#' @param type parameter type
#' @return a logical value
#' @export
isNMDoubleArrayParameter <- function(str, type) {
  return(grepl(paste0("^", type, "\\s*\\(\\s*(\\d+,\\d+)\\s*\\)"), str))
}

#' Is NONMEM/Fortran single array parameter.
#' 
#' @param str string
#' @param type parameter type
#' @return a logical value
#' @export
isNMArrayParameter <- function(str, type) {
  return(grepl(paste0("^", type, "\\s*\\(\\s*(\\d+)\\s*\\)"), str))
}

#' Get type of NONMEM parameter.
#' 
#' @param str string
#' @return the type of NONMEM parameter
#' @export
getNMParameterType <- function(str) {
  
  retValue <- structure(list(
    type=NULL,
    index=NULL
  ), class="parameter_type")
  
  if (isNMThetaParameter(str)) {
    retValue$type <- "THETA"
    retValue$index <- extractValueInParentheses(str)
    
  } else if (isNMEtaParameter(str)) {
    retValue$type <- "ETA"
    retValue$index <- extractValueInParentheses(str)
    
  } else if (isNMErrorParameter(str)) {
    retValue$type <- "EPS"
    retValue$index <- extractValueInParentheses(str)
  }
  
  if (is.null(retValue$type)) {
    retValue <- NULL
  }
  
  return(retValue)
}

#' Extract value between parentheses.
#' 
#' @param str string
#' @return a string
#' @export
extractValueInParentheses <- function(str) {
  retValue <- gsub("[\\(\\)]", "", regmatches(str, gregexpr("\\(.*?\\)", str))[[1]])
  if (length(retValue) == 0) {
    stop(paste0("No parentheses found in ", str))
  }
  if (length(retValue) > 1) {
    stop(paste0("Several parentheses found in ", str))
  }
  
  return(retValue[1])
}