
#' Is NONMEM parameter method.
#' 
#' @param str string
#' @return a logical value
#' @export
isPharmpyParameter <- function(str) {
  return(isNMThetaParameter(str) | isNMEtaParameter(str) | isNMErrorParameter(str))
}

#' Is NONMEM THETA parameter method (THETA Fortran array).
#' 
#' @param str string
#' @return a logical value
#' @export
isPharmpyThetaParameter <- function(str) {
  return(isPharmpyArrayParameter(str, "THETA"))
}

#' Is NONMEM ETA parameter method (ETA Fortran array).
#' 
#' @param str string
#' @return a logical value
#' @export
isPharmpyEtaParameter <- function(str) {
  return(isPharmpyArrayParameter(str, "ETA"))
}

#' Is NONMEM OMEGA parameter method (OMEGA Fortran double array).
#' 
#' @param str string
#' @return a logical value
#' @export
isPharmpyOmegaParameter <- function(str) {
  return(isPharmpyDoubleArrayParameter(str, "OMEGA"))
}

#' Is NONMEM SIGMA parameter method (SIGMA Fortran double array).
#' 
#' @param str string
#' @return a logical value
#' @export
isPharmpySigmaParameter <- function(str) {
  return(isPharmpyDoubleArrayParameter(str, "SIGMA"))
}

#' Is NONMEM ERROR parameter method (EPS or ERR Fortran array).
#' 
#' @param str string
#' @return a logical value
#' @export
isPharmpyEpsParameter <- function(str) {
  return(isPharmpyArrayParameter(str, "EPS"))
}

#' Is NONMEM/Fortran double array parameter.
#' 
#' @param str string
#' @param type parameter type
#' @return a logical value
#' @export
isPharmpyDoubleArrayParameter <- function(str, type) {
  return(grepl(paste0("^", type, "_\\d+_\\d+$"), str))
}

#' Is NONMEM/Fortran single array parameter.
#' 
#' @param str string
#' @param type parameter type
#' @return a logical value
#' @export
isPharmpyArrayParameter <- function(str, type) {
  return(grepl(paste0("^", type, "_\\d+$"), str))
}

#' Get type of NONMEM parameter.
#' 
#' @param str string
#' @return the type of NONMEM parameter
#' @export
toNONMEMStyle <- function(str) {
  index <- extractValueAfterUnderscore(str)
  
  if (isPharmpyThetaParameter(str)) {
    retValue <- sprintf("THETA(%i)", index)
    
  } else if (isPharmpyEtaParameter(str)) {
    retValue <- sprintf("OMEGA(%i,%i)", index, index)
  
  } else if (isPharmpyOmegaParameter(str)) {
    retValue <- sprintf("OMEGA(%i,%i)", index[1], index[2])
    
  } else if (isPharmpyEpsParameter(str)) {
    retValue <- sprintf("SIGMA(%i,%i)", index, index)
    
  } else if (isPharmpySigmaParameter(str)) {
    retValue <- sprintf("SIGMA(%i,%i)", index[1], index[2])
  }

  return(retValue)
}

#' Extract value(s) after underscore.
#' 
#' @param str string
#' @return a string
#' @export
extractValueAfterUnderscore <- function(str) {
  tmp <- sub(pattern="^[^_]*_", replacement="", x=str)
  tmp_ <- strsplit(tmp, split="_")[[1]]
  return(as.numeric(tmp_))
}
