
#' Print symbolic expressions or conditions.
#' 
#' @param x SymPy expression or condition
#' @param output type of desired output
#' @return code
#' @importFrom reticulate import py_capture_output
#' @export
printSymPy <- function(x, output = "C") {
  sympy <- reticulate::import("sympy")
  if (output == "C") {
    print <- reticulate::py_capture_output(sympy$print_ccode(x))
    print <- gsub("[\r\n]", "", print)
  } else {
    print <- as.character(x)
  }
}

getNumberOfEtas <- function(model) {
  pharmpyOmegas <- model$control_stream$get_records("OMEGA")
  etas <- 0
  for (index in seq_along(pharmpyOmegas)) {
    map <- pharmpyOmegas[[index]]$eta_map
    etas <- etas + length(map)
  }
  return(etas)
}

#'
#' Remove RATE input from $INPUT field (string-based model). 
#' 
#' @param x string value (the whole control stream)
#' @return the same string value without RATE input
#' @export
#' 
removeRateFromString <- function(x) {
  retValue <- x
  
  # RATE followed by at least one space
  retValue <- gsub(pattern="^(.*)(\\$INPUT)([^\\$]*)([[:space:]]+RATE[ ]+)(.*)", replacement="\\1\\2\\3 \\5", x=retValue)
  
  # RATE followed by a combination of break line or space
  # In that case, break line is re-added
  retValue <- gsub(pattern="^(.*)(\\$INPUT)([^\\$]*)([[:space:]]+RATE[[:space:]]+)(.*)", replacement="\\1\\2\\3 \n\\5", x=retValue)
  
  return(retValue)
}

#'
#' Remove RATE input from $INPUT field in given control stream. 
#' 
#' @param file control stream file name
#' @return nothing
#' @export
#' 
removeRateFromCtl <- function(file) {
  fileConn = file(file)
  x <- paste0(readLines(con=fileConn), collapse="\n")
  x_ <- removeRateFromString(x)
  
  writeLines(text=x_, con=fileConn)
  close(fileConn)
}

#'
#' Give name to the off-diagonal elements of the covariance matrix. 
#' 
#' @param model Campsis model
#' @return updated Campsis model
#' @importFrom campsismod isDiag
#' @export
#' 
nameCovariance <- function(model) {
  parameters <- model@parameters
  hasVarcov <- length(parameters@varcov) > 0
  
  retValue <- parameters
  retValue@list <- list()
  colnamesVarcov <- colnames(parameters@varcov)
  
  for (listIndex in seq_len(length(parameters))) {
    parameter <- parameters@list[[listIndex]]
    if (is(parameter, "omega") && !parameter %>% campsismod::isDiag()) {
      oldName <- parameter %>% getName()
      covName <- getCovarianceName(parameters=parameters, index1=parameter@index, index2=parameter@index2)
      if (!is.na(covName)) {
        parameter@name <- covName
        updatedName <- parameter %>% getName()
        if (hasVarcov) {
          colnamesVarcov[colnamesVarcov==oldName] <- updatedName
        }
      }
    }
    retValue@list[[listIndex]] <- parameter
  }
  colnames(retValue@varcov) <- colnamesVarcov
  rownames(retValue@varcov) <- colnamesVarcov
  
  model@parameters <- retValue
  return(model)
}

getCovarianceName <- function(parameters, index1, index2) {
  omega1 <- parameters %>% campsismod::getByIndex(Omega(index=index1, index2=index1))
  omega2 <- parameters %>% campsismod::getByIndex(Omega(index=index2, index2=index2))
  
  name1 <- omega1@name
  name2 <- omega2@name
  
  if (is.na(name1) || is.na(name2)) {
    return(as.character(NA))
  } else {
    return(paste0(name1, "_", name2))
  }
}

#'
#' Convert off-diagonal elements to correlations.
#' 
#' @param model Campsis model
#' @return updated Campsis model
#' @importFrom campsismod getByIndex replace standardise
#' @export
#' 
covarToCor <- function(model) {
  parameters <- model@parameters
  for (param in parameters@list) {
    if (is(param, "omega") && param@type=="covar") {
      omega1 <- parameters %>% campsismod::getByIndex(Omega(index=param@index, index2=param@index)) %>% campsismod::standardise()
      omega2 <- parameters %>% campsismod::getByIndex(Omega(index=param@index2, index2=param@index2)) %>% campsismod::standardise()
      param@value <- param@value/(sqrt(omega1@value)*sqrt(omega2@value))
      param@type <- "cor"
      model <- model %>% campsismod::replace(param)
    }
  }
  return(model)
}
