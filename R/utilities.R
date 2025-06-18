
#' Print symbolic expressions or conditions.
#' 
#' @param x SymPy expression or condition
#' @param output type of desired output
#' @param simplify whether to simplify the expression before printing, default is TRUE
#' @return code
#' @importFrom reticulate import py_capture_output
#' @export
printSymPy <- function(x, output="C", simplify=TRUE) {
  sympy <- reticulate::import("sympy")
  if (output == "C") {
    expr <- x # sympy$parse_expr(as.character(x))
    # print(class(expr))
    # print(expr)
    if (reticulate::py_has_attr(expr, name="subs")) {
      # expr <- expr$subs(sympy$Function("newind")(), sympy$Symbol("NEWIND"))
    }
    if (simplify) {
      expr <- tryCatch({
        sympy$simplify(expr)
      }, error = function(e) {
        return(expr)
      })
    }
    # Note: when not supported in C
    # Message
    # /* Not supported in C: */
    # /* A_0 */
    # is deleted automatically
    print <- tryCatch({
      gsub(pattern="^/\\*.*\\*/\\s*", replacement="",
           x=reticulate::py_capture_output(sympy$print_ccode(expr, strict=FALSE)))
    }, error = function(e) {
      return(as.character(x))
    })
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
#' @param x string value (the whole control stream, 1 line)
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
#' Remove NONMEM comments. 
#' 
#' @param lines lines
#' @return lines without any comment
#' @export
#' 
removeNONMEMComments <- function(lines) {
  retValue <- lines
  
  # Any comment
  retValue <- gsub(pattern="^(.*)(;.*)", replacement="\\1", x=retValue)
  retValue <- trimws(x=retValue, which="right")
  
  return(retValue)
}

#'
#' Adapt NONMEM control stream by manipulating the source file. 
#' 
#' @param file control stream file name
#' @param rem_rate remove RATE in control stream automatically to avoid issues with Pharmpy.
#'  Otherwise, it will look for the dataset and possibly adapt the ODE's to add the rates, default is FALSE
#' @param rem_abbr_replace remove section ABBREVIATED REPLACE, causing issue in import, default is FALSE
#' @return nothing
#' @export
#' 
adaptNONMEMControlStream <- function(file, rem_rate, rem_abbr_replace) {
  fileConn = file(file)
  lines <- readLines(con=fileConn)
  lines <- removeNONMEMComments(lines)
  
  retValue <- paste0(lines, collapse="\n")
  
  if (rem_rate) {
    retValue <- removeRateFromString(retValue)
  }
  if (rem_abbr_replace) {
    retValue <- removeAbbreviatedReplaceFromString(retValue)
  }
  
  writeLines(text=retValue, con=fileConn)
  close(fileConn)
}

#'
#' Remove $ABBREVIATED REPLACE section from string. 
#' 
#' @param x string value (the whole control stream, 1 line)
#' @return the same string value without the abbreviated replace section
#' @export
#' 
removeAbbreviatedReplaceFromString <- function(x) {
  retValue <- x
  retValue <- gsub(pattern="^(.*)(\\$ABBREVIATED REPLACE)([^\n]*)(\n)(.*)", replacement="\\1\\4\\5", x=retValue)
  return(retValue)
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
