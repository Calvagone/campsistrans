#' 
#' Convert raw variance-covariance matrix for campsis model.
#' 
#' @param varcov raw variance-covariance matrix
#' @param parameters Campsis model parameters
#' @importFrom campsismod getNONMEMName
#' @return an updated variance-covariance matrix
convertVarcov <- function(varcov, parameters) {
  # Empty variance-covariance matrix
  if (length(varcov) == 0) {
    return(varcov)
  }
  
  # Replace THETA1 by THETA(1), etc
  oldNames <- colnames(varcov)
  standardNMNames <- gsub(pattern="THETA(\\d+)", replacement="THETA(\\1)", x=oldNames)
  rownames(varcov) <- standardNMNames
  colnames(varcov) <- standardNMNames
  
  # Retrieve NONMEM names from parameters
  nmNames <- parameters@list %>% purrr::map_chr(.f=~.x %>% campsismod::getNONMEMName())
  
  # Retrieve varcov parameters
  varcovParams <- colnames(varcov) %>% purrr::map(.f=function(.x) {
    return(convertNONMEMParameter(name=.x, value=0, fix=FALSE))
  })
  
  # Remove unnecessary rows and columns
  for (varcovParam in varcovParams) {
    nmName <- varcovParam %>% campsismod::getNONMEMName()
    parameter <- parameters %>% getByIndex(varcovParam)
    if (length(parameter)==0) {
      if (!is(varcovParam, "theta") && !varcovParam %>% campsismod::isDiag()) {
        # All off-diagonal OMEGA's are exported by NONMEM into the cov file
        # Even if they are NOT described in the control stream...
        # This means we can remove it from the matrix
        index <- which(colnames(varcov) == nmName)
        varcov <- varcov[-index, ]
        varcov <- varcov[, -index]
      } else {
        stop(paste0("Parameter ", nmName, " can't be found in model parameters."))
      }
    } else {
      # Parameter is well described in model
      # 1) The parameter may be fixed, if yes, it needs to be removed
      # 2) The parameter may be an OMEGA 'SAME', then it also needs to be removed
      if (parameter@fix || (is(parameter, "omega") && isTRUE(parameter@same))) {
        index <- which(colnames(varcov) == nmName)
        varcov <- varcov[-index, ]
        varcov <- varcov[, -index]
      }
    }
  }
  
  # Update list 
  varcovParams <- colnames(varcov) %>% purrr::map(.f=function(.x) {
    varcovParam <- convertNONMEMParameter(name=.x, value=0, fix=FALSE)
    parameter <- parameters %>% getByIndex(varcovParam)
    if (length(parameter)==0) {
      stop("This should never happen because already checked above.")
    }
    return(parameter)
  })
  
  # Rename varcov matrix correctly according to names in model
  names <- varcovParams %>% purrr::map_chr(.f=~.x %>% getName())
  rownames(varcov) <- names
  colnames(varcov) <- names
  return(varcov)
}

#' Convert NONMEM parameter (string form) to campsismod parameter.
#' 
#' @param name NONMEM parameter name, character value
#' @param value parameter value
#' @param fix is fixed or not, logical value
#' @return S4 parameters object
#' @importFrom campsismod Theta Omega Sigma
#' @export
convertNONMEMParameter <- function(name, value, fix) {
  index <- extractValueInParentheses(name)
  isTheta <- isNMThetaParameter(name)
  isOmega <- isNMOmegaParameter(name)
  isSigma <- isNMSigmaParameter(name)
  
  if (isTheta) {
    param <- campsismod::Theta(index=index, value=value, fix=fix)
    
  } else if (isOmega || isSigma) {
    indexes <- strsplit(index, ",")
    index1 <- indexes[[1]][1]
    index2 <- indexes[[1]][2]
    className <- if(isOmega) {"omega"} else {"sigma"}
    if (isOmega) {
      param <- campsismod::Omega(index=index1, index2=index2, value=value, fix=fix)
    } else {
      param <- campsismod::Sigma(index=index1, index2=index2, value=value, fix=fix)
    }
  } else {
    stop(paste0("Unknown parameter ", name, ": estimated parameter type must be THETA, OMEGA or SIGMA."))
  }
  return(param)
}
