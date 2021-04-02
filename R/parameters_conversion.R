
#' Create PMX mapping.
#' 
#' @param theta named integer vector for THETA mapping
#' @param omega named integer vector for THETA mapping
#' @param sigma named integer vector for OMEGA mapping
#' @importFrom assertthat assert_that
#' @importFrom pmxmod add Parameters Theta Omega Sigma
#' @importFrom purrr map2
#' @return PMX mapping object
#' @export
mapping <- function(theta=NULL, omega=NULL, sigma=NULL) {
  params <- Parameters()
  if (!is.null(theta)) {
    assertthat::assert_that(is.numeric(theta), msg="theta is not numeric")
    names <- if (is.character(names(theta))) {names(theta)} else {rep(NA, length(theta))}
    purrr::map2(theta, names, .f=function(index, name) {
      params <<- params %>% pmxmod::add(Theta(name=as.character(name), index=index))
    })
  }
  if (!is.null(omega)) {
    assertthat::assert_that(is.numeric(omega), msg="omega is not numeric")
    names <- if (is.character(names(omega))) {names(omega)} else {rep(NA, length(omega))}
    purrr::map2(omega, names, .f=function(index, name) {
      params <<- params %>% pmxmod::add(Omega(name=as.character(name), index=index, index2=index))
    })
  }
  if (!is.null(sigma)) {
    assertthat::assert_that(is.numeric(sigma), msg="sigma is not numeric")
    names <- if (is.character(names(sigma))) {names(sigma)} else {rep(NA, length(sigma))}
    purrr::map2(sigma, names, .f=function(index, name) {
      params <<- params %>% pmxmod::add(Sigma(name=as.character(name), index=index, index2=index))
    })
  }
  retValue <- structure(list(
    params=params
  ), class="pmxmapping")
}


#' Define and annotate your parameters.
#' 
#' @param model Pharmpy model
#' @param mapping PMX mapping
#' @return parameters definition table
#' @param estimate if TRUE, estimated values are used, if FALSE, initial values are used
#' @importFrom purrr map map2
#' @importFrom pmxmod add clean getByIndex getNONMEMName Parameters sort
#' @export
convertParameters <- function(model, mapping, estimate) {
  
  assertthat::assert_that(inherits(model, "pharmpy.plugins.nonmem.model.Model"),
                          msg="model is not a Pharmpy model")
  if (!is.null(mapping)) {
    assertthat::assert_that(inherits(mapping, "pmxmapping"),
                            msg="mapping is not a PMX mapping object")
  }
  
  # Retrieve parameters from mapping
  mappingList <- mapping$params
  
  # Retrieve initial values from parset
  parset <- model$parameters
  pharmpyList <- retrieveInitialValues(parset)
  
  # Collect names from mapping list (LOOP 1)
  list <- purrr::map(pharmpyList@list, .f=function(parameter) {
    namedParameter <- mappingList %>% pmxmod::getByIndex(parameter)
    if (length(namedParameter) > 0) {
      parameter@name <- namedParameter@name
    }
    return(parameter)
  })
  params <- Parameters()
  params@list <- list
  
  # Check no parameter is missing (LOOP 2)
  purrr::map(mappingList@list, .f=function(parameter) {
    returnedParameter <- params %>% pmxmod::getByIndex(parameter)
    if (length(returnedParameter) == 0) {
      params <<- params %>% pmxmod::add(parameter)
    }
  })
  
  if (!estimate) {
    return(params %>% pmxmod::clean() %>% pmxmod::sort())
  }
  
  # Reading estimated values with pharmpy
  estimates <- model$modelfit_results$parameter_estimates
  
  if (is.null(estimates)) {
    stop("No NONMEM results are available through Pharmpy")
  }
  
  list <- params@list %>% purrr::map(.f=function(param) {
    name <- param %>% pmxmod::getNONMEMName()
    estimateIndex <- which(names(estimates)==name)
    if (length(estimateIndex) == 0) {
      if (!is.na(param@fix) && !param@fix) {
        warning(paste0("No estimate for parameter ", name))
      }
    } else if (length(estimateIndex) == 1){
      param@value <- estimates[[estimateIndex]]
      
    } else {
      warning(paste0("Several values corresponding to ", name))
    }
    
    return(param)
  })
  
  parameters <- Parameters()
  parameters@list <- list
  return(parameters %>% pmxmod::clean() %>% pmxmod::sort())
}

#' Retrieve initial values from Pharmpy parameter set.
#' 
#' @param parset Pharmpy parameter set
#' @return S4 parameters object
#' @importFrom purrr map2
#' @importFrom assertthat assert_that
#' @export
retrieveInitialValues <- function(parset) {
  assertthat::assert_that(inherits(parset, "pharmpy.parameter.ParameterSet"),
                          msg="parset is not a parameter set")
  
  parameters <- purrr::map2(parset$inits, names(parset$inits), .f=function(initialValue, name) {
    fix <- as.logical(parset$fix[name])
    return(convertNONMEMParameter(name=name, value=initialValue, fix=fix))
  })
  
  return(new("parameters", list=parameters))
}

#' Convert NONMEM parameter (string form) to pmxmod parameter.
#' 
#' @param name NONMEM parameter name, character value
#' @param value parameter value
#' @param fix is fixed or not, logical value
#' @return S4 parameters object
#' @importFrom pmxmod Theta Omega Sigma
#' @export
convertNONMEMParameter <- function(name, value, fix) {
  index <- extractValueInParentheses(name)
  isTheta <- isNMThetaParameter(name)
  isOmega <- isNMOmegaParameter(name)
  isSigma <- isNMSigmaParameter(name)
  
  if (isTheta) {
    param <- pmxmod::Theta(index=index, value=value, fix=fix)
    
  } else if (isOmega || isSigma) {
    indexes <- strsplit(index, ",")
    index1 <- indexes[[1]][1]
    index2 <- indexes[[1]][2]
    className <- if(isOmega) {"omega"} else {"sigma"}
    if (isOmega) {
      param <- pmxmod::Omega(index=index1, index2=index2, value=value, fix=fix)
    } else {
      param <- pmxmod::Sigma(index=index1, index2=index2, value=value, fix=fix)
    }
  } else {
    stop(paste0("Unknown parameter ", name, ": estimated parameter type must be THETA, OMEGA or SIGMA."))
  }
  return(param)
}
