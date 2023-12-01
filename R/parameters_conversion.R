
#' Add mapping to parameters object.
#' 
#' @param x mapping vector
#' @param parameters parameters object
#' @param type mapping vector type
#' @importFrom assertthat assert_that
#' @importFrom campsismod add Theta Omega Sigma
#' @importFrom purrr map2
#' @return parameters
addMapping <- function(x, parameters, type) {
  if (!is.null(x)) {
    assertthat::assert_that(is.numeric(x), msg=paste0(type, " is not numeric"))
    names <- names(x)
    if (is.null(names)) {
      names <- rep(NA, length(x))
    }
    names <- ifelse(names=="", NA, names)
    temp <- purrr::map2(x, names, .f=function(index, name) {
      if (type=="theta") {
        return(Theta(name=as.character(name), index=index))
      } else if (type=="omega") {
        return(Omega(name=as.character(name), index=index, index2=index))
      } else if (type=="sigma") {
        return(Sigma(name=as.character(name), index=index, index2=index))
      } else {
        stop("Type can only be theta, omega or sigma")
      }
    })
    parameters <- parameters %>% add(temp)
  }
  return(parameters)
}

#' Create PMX mapping.
#' 
#' @param theta named integer vector for THETA mapping
#' @param omega named integer vector for THETA mapping
#' @param sigma named integer vector for OMEGA mapping
#' @param auto derive parameter names based on NONMEM equations. Default value
#' is FALSE for backward compatibility.
#' @importFrom assertthat assert_that
#' @importFrom campsismod Parameters
#' @return PMX mapping object
#' @export
mapping <- function(theta=NULL, omega=NULL, sigma=NULL, auto=FALSE) {
  parameters <- Parameters()
  parameters <- addMapping(theta, parameters=parameters, type="theta")
  parameters <- addMapping(omega, parameters=parameters, type="omega")
  parameters <- addMapping(sigma, parameters=parameters, type="sigma")
  
  retValue <- structure(list(
    params=parameters,
    auto=auto
  ), class="pmxmapping")
}

#' Process parameters before return.
#' 
#' @param parameters parameters
#' @return updated parameters
processParameters <- function(parameters) {
  # Delete attributes
  attributes(parameters@list) <- NULL
  
  # Sort parameters
  parameters <- parameters %>% campsismod::sort()
  
  # Fix OMEGA's
  parameters <- tryCatch(
    expr=parameters %>% campsismod::fixOmega(),
    error=function(cond) {
      print(sprintf("Fixing OMEGAs did not work: %s", cond$message))
      return(parameters)
    }
  )

  return(parameters)
}

#' Define and annotate your parameters.
#' 
#' @param model Pharmpy model
#' @param mapping PMX mapping
#' @return parameters definition table
#' @param estimate if TRUE, estimated values are used, if FALSE, initial values are used
#' @importFrom purrr map map2
#' @importFrom campsismod add getByIndex getNONMEMName Parameters sort
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
    namedParameter <- mappingList %>% campsismod::getByIndex(parameter)
    if (length(namedParameter) > 0) {
      parameter@name <- namedParameter@name
    }
    return(parameter)
  })
  
  # Skip parameters validation here
  parameters <- Parameters()
  parameters@list <- list
  
  # Check no parameter is missing (LOOP 2)
  purrr::map(mappingList@list, .f=function(parameter) {
    returnedParameter <- parameters %>% campsismod::getByIndex(parameter)
    if (length(returnedParameter) == 0) {
      parameters <<- parameters %>% campsismod::add(parameter)
    }
  })
  
  if (!estimate) {
    return(processParameters(parameters))
  }
  
  # Reading estimated values with pharmpy
  estimates <- model$modelfit_results$parameter_estimates
  
  if (is.null(estimates)) {
    stop("No NONMEM results are available through Pharmpy")
  }
  
  list <- parameters@list %>% purrr::map(.f=function(param) {
    name <- param %>% campsismod::getNONMEMName()
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
  
  # Skip parameters validation here
  parameters <- Parameters()
  parameters@list <- list
  
  return(processParameters(parameters))
}

#' Retrieve initial values from Pharmpy parameter set.
#' 
#' @param parset Pharmpy parameter set
#' @return S4 parameters object
#' @importFrom purrr map2
#' @importFrom assertthat assert_that
#' @export
retrieveInitialValues <- function(parset) {
  assertthat::assert_that(inherits(parset, "pharmpy.parameter.Parameters"),
                          msg="parset is not a parameter set")
  
  paramsList <- purrr::map2(parset$inits, names(parset$inits), .f=function(initialValue, name) {
    fix <- as.logical(parset$fix[name])
    return(convertNONMEMParameter(name=name, value=initialValue, fix=fix))
  })
  
  # Skip parameters validation because SAME omegas are not returned!
  parameters <- Parameters()
  parameters@list <- paramsList

  return(parameters)
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
