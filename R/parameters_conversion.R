
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
  parameters <- parameters %>% campsismod::fixOmega()
  
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
  
  assertthat::assert_that(inherits(model, "pharmpy.model.model.Model"),
                          msg="model is not a Pharmpy model")
  if (!is.null(mapping)) {
    assertthat::assert_that(inherits(mapping, "pmxmapping"),
                            msg="mapping is not a PMX mapping object")
  }
  
  # Retrieve parameters from mapping
  mappingList <- mapping$params
  
  # Retrieve initial values
  pharmpyList <- retrieveInitialValues(model)
  
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
#' @param model Pharmpy model
#' @return S4 parameters object
#' @importFrom purrr map2
#' @importFrom assertthat assert_that
#' @export
retrieveInitialValues <- function(model) {
  parset <- model$parameters
  assertthat::assert_that(inherits(parset, "pharmpy.model.parameters.Parameters"),
                          msg="parset is not a parameter set")
  
  paramsList <- NULL
  indexes <- seq_along(parset$names)
  for (index in indexes) {
    name <- parset$names[index]
    originalName <- getOriginalParameterName(name=name, model=model, index=index)
    fix <- as.logical(parset$fix[index])
    initialValue <- as.numeric(parset$inits[index])
    paramsList <- paramsList %>% append(convertNONMEMParameter(name=name, originalName=originalName, value=initialValue, fix=fix))
  }

  # Skip parameters validation because SAME omegas are not returned!
  parameters <- Parameters()
  parameters@list <- paramsList

  return(parameters)
}

# leftJoinOriginalParameterNames <- function(parset, model) {
#   rownames <- parset$names
#   originalNames <- seq_along(rownames) %>% purrr::map_chr(~getOriginalParameterName(name=rownames[.x], model=model, index=.x))
#   parset <- parset %>%
#     dplyr::mutate(original_name=originalNames)
#   return(parset)
# }

getOriginalParameterName <- function(name, model, index) {

  etas <- model$random_variables$etas
  etaNames <- seq_along(etas) %>% purrr::map_chr(~as.character(etas[[.x - 1]]$names))
  etaParameterNames <- seq_along(etas) %>% purrr::map_chr(~as.character(etas[[.x - 1]]$parameter_names))
  etaIndex <- which(etaParameterNames==name)
  if (length(etaIndex) > 0) {
    return(toNONMEMStyle(etaNames[etaIndex]))
  }
  
  epsilons <- model$random_variables$epsilons
  epsilonNames <- seq_along(epsilons) %>% purrr::map_chr(~as.character(epsilons[[.x - 1]]$names))
  epsilonParameterNames <- seq_along(epsilons) %>% purrr::map_chr(~as.character(epsilons[[.x - 1]]$parameter_names))
  epsilonIndex <- which(epsilonParameterNames==name)
  if (length(epsilonIndex) > 0) {
    return(toNONMEMStyle(epsilonNames[epsilonIndex]))
  }
  
  retValue <- name
  
  # If not off-diagonal element -> we deduce it is a THETA
  if (!isPharmpyOmegaParameter(name) && !isPharmpySigmaParameter(name)) {
    retValue <- paste0("THETA_", index)
  }

  return(toNONMEMStyle(retValue))
}

#' Convert NONMEM parameter (string form) to campsismod parameter.
#' 
#' @param name NONMEM parameter name, character value
#' @param originalName indexed name, i.e. THETA_1, ETA_1, EPS_1
#' @param value parameter value
#' @param fix is fixed or not, logical value
#' @return S4 parameters object
#' @importFrom campsismod Theta Omega Sigma
#' @export
convertNONMEMParameter <- function(name, originalName, value, fix) {
  print(originalName)
  index <- extractValueInParentheses(originalName)
  isTheta <- isNMThetaParameter(originalName)
  isOmega <- isNMOmegaParameter(originalName)
  isSigma <- isNMSigmaParameter(originalName)
  
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
    stop(sprintf("Unknown parameter %s (%s): estimated parameter type must be THETA, OMEGA or SIGMA.", name, originalName))
  }
  return(param)
}
