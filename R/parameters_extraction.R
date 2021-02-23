

isNamedNumeric <- function(x) {
  
  assertthat::assert_that(is.character(names(x)), msg="x is not named")
}

#' Create PMX mapping.
#' 
#' @param theta named integer vector for THETA mapping
#' @param omega named integer vector for THETA mapping
#' @param sigma named integer vector for OMEGA mapping
#' @importFrom assertthat assert_that
#' @importFrom pmxmod addParameter
#' @importFrom purrr map2
#' @return PMX mapping object
#' @export
mapping <- function(theta=NULL, omega=NULL, sigma=NULL) {
  params <- new("parameters", list=list())
  if (!is.null(theta)) {
    assertthat::assert_that(is.numeric(theta), msg="theta is not numeric")
    names <- if (is.character(names(theta))) {names(theta)} else {rep(NA, length(theta))}
    purrr::map2(theta, names, .f=function(index, name) {
      params <<- params %>% pmxmod::addParameter(
        new("theta", name=as.character(name), index=as.integer(index), value=as.numeric(NA), fix=NA)
        )
    })
  }
  if (!is.null(omega)) {
    assertthat::assert_that(is.numeric(omega), msg="omega is not numeric")
    names <- if (is.character(names(omega))) {names(omega)} else {rep(NA, length(omega))}
    purrr::map2(omega, names, .f=function(index, name) {
      params <<- params %>% pmxmod::addParameter(
        new("omega", name=as.character(name), index=as.integer(index), index2=as.integer(index), value=as.numeric(NA), fix=NA)
        )
    })
  }
  if (!is.null(sigma)) {
    assertthat::assert_that(is.numeric(sigma), msg="sigma is not numeric")
    names <- if (is.character(names(sigma))) {names(sigma)} else {rep(NA, length(sigma))}
    purrr::map2(sigma, names, .f=function(index, name) {
      params <<- params %>% pmxmod::addParameter(
        new("sigma", name=as.character(name), index=as.integer(index), index2=as.integer(index), value=as.numeric(NA), fix=NA)
        )
    })
  }
  retValue <- structure(list(
    params=params
  ), class="pmx_mapping")
}


#' Define and annotate your parameters.
#' 
#' @param model Pharmpy model
#' @param mapping PMX mapping
#' @return parameters definition table
#' @param estimate if TRUE, estimated values are used, if FALSE, initial values are used
#' @importFrom purrr map map2
#' @importFrom pmxmod clean getNONMEMName hasParameter order
#' @export
extractParameters <- function(model, mapping, estimate) {
  
  assertthat::assert_that(inherits(model, "pharmpy.plugins.nonmem.model.Model"),
                          msg="model is not a Pharmpy model")
  if (!is.null(mapping)) {
    assertthat::assert_that(inherits(mapping, "pmx_mapping"),
                            msg="mapping is not a PMX mapping object")
  }
  
  # Retrieve parameters from mapping
  mappingList <- mapping$params
  
  # Retrieve initial values from parset
  parset <- model$parameters
  pharmpyList <- initialValues(parset)
  
  # Collect names from mapping list (LOOP 1)
  list <- purrr::map(pharmpyList@list, .f=function(parameter) {
    namedParameter <- mappingList %>% pmxmod::hasParameter(parameter)
    if (length(namedParameter) > 0) {
      parameter@name <- namedParameter@name
    }
    return(parameter)
  })
  params <- new("parameters", list=list)
  
  # Check no parameter is missing (LOOP 2)
  purrr::map(mappingList@list, .f=function(parameter) {
    returnedParameter <- params %>% pmxmod::hasParameter(parameter)
    if (length(returnedParameter) == 0) {
      params <<- params %>% pmxmod::addParameter(parameter)
    }
  })
  
  if (!estimate) {
    return(params %>% pmxmod::clean() %>% pmxmod::order())
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
  
  return(new("parameters", list=list) %>% pmxmod::clean() %>% pmxmod::order())
}

#' Retrieve initial values from Pharmpy parameter set.
#' 
#' @param parset Pharmpy parameter set
#' @return S4 parameters object
#' @importFrom purrr map2
#' @importFrom assertthat assert_that
#' @export
initialValues <- function(parset) {
  assertthat::assert_that(inherits(parset, "pharmpy.parameter.ParameterSet"),
                          msg="parset is not a parameter set")
  
  params <- purrr::map2(parset$inits, names(parset$inits), .f=function(initial_value, nm_name) {
    fix <- as.logical(parset$fix[nm_name])
    index <- extractValueInParentheses(nm_name)
    isTheta <- isNMThetaParameter(nm_name)
    isOmega <- isNMOmegaParameter(nm_name)
    isSigma <- isNMSigmaParameter(nm_name)
    
    if (isTheta) {
      param <- new("theta", name=as.character(NA), index=as.integer(index), value=initial_value, fix=fix)

    } else if (isOmega || isSigma) {
      indexes <- strsplit(index, ",")
      index1 <- indexes[[1]][1]
      index2 <- indexes[[1]][2]
      className <- if(isOmega) {"omega"} else {"sigma"}
      param <- new(className, name=as.character(NA), index=as.integer(index1), index2=as.integer(index2), value=initial_value, fix=fix)
      
    } else {
      stop(paste0("Unknown parameter ", nm_name, ": estimated parameter type must be THETA, OMEGA or SIGMA."))
    }
    return(param)
  })
  
  return(new("parameters", list=params))
}
