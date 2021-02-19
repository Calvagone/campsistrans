
#' Create PMX mapping.
#' 
#' @param theta named integer vector for THETA mapping
#' @param omega named integer vector for THETA mapping
#' @param sigma named integer vector for OMEGA mapping
#' @return PMX mapping object
#' @export
mapping <- function(theta=NULL, omega=NULL, sigma=NULL) {
  if (!is.null(theta)) {
    assertthat::assert_that(is.numeric(theta), msg="theta is not numeric")
    assertthat::assert_that(is.character(names(theta)), msg="theta is not named")
  }
  if (!is.null(omega)) {
    assertthat::assert_that(is.numeric(omega), msg="omega is not numeric")
    assertthat::assert_that(is.character(names(omega)), msg="omega is not named")
  }
  if (!is.null(sigma)) {
    assertthat::assert_that(is.numeric(sigma), msg="sigma is not numeric")
    assertthat::assert_that(is.character(names(sigma)), msg="sigma is not named")
  }
  retValue <- structure(list(
    theta=theta,
    omega=omega,
    sigma=sigma
  ), class="pmx_mapping")
}
  

#' Define and annotate your parameters.
#' 
#' @param model Pharmpy model
#' @param mapping PMX mapping
#' @return parameters definition table
#' @param estimate if TRUE, estimated values are used, if FALSE, initial values are used
#' @importFrom purrr map map2
params <- function(model, mapping, estimate) {
  
  assertthat::assert_that(inherits(model, "pharmpy.plugins.nonmem.model.Model"),
                          msg="model is not a Pharmpy model")
  if (!is.null(mapping)) {
    assertthat::assert_that(inherits(mapping, "pmx_mapping"),
                            msg="mapping is not a PMX mapping object")
  }
  
  # Retrieve initial values from parset
  parset <- model$parameters
  params <- initialValues(parset)
  
  # Build named vectors thetas, omegas and sigmas
  if (!is.null(mapping)) {
    thetas <- mapping$theta
    omegas <- mapping$omega
    sigmas <- mapping$sigma
  }
  if (!exists("thetas") || is.null(thetas)) {
    thetas <- params %>% maxIndex(type="theta") %>% seq_len()
    names(thetas) <- rep("", length(thetas))
  }
  if (!exists("omegas") || is.null(omegas)) {
    omegas <- params %>% maxIndex(type="omega") %>% seq_len()
    names(omegas) <- rep("", length(omegas))
  }
  if (!exists("sigmas") || is.null(sigmas)) {
    sigmas <- params %>% maxIndex(type="sigma") %>% seq_len()
    names(sigmas) <- rep("", length(sigmas))
  }

  # Adding suffix to initial params based on mapping
  thetas <- purrr::map2(thetas, names(thetas), .f=function(index, suffix) {
    suffix_ <- ifelse(suffix=="", character(), suffix)
    param <- params %>% getParameter(type="theta", index=as.integer(index))
    if (length(param)==0) {
      param <- new("theta", name=character(), index=as.integer(index), suffix=character(), fix=NA, value=0)
      warning(paste0("THETA ", index, " was not present in Pharmpy\n"))
    }
    param@suffix <- suffix_
    return(param)
  })
  omegas <- purrr::map2(omegas, names(omegas), .f=function(index, suffix) {
    suffix_ <- ifelse(suffix=="", character(), suffix)
    param <- params %>% getParameter(type="omega", index=as.integer(index), index2=as.integer(index))
    if (length(param)==0) {
      param <- new("omega", name=character(), index=as.integer(index), index2=as.integer(index), suffix=character(), fix=NA, value=0)
      warning(paste0("OMEGA ", index, " was not present in Pharmpy\n"))
    }
    param@suffix <- suffix_
    return(param)
  })
  sigmas <- purrr::map2(sigmas, names(sigmas), .f=function(index, suffix) {
    suffix_ <- ifelse(suffix=="", character(), suffix)
    param <- params %>% getParameter(type="sigma", index=as.integer(index), index2=as.integer(index))
    if (length(param)==0) {
      param <- new("sigma", name=character(), index=as.integer(index), index2=as.integer(index), suffix=character(), fix=NA, value=0)
      warning(paste0("SIGMA ", index, " was not present in Pharmpy\n"))
    }
    param@suffix <- suffix_
    return(param)
  })
  
  params <- new("parameters", list=c(thetas, omegas, sigmas))

  
  if (!estimate) {
    return(params)
  }
  
  # Reading estimated values with pharmpy
  estimates <- model$modelfit_results$parameter_estimates
  
  if (is.null(estimates)) {
    stop("No NONMEM results are available through Pharmpy")
  }
  
  estimatedParams <- params@list %>% purrr::map(.f=function(param) {
    name <- param %>% getNONMEMName()
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
  
  return(estimatedParams)
}

#' Retrieve initial values from Pharmpy parameter set.
#' 
#' @param parset Pharmpy parameter set
#' @return S4 parameters object
#' @importFrom purrr map2
#' @importFrom dplyr mutate
#' @export
initialValues <- function(parset) {
  assertthat::assert_that(inherits(parset, "pharmpy.parameter.ParameterSet"),
                          msg="parset is not a parameter set")
  
  params <- purrr::map2(parset$inits, names(parset$inits), .f=function(initial_value, nm_name) {
    fix <- as.logical(parset$fix[nm_name])
    index <- extractValueInParentheses(nm_name)
    
    if (isNMThetaParameter(nm_name)) {
      param <- new("theta", name=character(), index=as.integer(index), suffix=character(), fix=fix, value=initial_value)

    } else if (isNMOmegaParameter(nm_name)) {
      indexes <- strsplit(index, ",")
      index1 <- indexes[[1]][1]
      index2 <- indexes[[1]][2]
      param <- new("omega", name=character(), index=as.integer(index1), index2=as.integer(index2), suffix=character(), fix=fix, value=initial_value)
      
    } else if (isNMSigmaParameter(nm_name)) {
      indexes <- strsplit(index, ",")
      index1 <- indexes[[1]][1]
      index2 <- indexes[[1]][2]
      param <- new("sigma", name=character(), index=as.integer(index1), index2=as.integer(index2), suffix=character(), fix=fix, value=initial_value)
      
    } else {
      stop(paste0("Unknown parameter ", nm_name, ": estimated parameter type must be THETA, OMEGA or SIGMA."))
    }
    return(param)
  })
  
  return(new("parameters", list=params))
}
