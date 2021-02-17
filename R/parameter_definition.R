
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
#' @importFrom purrr map2_df
#' @importFrom dplyr bind_rows filter left_join mutate select
params <- function(model, mapping=NULL) {
  
  assertthat::assert_that(inherits(model, "pharmpy.plugins.nonmem.model.Model"),
                          msg="model is not a Pharmpy model")
  if (!is.null(mapping)) {
    assertthat::assert_that(inherits(mapping, "pmx_mapping"),
                            msg="mapping is not a PMX mapping object")
  }
  
  # Retrieve initial values from parset
  parset <- model$parameters
  initialValues <- initialValues(parset)
  thetaNo <- max(initialValues %>% dplyr::filter(type=="THETA") %>% dplyr::pull(primary_index))
  omegaNo <- max(initialValues %>% dplyr::filter(type=="OMEGA") %>% dplyr::pull(primary_index))
  sigmaNo <- max(initialValues %>% dplyr::filter(type=="SIGMA") %>% dplyr::pull(primary_index))
  
  # Build named vectors thetas, omegas and sigmas
  if (!is.null(mapping)) {
    thetas <- mapping$theta
    omegas <- mapping$omega
    sigmas <- mapping$sigma
  }
  if (!exists("thetas") || is.null(thetas)) {
    thetas <- seq_len(thetaNo)
    names(thetas) <- as.character(thetas)
  }
  if (!exists("omegas") || is.null(omegas)) {
    omegas <- seq_len(omegaNo)
    names(omegas) <- as.character(omegas)
  }
  if (!exists("sigmas") || is.null(sigmas)) {
    sigmas <- seq_len(sigmaNo)
    names(sigmas) <- as.character(sigmas)
  }

  # initial parameter table based on used input
  thetaDf <- purrr::map2_df(thetas, names(thetas), .f=function(index, name) {
    type <- "THETA"
    c(nm_name=paste0(type, "(", index, ")"), name=paste0(type, "_", name),
      primary_index=index, secondary_index=NA, type=type, suffix=name, diag=NA)
  })
  omegaDf <- purrr::map2_df(omegas, names(omegas), .f=function(index, name) {
    type <- "OMEGA"
    c(nm_name=paste0(type, "(", index, ",", index, ")"), name=paste0(type, "_", name),
      primary_index=index, secondary_index=index, type=type, suffix=name, diag=TRUE)
  })
  sigmaDf <- purrr::map2_df(sigmas, names(sigmas), .f=function(index, name) {
    type <- "SIGMA"
    c(nm_name=paste0(type, "(", index, ",", index, ")"), name=paste0(type, "_", name),
      primary_index=index, secondary_index=index, type=type, suffix=name, diag=TRUE)
  })

  # Conversion to logical and numeric
  params <- dplyr::bind_rows(thetaDf, omegaDf, sigmaDf) %>% 
    dplyr::mutate(diag=as.logical(diag),
                  primary_index=as.numeric(primary_index),
                  secondary_index=as.numeric(secondary_index))
  
  # Merge initial values, will add automatically the correlated OMEGA's
  nonCorrelationParameters <- initialValues %>% dplyr::filter(is.na(diag) | diag) %>% dplyr::select(nm_name, initial_value, fix)
  correlationParameters <- initialValues %>% dplyr::filter(!is.na(diag) & !diag)
  
  params <- params %>% dplyr::left_join(nonCorrelationParameters, by="nm_name")
  params <- params %>% dplyr::bind_rows(correlationParameters)
  
  # Merging parameter estimates if there is
  estimates <- model$modelfit_results$parameter_estimates
  if (!is.null(estimates)) {
    estimates <- unlist(as.list(model$modelfit_results$parameter_estimates))
    estimates <- data.frame(nm_name=names(estimates), estimate=as.numeric(estimates))
    params <- params %>% dplyr::left_join(estimates, by="nm_name")
  }
  
  retValue <- structure(list(
    table=params
  ), class="pmx_params")
  
  return(retValue)
}

#' Retrieve initial values from Pharmpy parameter set.
#' 
#' @param parset Pharmpy parameter set
#' @return initial values tibble
#' @importFrom purrr map2_df
#' @importFrom dplyr mutate
#' @export
initialValues <- function(parset) {
  assertthat::assert_that(inherits(parset, "pharmpy.parameter.ParameterSet"),
                          msg="parset is not a parameter set")
  
  retValue <- purrr::map2_df(parset$inits, names(parset$inits), .f=function(initial_value, nm_name) {
    fix <- as.logical(parset$fix[nm_name])
    index <- extractValueInParentheses(nm_name)
    
    if (isNMThetaParameter(nm_name)) {
      primary_index <- index
      secondary_index <- NA
      diag <- NA
      type <- "THETA"
      nm_name <- paste0(type, "(", primary_index, ")")
      suffix <- primary_index
      
    } else if (isNMOmegaParameter(nm_name)) {
      indexes <- strsplit(index, ",")
      primary_index <- indexes[[1]][1]
      secondary_index <- indexes[[1]][2]
      diag <- primary_index==secondary_index
      type <- "OMEGA"
      if (diag) {
        suffix <- paste0(primary_index)
      } else {
        suffix <- paste0(primary_index, "_", secondary_index)
      }
      
    } else if (isNMSigmaParameter(nm_name)) {
      indexes <- strsplit(index, ",")
      primary_index <- indexes[[1]][1]
      secondary_index <- indexes[[1]][2]
      diag <- primary_index==secondary_index
      type <- "SIGMA"
      if (diag) {
        suffix <- paste0(primary_index)
      } else {
        suffix <- paste0(primary_index, "_", secondary_index)
      }
      
    } else {
      stop(paste0("Unknown parameter ", nm_name, ": estimated parameter type must be THETA, OMEGA or SIGMA."))
    }
    
    c(nm_name=nm_name, name=paste0(type, "_", suffix), initial_value=initial_value, fix=fix,
      primary_index=primary_index, secondary_index=secondary_index, diag=diag, type=type, suffix=suffix)
  })
  
  retValue <-
    retValue %>% dplyr::mutate(
      diag = as.logical(diag),
      fix = as.logical(fix),
      initial_value = as.numeric(initial_value),
      primary_index = as.numeric(primary_index),
      secondary_index = as.numeric(secondary_index)
    )
  return(retValue)
}
