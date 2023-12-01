
#' Auto-extra parameters from model code.
#' Unused model parameters are automatically deleted.
#' Indexes are automatically re-adjusted.
#' 
#' @param model any Campsis model. All parameters must be named (i.e. not NA)
#' @return updated model
#' @export
autoExtractParameters2 <- function (model) {
  records <- model@model
  parameters <- model@parameters
  
  thetasM <- parameters@list %>% 
    purrr::keep(~is(.x, "theta")) %>%
    purrr::map_chr(~.x@name)
  
  omegasM <- parameters@list %>% 
    purrr::keep(~is(.x, "omega") && is(.x, "double_array_parameter") && (.x@index == .x@index2)) %>%
    purrr::map_chr(~.x@name)
  
  sigmaM <- parameters@list %>% 
    purrr::keep(~is(.x, "sigma") && is(.x, "double_array_parameter") && (.x@index == .x@index2)) %>%
    purrr::map_chr(~.x@name)
  
  # Parameters extraction
  output <- records@list %>%
    purrr::map(.f=~extractParametersCore(.x)) %>%
    purrr::flatten()
  
  thetas <- output %>%
    purrr::map(.f=~.x$thetas) %>%
    purrr::list_c() %>%
    unique()
  
  omegas <- output %>%
    purrr::map(.f=~.x$omegas) %>%
    purrr::list_c() %>%
    unique()
  
  sigmas <- output %>%
    purrr::map(.f=~.x$sigmas) %>%
    purrr::list_c() %>%
    unique()
  
  # Check if parameters are unused (i.e. not there in model code)
  unusedThetas <- thetasM[!thetasM %in% thetas]
  unusedOmegas <- omegasM[!omegasM %in% omegas]
  unusedSigmas <- sigmaM[!sigmaM %in% sigmas]
  
  # Add new parameters
  for (theta in thetas) {
    if (is.null(model %>% campsismod::find(Theta(name=theta)))) {
      model <- model %>%
        add(Theta(name=theta, value=0))
    }
  }
  for (omega in omegas) {
    if (is.null(model %>% campsismod::find(Omega(name=omega)))) {
      model <- model %>%
        add(Omega(name=omega, value=0, type="var"))
    }
  }
  for (sigma in sigmas) {
    if (is.null(model %>% campsismod::find(Sigma(name=sigma)))) {
      model <- model %>%
        add(Sigma(name=sigma, value=0, type="var"))
    }
  }
  
  # Delete unused parameters
  for (theta in unusedThetas) {
    model <- model %>%
      delete(Theta(name=theta))
  }
  for (omega in unusedOmegas) {
    model <- deleteParameterAndCorrelations(model=model, parameter=Omega(name=omega))
  }
  for (sigma in unusedSigmas) {
    model <- deleteParameterAndCorrelations(model=model, parameter=Sigma(name=sigma))
  }
  
  # Repair parameters
  model@parameters <- repairParameters(model@parameters)
  
  return(model)
}

deleteParameterAndCorrelations <- function(model, parameter) {
  parameterInModel <- model %>% campsismod::find(parameter)
  if (is.null(parameterInModel)) {
    return(model)
  }
  indexes <- c(parameterInModel@index, parameterInModel@index2)
  type <- as.character(class(parameter)) # omega or sigma
  model@parameters@list <- model@parameters@list %>%
    purrr::discard(.p=~(as.character(class(.x))==type) && ((.x@index %in% indexes) || (.x@index2 %in% indexes)))
  return(model)
}

extractParametersCore <- function(record) {
  retValue <- record@statements@list %>% purrr::map(.f=function(statement) {
    text <- ""
    if (is(statement, "equation")) {
      text <- statement@rhs
    } else if (is(statement, "if_statement")) {
      text <- paste0(statement@condition, " ", statement@equation@rhs)
    }
    return(list(thetas=extractParameters(text=text, type="THETA"),
                omegas=extractParameters(text=text, type="ETA"),
                sigmas=extractParameters(text=text, type="EPS")))
  })
  return(retValue)
}

extractParameters <- function(text, type) {
  text_ <- paste0(" ", text)
  retValue <- gregexpr(pattern=sprintf("[^a-zA-Z_](%s_)[A-Za-z0-9_]+", type), text=text_)[[1]]
  start <- as.integer(retValue)
  if (length(start)==1 && start==-1) {
    return(character(0))
  }
  length <- attr(retValue, "match.length")
  return(purrr::map2_chr(.x=start, .y=length, .f=~substring(text_, first=.x + nchar(type) + 2, last=.x + .y - 1)))
}

repairParametersByType <- function(parameters, type) {
  # Select parameters according to type
  parameters@list <- parameters@list %>%
    purrr::keep(~is(.x, type))
  
  # Sort parameters
  parameters <- parameters %>% campsismod::sort()
  
  # Thetas or on diag parameters 
  onDiagParameters <- parameters
  onDiagParameters@list <- onDiagParameters@list %>%
    purrr::keep(~!(is(.x, "double_array_parameter") && (.x@index != .x@index2)))
  
  # Off diagonal parameters
  offDiagParameters <- parameters
  offDiagParameters@list <- offDiagParameters@list %>%
    purrr::keep(~is(.x, "double_array_parameter") && (.x@index != .x@index2))
  
  # For each correlation, collect the 2 omegas names
  offDiagParameters_ <- offDiagParameters@list %>%
    purrr::map(~list(parameter=.x,
                    name1=onDiagParameters %>% campsismod::getByIndex(Omega(index=.x@index, index2=.x@index)) %>% .@name,
                    name2=onDiagParameters %>% campsismod::getByIndex(Omega(index=.x@index2, index2=.x@index2)) %>% .@name))
  
  # Collect original indexes
  originalIndexes <- onDiagParameters@list %>% purrr::map_int(~.x@index)
  
  # Compute destination indexes
  destIndexes <- seq_along(originalIndexes)
  
  # Assign new indexes
  retValue <- onDiagParameters
  retValue@list <- retValue@list %>% purrr::map(.f=function(parameter) {
    index1 <- parameter@index  
    which1 <- which(originalIndexes==index1)
    assertthat::assert_that(which1 %>% length()==1, msg="which1 should be length 1")
    parameter@index <- destIndexes[which1]
    
    if (is(parameter, "double_array_parameter")) {
      index2 <- parameter@index2
      which2 <- which(originalIndexes==index2)
      assertthat::assert_that(which2 %>% length()==1, msg="which2 should be length 1")
      parameter@index2 <- destIndexes[which2]
    }
    return(parameter)
  })

  # Add correlations back
  for (offDiagParameter_ in offDiagParameters_) {
    offDiagParameter <- offDiagParameter_$parameter
    name1 <- offDiagParameter_$name1
    name2 <- offDiagParameter_$name2
    offDiagParameter@index <- retValue %>% campsismod::find(Omega(name=name1)) %>% .@index
    offDiagParameter@index2 <- retValue %>% campsismod::find(Omega(name=name2)) %>% .@index
    retValue <- retValue %>%
      add(offDiagParameter)
  }
  
  # Sort parameters (since correlations are put at the end)
  retValue <- retValue %>% campsismod::sort()

  return(retValue)
}

repairParameters <- function(parameters) {
  thetas <- repairParametersByType(parameters, type="theta")
  omegas <- repairParametersByType(parameters, type="omega")
  sigmas <- repairParametersByType(parameters, type="sigma")
  parameters@list <- c(thetas@list, omegas@list, sigmas@list)
  return(parameters)
}

