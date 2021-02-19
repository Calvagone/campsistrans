

#' Access estimate and initial values from model.
#' 
#' @param model PMX model
#' @param nmtype NONMEM type: THETA, OMEGA or SIGMA
#' @param estimate if TRUE, estimated values are used, if FALSE, initial values are used
#' @return named vector with the requested parameters
#' @export
rxodeParams <- function(model, nmtype, estimate=TRUE) {
  
  assertthat::assert_that(inherits(model, "pmx_model"),
                          msg="model is not a PMX model")
  table <- model$params$table
  parameters <- table %>% dplyr::filter(type==nmtype & (is.na(diag) | diag))
  
  if (nmtype=="OMEGA") {
    names <- paste0("ETA", "_", parameters %>% dplyr::pull(suffix))
  } else if(nmtype=="SIGMA") {
    names <- paste0("EPS", "_", parameters %>% dplyr::pull(suffix))
  } else {
    names <- paste0(nmtype, "_", parameters %>% dplyr::pull(suffix))
  }
  
  
  if (estimate) {
    if ("estimate" %in% colnames(table)) {
      retValue <- parameters %>% dplyr::pull("estimate")
    } else {
      stop("Parameters table does not contain estimated parameters")
    }
  } else {
    if ("initial_value" %in% colnames(table)) {
      retValue <- parameters %>% dplyr::pull("initial_value")  
    } else {
      stop("Parameters table does not contain initial values")
    }
  }
  
  names(retValue) <- names
  return(retValue)
}

#' Get the THETA vector for RxODE.
#' 
#' @param model PMX model
#' @param estimate if TRUE, estimated values are used, if FALSE, initial values are used
#' @return named vector with THETA values
#' @export
rxodeTheta <- function(model, estimate=TRUE) {
  return(rxodeParams(model=model, nmtype="THETA", estimate=estimate))
}

#' Get the IIV matrix (omega) for RxODE.
#' 
#' @param model PMX model
#' @param estimate if TRUE, estimated values are used, if FALSE, initial values are used
#' @return named matrix with OMEGA values
#' @export
rxodeOmega <- function(model, estimate=TRUE) {
  table <- model$params$table
  diag <- rxodeParams(model=model, nmtype="OMEGA", estimate=estimate)
  diagMatrix <- toDiagonalMatrix(diag)
  
  # Adding ETA correlations to the matrix
  corrEtas <- table %>% dplyr::filter(type=="OMEGA" & (!is.na(diag) & !diag))
  for (rowIndex in seq_len(nrow(corrEtas))) {
    row <- corrEtas[rowIndex,]
    primary_index <- row %>% dplyr::pull(primary_index)
    secondary_index <- row %>% dplyr::pull(secondary_index)
    if (estimate) {
      value <- row %>% dplyr::pull("estimate")
    } else {
      value <- row %>% dplyr::pull(initial_value)
    }
    
    # Matrix is symmetric
    diagMatrix[paste0("ETA_", primary_index), paste0("ETA_", secondary_index)] <- value
    diagMatrix[paste0("ETA_", secondary_index), paste0("ETA_", primary_index)] <- value
  }
  
  return(diagMatrix)
}

#' Get the RUV matrix (sigma) for RxODE.
#' No correlations are possible for now.
#' 
#' @param model PMX model
#' @param estimate if TRUE, estimated values are used, if FALSE, initial values are used
#' @return named matrix with SIGMA values
#' @export
rxodeSigma <- function(model, estimate=TRUE) {
  diag <- rxodeParams(model=model, nmtype="SIGMA", estimate=estimate)
  diagMatrix <- toDiagonalMatrix(diag)
  return(diagMatrix)
}