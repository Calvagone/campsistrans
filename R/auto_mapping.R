

#' Check if the given parameter needs to be renamed.
#' 
#' @param parameter a parameter
#' @return a logical value
#' 
needsAutoRenaming <- function(parameter) {
  cond1 <- is.na(parameter@name)
  cond2 <- TRUE
  if (is(parameter, "double_array_parameter")) {
    cond2 <- parameter %>% campsismod::isDiag()
  }
  return(cond1 && cond2)
}

#' Auto-rename parameters based on equations.
#' 
#' @param model CAMPSIS model
#' @param mapping mapping object
#' @return CAMPSIS model with updated parameters
#' @importFrom dplyr group_split mutate
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' 
autoRenameParameters <- function(model, mapping) {
  # Don't run function if auto-mapping is disabled
  if (!mapping$auto) {
    return(model)
  }
  
  # Retrieve all unnamed parameters
  unnamedParameters <- model@parameters@list %>% purrr::keep(.p=~.x %>% needsAutoRenaming())
  
  # Create mapping table
  mappingTable <- unnamedParameters %>% purrr::map_df(.f=function(x) {
    nameInModel <- x %>% getNameInModel()
    name_ <- searchCandidateName(model, x)
    return(tibble::tibble(NAME=nameInModel, TYPE=class(x) %>% as.character(), CANDIDATE_NAME=name_))
  })
  
  # Add indexes if candidate names are identical for the same parameter type
  # Especially useful for IOV
  mappingTable <- mappingTable %>% dplyr::group_split(TYPE, CANDIDATE_NAME) %>% purrr::map_df(.f=function(x) {
    if (nrow(x) > 1) {
      x <- x %>% dplyr::mutate(CANDIDATE_NAME=paste0(CANDIDATE_NAME, "_", seq_len(nrow(x))))
    }
    return(x)
  })
  
  # Rename parameters and replace in model
  model@parameters@list <- model@parameters@list %>% purrr::map(.f=function(x) {
    if (!(x %>% needsAutoRenaming())) {
      return(x)
    }
    nameInModel <- x %>% getNameInModel()
    mappingRow <- mappingTable %>% dplyr::filter(NAME==nameInModel)
    candidateName <- mappingRow$CANDIDATE_NAME
    x@name <- candidateName
    model <<- model %>% replaceAll(pattern=nameInModel, replacement=x %>% getNameInModel())
    return(x)
  })
 
  return(model) 
}

#' Seach candidate name.
#' 
#' @param model CAMPSIS model
#' @param parameter parameter to search for a name in model
#' @return a candidate name or NA if nothing was found
#' 
searchCandidateName <- function(model, parameter) {
  parameterName <- parameter %>% getNameInModel()
  
  # Retrieve statements from MAIN and ERROR
  list <- list()
  main <- model %>% find(MainRecord())
  if (!is.null(main)) {
    list <- c(list, main@statements@list)
  }
  error <- model %>% find(ErrorRecord())
  if (!is.null(error)) {
    list <- c(list, error@statements@list)
  }
  pattern <- VariablePattern(parameterName)
  
  # Detect first match
  equation <- list %>% purrr::detect(.f=function(x) {
    if (is(x, "equation")) {
      rhs <- x@rhs %>% replaceAll(pattern=pattern, replacement="")
      hasParam <- rhs %>% nchar() != x@rhs %>% nchar()
      return(hasParam)
    } else {
      return(FALSE)
    }
  })
  if (is.null(equation)) {
    return(NA)
  } else {
    return(equation@lhs)
  }
}
