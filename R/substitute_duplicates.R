
#' Substitute duplicate equation names (using underscore characters).
#' 
#' @param model Campsis model with duplicate equation names
#' @return Campsis model without duplicate equation names
#' @export
substituteDuplicateEquationNames <- function(model) {
  
  retValue <- CampsisModel()
  notDone <- TRUE
  
  while (notDone) {
    tmp <- removeFirstStatement(model)
    model <- tmp$model
    record <- tmp$record
    statement <- tmp$statement
    if (is.null(statement)) {
      notDone <- FALSE
    } else {
      if (is(statement, "equation")) {
        original <- statement@lhs
        if (retValue %>% campsismod::contains(statement)) {
          
          # Find replacement equation
          replacement <- findReplacementEquation(model1=retValue, model2=model, original)
          statement@lhs <- replacement
          
          # Update everywhere
          model <- model %>%
            replaceAll(original, replacement)
        }
      }
      retValue <- retValue %>%
        add(statement, pos=campsismod::Position(record))
    }
  }
  
  # Copy parameters and properties
  retValue@parameters <- model@parameters
  retValue@compartments@properties <- model@compartments@properties
  
  return(retValue)
}

findReplacementEquation <- function(model1, model2, original) {
  replacement <- paste0(original, "_")
  while (model1 %>% campsismod::contains(Equation(replacement)) ||
         model2 %>% campsismod::contains(Equation(replacement))) {
    replacement <- paste0(replacement, "_")
  }
  return(replacement)
}

removeFirstStatement <- function(model) {
  main <- model %>% campsismod::find(MainRecord())
  ode <- model %>% campsismod::find(OdeRecord())
  error <- model %>% campsismod::find(ErrorRecord())
  
  if (is.null(main) || length(main) == 0) {
    if (is.null(ode) || length(ode) == 0) {
      if (is.null(error) || length(error) == 0) {
        return(list(statement=NULL, record=ErrorRecord(), model=model))
      } else {
        removeFirstStatementCore(record=error, model=model)  
      }
    } else {
      removeFirstStatementCore(record=ode, model=model)  
    }
  } else {
    removeFirstStatementCore(record=main, model=model)  
  }
}

removeFirstStatementCore <- function(record, model) {
  statement <- record@statements %>% getByIndex(1L)
  record <- record %>% delete(1L)
  model <- model %>% replace(record)
  record@statements@list <- list()
  return(list(statement=statement, record=record, model=model))
}

#' Check for duplicate equation names.
#' 
#' @param model Campsis model
#' @return Campsis model without duplicate equation names
#' @export
checkForDuplicateEquationNames <- function(model) {
  main <- model %>% campsismod::find(MainRecord())
  ode <- model %>% campsismod::find(OdeRecord())
  error <- model %>% campsismod::find(ErrorRecord())
  
  retValue <- NULL %>%
    append(collectEquationNames(main)) %>%
    append(collectEquationNames(ode)) %>%
    append(collectEquationNames(error))
  
  # Keep duplicates only
  retValue <- retValue[duplicated(retValue)]
  
  return(unique(retValue))
}

collectEquationNames <- function(record) {
  if (is.null(record)) {
    return(NULL)
  } else {
    equations <- record@statements@list %>%
      purrr::keep(~is(.x, "equation")) %>%
      purrr::map_chr(~.x@lhs)
    return(equations)
  }
}
