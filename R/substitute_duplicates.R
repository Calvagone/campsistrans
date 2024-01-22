
#' Substitute duplicate equations (using underscore characters).
#' 
#' @export
substituteDuplicates <- function(model) {
  
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
          replacement <- findReplacementEquation(retValue, original)
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

findReplacementEquation <- function(model, original) {
  replacement <- paste0(original, "_")
  while (model %>% campsismod::contains(Equation(replacement))) {
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

checkForDuplicateEquations <- function(model) {
  main <- model %>% campsismod::find(MainRecord())
  ode <- model %>% campsismod::find(OdeRecord())
  error <- model %>% campsismod::find(ErrorRecord())
  
  retValue <- NULL
  retValue <- retValue %>%
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
