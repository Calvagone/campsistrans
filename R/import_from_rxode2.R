
#' Extract model code from an rxode2 function object.
#' 
#' @param rxModel the rxode2 function object
#' @return a functional Campsis model
#' 
importRxode2 <- function(rxModel) {
  
  # Extract model code
  model <- extractModelCodeFromRxode(rxModel)
  
  # Extract compartment properties
  model <- model %>%
    extractCompartmentPropertiesFromRxode()
}

#' Extract model code from an rxode2 function object.
#' 
#' @param rxModel the rxode2 function object
#' @return a Campsis model with all original rxode2 statements in the ODE block and detected compartments
#' 
extractModelCodeFromRxode <- function(rxModel) {
  code <- strsplit(rxModel$funTxt, "\n")[[1]]
  
  # Remove compartment declarations if any
  # Not sure at this stage if this is needed
  code <- gsub(pattern="cmt\\s*\\(\\s*\\w+\\s*\\)", replacement="", x=code)
  code <- code[code!=""]
  
  # Replace R assignments by equals
  code <- gsub(pattern="<-", replacement="=", x=code)
  
  # Detect compartments based on d/dt
  cmtNames <- code %>% purrr::map_chr(function(x) {
    if (isODE(x)) {
      return(extractTextBetweenBrackets(extractLhs(x)))
    } else {
      return("")
    }
  }) %>% purrr::discard(~ .x=="") %>% unique()
  
  # Add A_ prefix to compartment names
  for (cmtName in cmtNames) {
    code <- replaceAll(object=code, pattern=VariablePattern(cmtName),
                       replacement=sprintf("A_%s", cmtName))
  }
  
  # Parse code using campsismod
  statements <- campsismod:::parseStatements(code)
  
  # Create raw Campsis model, put all statements in ODE record, and update compartments
  model <- CampsisModel()
  ode <- OdeRecord()
  ode@statements <- statements
  model <- model %>%
    add(ode) %>%
    updateCompartments()
  
  return(model)
}

#' Extract compartment properties from rxode2 statements.
#' 
#' @param model a Campsis model with all original rxode2 statements in the ODE block
#' @return a Campsis model with updated compartment properties
#' 
extractCompartmentPropertiesFromRxode <- function(model) {
  ode <- model %>%
    find(OdeRecord())
  
  # Compartment properties indexes
  indexes <- ode@statements@list %>%
    purrr::map_lgl(~is(.x, "unknown_statement") && isRxodeCompartmentPropertyEquation(.x@line)) %>%
    which()
  
  # Extract compartment properties
  compartmentProperties <- ode@statements@list[indexes] %>%
    purrr::map(~{
      lhs <- extractLhs(.x@line)
      compartmentNameWithA <- extractTextBetweenBrackets(lhs)
      rhs <- extractRhs(.x@line) %>% trimws()
      compartmentIndex <- getCompartmentIndex(object=model, name=gsub(pattern="A_", replacement="", x=compartmentNameWithA))
      
      if (startsWith(lhs, "f")) {
        return(Bioavailability(compartment=compartmentIndex, rhs=rhs))
      } else if (startsWith(lhs, "alag")) {
        return(LagTime(compartment=compartmentIndex, rhs=rhs))
      } else if (startsWith(lhs, "dur")) {
        return(InfusionDuration(compartment=compartmentIndex, rhs=rhs))
      } else if (startsWith(lhs, "rate")) {
        return(InfusionRate(compartment=compartmentIndex, rhs=rhs))
      } else {
        stop("Should never occur since left-hand side is checked by regex") 
      }
    })
  model@compartments@properties@list <- compartmentProperties
  
  # Remove compartment properties from code
  ode@statements@list <- ode@statements@list[-indexes]
  model <- model %>%
    replace(ode)
  
  return(model)
}

isRxodeCompartmentPropertyEquation <- function(x) {
  assertSingleCharacterString(x)
  parts <- strsplit(x, split="=")[[1]]
  if (length(parts) == 1) {
    return(FALSE)
  }
  return(grepl(pattern=paste0("^(f|alag|dur|rate)\\(", campsismod:::variablePatternStr(),
                              "\\)$"), x=parts[1] %>% trim()))
}
