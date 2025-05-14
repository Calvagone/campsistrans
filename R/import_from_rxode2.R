
#' Extract model code from an rxode2 function object.
#' 
#' @param rxmod the rxode2 function object
#' @param rem_pop_suffix remove the '_pop' suffix from the population parameters. The model code is adapted accordingly.
#' @param rem_omega_prefix remove the 'omega_' prefix from the OMEGA parameters. The model code is adapted accordingly.
#' @param subroutine optional subroutine, integer vector of 2 values: ADVAN and TRANS.
#' @return a functional Campsis model
#' @export
importRxode2 <- function(rxmod, rem_pop_suffix=FALSE, rem_omega_prefix=FALSE, subroutine=NULL) {
  # Extract model code
  model <- extractModelCodeFromRxode(rxmod=rxmod, subroutine=subroutine)
  
  # Extract compartment properties
  model <- extractCompartmentPropertiesFromRxode(model)

  # Extract initial conditions
  model <- extractInitialConditionsFromRxode(model)
  
  # Convert complex if statements into simple ones
  model <- model %>%
    convertComplexIfElseStatements()

  # Extract parameters
  model@parameters <- extractParametersFromRxode(rxmod,
                                                 rem_pop_suffix=rem_pop_suffix,
                                                 rem_omega_prefix=rem_omega_prefix)
  
  # Rename THETAs in model code
  for (parameter in model@parameters@list %>% purrr::keep(~is(.x, "theta"))) {
    oldNameInCode <- parameter@name
    
    # Rebuild old name in code
    if (!is.na(parameter@comment) && parameter@comment=="Population parameter" && rem_pop_suffix) {
      oldNameInCode <- paste0(oldNameInCode, "_pop")
    }
    # Replace in model code
    model <- model %>%
      replaceAll(VariablePattern(oldNameInCode), sprintf("THETA_%s", parameter@name))
  }
  
  # Rename ETAs in model code
  for (parameter in model@parameters@list %>% purrr::keep(~is(.x, "omega"))) {
    if (!parameter %>% campsismod::isDiag()) {
      next
    }
    
    # Rebuild old name in code
    if (rem_omega_prefix) {
      oldNameInCode <- sprintf("omega_%s", parameter@name)
    } else {
      oldNameInCode <- parameter@name
    }
    
    # Replace in model code
    model <- model %>%
      replaceAll(VariablePattern(oldNameInCode), sprintf("ETA_%s", parameter@name))
  }
  
  # Heuristic move to MAIN block
  model <- model %>%
    heuristicMoveToMain()
  
  # Process error model
  model <- model %>%
    convertRxodeErrorModel()
  
  # Substitute duplicate equation names
  model <- model %>%
    substituteDuplicateEquationNames()
  
  # Sort everything in the model for consistency (especially in non-regression tests)
  model <- model %>%
    campsismod::sort()

  return(model)
}

#' Extract model code from an rxode2 function object.
#' Note that all compartment names will be prefixed with 'A_'.
#' 
#' @param rxmod the rxode2 function object
#' @param subroutine NULL by default
#' @return a Campsis model with all original rxode2 statements in the ODE block and detected compartments
#' @importFrom rly lex yacc
#' 
extractModelCodeFromRxode <- function(rxmod, subroutine) {
  code <- strsplit(rxmod$funTxt, "\n")[[1]]
  
  # Remove compartment declarations if any
  # Not sure at this stage if this is needed
  code <- gsub(pattern="^cmt\\s*\\(.*\\)\\s*$", replacement="", x=code)
  code <- code[code!=""]
  
  print(code)
  
  # Replace R assignments by equals
  code <- gsub(pattern="<-", replacement="=", x=code)
  
  # Retrieve compartments
  cmtNames <- rxmod$stateDf[, "Compartment Name"]

  # Possibly replace strange coding of compartment properties (especially with the dot)
  for (cmtIndex in seq_along(cmtNames)) {
    # Fractions
    code <- replaceAll(object=code, pattern=VariablePattern(sprintf("rxf.rxddta%s\\.", cmtIndex)),
                       replacement=sprintf("F_A%s", cmtIndex))
    # Initial conditions
    code <- replaceAll(object=code, pattern=VariablePattern(sprintf("rxini.rxddta%s\\.", cmtIndex)),
                       replacement=sprintf("INIT_A%s", cmtIndex))
    # Infusion durations
    code <- replaceAll(object=code, pattern=VariablePattern(sprintf("rxdur.rxddta%s\\.", cmtIndex)),
                       replacement=sprintf("DUR_A%s", cmtIndex))
  }
  
  # Add A_ prefix to compartment names
  for (cmtName in cmtNames) {
    code <- replaceAll(object=code, pattern=VariablePattern(cmtName),
                       replacement=sprintf("A_%s", cmtName))
  }

  # Parse code using campsismod
  lexer  <- rly::lex(Rxode2Lexer)
  parser <- rly::yacc(Rxode2Parser)
  
  # browser()
  list <- parser$parse(paste0(code, collapse="\n"), lexer)
  
  # Create raw Campsis model, put all statements in ODE record, and update compartments
  model <- CampsisModel()
  ode <- OdeRecord()
  ode@statements@list <- list
  
  model <- model %>%
    add(ode)
  
  # Add subroutine if any
  if (!is.null(subroutine)) {
    assertthat::assert_that(length(subroutine)==2L, msg="Subroutine should be a vector of 2 integers")
    subroutineModel <- getSubroutineModelForRxode2(advan=subroutine[1], trans=subroutine[2])
    if (is.null(subroutineModel)) {
      warning("ODEs are not available for the given subroutine")
    } else {
      model <- replaceLinCmt(model=model, subroutineModel=subroutineModel)
    }
  }
  
  # Index compartments
  model <- model %>%
    updateCompartments()
  
  # Automatically convert time to t
  model <- model %>%
    replaceAll("time", "t")
  
  return(model)
}

#' The goal is to convert a complex if statement structure into simple if statements.
#' 
#' @param model original Campsis model
#' @return a Campsis model without complex if statements
#' 
convertComplexIfElseStatements <- function(model) {
  # Retrieve all ODE statements
  ode <- model %>% find(OdeRecord())
  
  # Modify all complex if statements
  ode@statements@list <- ode@statements@list %>% purrr::map(.f=function(x) {
    if (is(x, "complex_if_else_statement")) {
      return(convertComplexIfElseStatement(x))
    } else {
      return(x)
    }
  }) %>% unlist()

  # Update ODE record in model
  model <- model %>% replace(ode)
  
  return(model)
}

convertComplexIfElseStatement <- function(x) {
  retValue <- list()
  ifStatement <- x@list %>%
    purrr::detect(.f=~is(.x, "extended_if_statement"))
  elseIfStatements <- x@list %>%
    purrr::keep(.p=~is(.x, "else_if_statement"))
  elseStatement <- x@list %>%
    purrr::detect(.f=~is(.x, "else_statement"))
  
  assertthat::assert_that(!is.null(ifStatement), msg="Complex if statement without if clause")

  retValue <- retValue %>%
    append(unwrapExtentedIfStatement(elseStatement)) %>%
    append(unwrapExtentedIfStatement(ifStatement)) %>%
    append(elseIfStatements %>% purrr::map(~unwrapExtentedIfStatement(.x))) %>%
    unlist()
  
  return(retValue)
}

unwrapExtentedIfStatement <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  statements <- x@statements
  elseClause <- is(x, "else_statement")
  fixmeIndex <- 0L
  
  # Only keep equations for now (+ convert unknown statements to FIXME equations)
  statements@list <- statements@list %>%
    purrr::map(.f=function(statement) {
      if (is(statement, "equation")) {
        return(statement)
      } else if (is(statement, "unknown_statement")) {
        fixmeIndex <<- fixmeIndex + 1L
        return(Equation(lhs=sprintf("FIXME%i", fixmeIndex), rhs="0", comment=statement@line))
      } else {
        return(statement)
      }
    }) %>%
    purrr::keep(~is(.x, "equation"))
  
  # Create a list of simpled if statements
  retValue <- statements@list %>%
    purrr::map(.f=function(equation) {
      if (elseClause) {
        return(equation)
      } else {
        return(IfStatement(condition=x@condition, equation=equation))
      }
    })
  return(retValue)
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
  
  # Return model if no compartment properties
  if (length(indexes)==0) {
    return(model)
  }
  
  # Extract compartment properties
  compartmentProperties <- ode@statements@list[indexes] %>%
    purrr::map(~{
      lhs <- extractLhs(.x@line)
      compartmentNameWithA <- extractTextBetweenBrackets(lhs)
      rhs <- extractRhs(.x@line) %>% trimws()
      compartmentIndex <- tryCatch({
        getCompartmentIndex(object=model, name=gsub(pattern="A_", replacement="", x=compartmentNameWithA))
      }, error = function(e) {
        warning(sprintf("Compartment %s not found in model, compartment property linked to first compartment.", compartmentNameWithA))
        return(1L)
      })
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
  model@compartments@properties@list <- list()
  
  # Add compartment properties to model
  # Check property wasn't added yet to due issue in monolix2rx package when the same model is imported twice
  # See issue #70
  for (compartmentProperty in compartmentProperties) {
    if (!model %>% campsismod::contains(compartmentProperty)) {
      model <- model %>%
        add(compartmentProperty)
    }
  }
  
  # Remove compartment properties from code
  ode@statements@list <- ode@statements@list[-indexes]
  model <- model %>%
    replace(ode)
  
  return(model)
}

#' Extract initial conditions from rxode2 statements.
#' 
#' @param model a Campsis model with all original rxode2 statements in the ODE block
#' @return a Campsis model with updated initial conditions
#' 
extractInitialConditionsFromRxode <- function(model) {
  ode <- model %>%
    find(OdeRecord())
  
  # Compartment properties indexes
  indexes <- ode@statements@list %>%
    purrr::map_lgl(~is(.x, "unknown_statement") && isRxodeInitialConditionEquation(.x@line)) %>%
    which()
  
  # Return model if no compartment properties
  if (length(indexes)==0) {
    return(model)
  }
  
  # Extract initial conditions
  initialConditions <- ode@statements@list[indexes] %>%
    purrr::map(~{
      lhs <- extractLhs(.x@line)
      compartmentNameWithA <- sub("\\(.*\\)", "", lhs) %>% trimws()
      rhs <- extractRhs(.x@line) %>% trimws()
      compartmentIndex <- getCompartmentIndex(object=model, name=gsub(pattern="A_", replacement="", x=compartmentNameWithA))
      return(InitialCondition(compartment=compartmentIndex, rhs=rhs))
    })
  model@compartments@properties@list <- c(model@compartments@properties@list, initialConditions)
  
  # Remove initial conditions from code
  ode@statements@list <- ode@statements@list[-indexes]
  model <- model %>%
    replace(ode)
  
  return(model)
}

#' Extract all parameters from an rxode2 function object.
#' 
#' @param rxmod the rxode2 function object
#' @param rem_pop_suffix remove the '_pop' suffix from the population parameters.
#' @param rem_omega_prefix remove the 'omega_' prefix from the OMEGA parameters.
#' @return a functional Campsis model
#' @export
extractParametersFromRxode <- function(rxmod, rem_pop_suffix, rem_omega_prefix) {
  parameters <- Parameters()
  
  # Retrieve THETAs
  thetas <- rxmod$theta %>%
    purrr::imap(~Theta(name=.y, value=.x))
  parameters <- parameters %>%
    add(thetas)
  
  # Remove 'pop' suffix from THETA names if asked
  # Note that this is not systematic (e.g. a, b (error-related parameters) do not have a pop suffix)
  # Because of that, we explicitly mention in the comment slot that the parameter is
  # a 'Population parameter' when pop is removed
  if (rem_pop_suffix) {
    parameters@list <- parameters@list %>%
      purrr::map(.f=function(.x) {
        popSuffixDetected <- grepl(pattern="_pop$", x=.x@name)
        if (popSuffixDetected) {
          .x@name <- gsub(pattern="_pop$", replacement="", x=.x@name)
          .x@comment <- "Population parameter"
        }
        return(.x)
      })
  }
  
  # Retrieve OMEGAs
  omegaMatrix <- rxmod$omega
  assertthat::assert_that(all(dimnames(omegaMatrix)[[1]]==dimnames(omegaMatrix)[[2]]))
  omegaNames <- dimnames(omegaMatrix)[[1]]
  
  # Remove 'omega_' prefix from OMEGA names if asked
  if (rem_omega_prefix) {
    omegaNames <- gsub(pattern="^omega_", replacement="", x=omegaNames)
  }

  for (index1 in seq_along(omegaNames)) {
    for (index2 in seq_along(omegaNames)) {
      if (index1 > index2) {
        next
      }
      omegaValue <- omegaMatrix[index1, index2]
      # Discard automatically zeroes on the off-diagonal elements
      if (omegaValue != 0 || index1==index2) {
        omegaName1 <- omegaNames[index1]
        omegaName2 <- omegaNames[index2]
        if (index1 == index2) {
          omegaName <- omegaName1
        } else {
          omegaName <- paste0(omegaName1, "_", omegaName2)
        }
        omega <- Omega(name=omegaName, value=omegaValue, index=index1, index2=index2, type=ifelse(index1==index2, "var", "covar"))
        parameters <- parameters %>%
          add(omega)
      }
    }
  }
  
  return(parameters)
}

#' Heuristic move from ODE to main.
#' 
#' @param model model with all statements in the ODE block
#' @return updated model with a MAIN block
#' 
heuristicMoveToMain <- function(model) {
  # Identify all continuous variables
  continuousVariables <- model@compartments@list %>%
    purrr::map_chr(.f=~paste0("A_", .x@name)) %>% # Compartment names
    append("t") # Simulation time
  
  oldOde <- model %>%
    find(OdeRecord())
  
  # Replace all continuous variables by empty string
  oldOdeTmp <- oldOde
  for (continuousVariable in continuousVariables) {
    oldOdeTmp <- oldOdeTmp %>%
      replaceAll(pattern=VariablePattern(continuousVariable), replacement="")
  }
  
  # Search for the first statement that has changed
  main <- MainRecord()
  updatedOde <- oldOde
  
  for (index in seq_along(oldOde@statements@list)) {
    statementA <- oldOde@statements@list[[index]]
    statementB <- oldOdeTmp@statements@list[[index]]
    if (isTRUE(all.equal(statementA, statementB))) {
      lastIndex <- index
      main@statements@list <- main@statements@list %>%
        append(statementA)
      updatedOde@statements@list <- updatedOde@statements@list[-1]
    } else {
      break
    }
  }
  
  # Replace records in original model
  model <- model %>%
    add(main) %>%
    replace(updatedOde)
    
  return(model)
}

convertRxodeErrorModel <- function(model) {
  ode <- model %>%
    find(OdeRecord())
  
  ode@statements@list <- ode@statements@list %>%
    purrr::discard(.p=function(statement) {
      if (is(statement, "unknown_statement") && isRxodeErrorEquation(statement@line)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
  
  # Update ODE record in model
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

isRxodeInitialConditionEquation <- function(x) {
  assertSingleCharacterString(x)
  parts <- strsplit(x, split="=")[[1]]
  if (length(parts) == 1) {
    return(FALSE)
  }
  return(grepl(pattern=sprintf("^%s\\(0\\)$", campsismod:::variablePatternStr()),
               x=parts[1] %>% trim()))
}

isRxodeErrorEquation <- function(x) {
  assertSingleCharacterString(x)
  parts <- strsplit(x, split="~")[[1]]
  if (length(parts) == 1) {
    return(FALSE)
  }
  return(TRUE)
}

getSubroutineModelForRxode2 <- function(advan, trans) {
  model <- campsismod::model_suite$nonmem[[paste0("advan", advan, "_trans", trans)]]
  
  if (advan==12 && trans==4) {
    return(NULL)
  }
  
  if (is.null(model)) {
    return(NULL)
  }

  return(model)
}

replaceLinCmt <- function(model, subroutineModel) {
  subroutineOde <- subroutineModel %>%
    find(OdeRecord()) %>%
    delete(Equation("F"))
  
  scale <- ""
  if (subroutineModel %>% campsismod::contains(Equation("S1"))) {
    scale <- "/scale1"
  } else if (subroutineModel %>% campsismod::contains(Equation("S2"))) {
    scale <- "/scale2"
  }
  
  # All parameters to uppercase (safe)
  linParameters <- subroutineModel@parameters@list %>%
    purrr::keep(.p=~is(.x, "theta")) %>%
    purrr::map_chr(.f=~.x@name)
  
  for (parameter in linParameters) {
    model <- model %>%
      replaceAll(tolower(parameter), parameter)
  }
  
  # Detect where to insert the ODEs
  ode <- model %>%
    find(OdeRecord()) # To replace by ODE record

  equationIndex <- ode@statements@list %>%
    purrr::detect_index(.f=detectFun <- function(statement) {
    if (is(statement, "equation")) {
      return(grepl(pattern="linCmt\\(\\)", x=statement@rhs))
    } else {
      return(FALSE)
    }
  })
  
  if (equationIndex==0) {
    return(model)
  }

  # Append ODE block
  equation <- ode@statements@list[[equationIndex]]
  
  ode@statements@list <- ode@statements@list %>%
    append(subroutineOde@statements@list, equationIndex - 1)
  
  # Replace linCmt equation and delete central
  equation@rhs <- sprintf("A_CENTRAL%s", scale)
  ode <- ode %>%
    replace(equation) %>%
    delete(Equation("central"))
  
  # Replace in original model
  model <- model %>%
    replace(ode)
  
  return(model)
}

