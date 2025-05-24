
#' Extract model code from an rxode2 function object.
#' 
#' @param rxmod the rxode2 function object
#' @param pop_parameter_regex regular expression to identify the population parameters
#' @param omega_parameter_regex regular expression to identify the OMEGA parameters
#' @param subroutine optional subroutine, integer vector of 2 values: ADVAN and TRANS.
#' @param cov import variance-covariance matrix if present, TRUE by default
#' @return a functional Campsis model
#' @export
importRxode2 <- function(rxmod, pop_parameter_regex=NULL, omega_parameter_regex=NULL, subroutine=NULL, cov=TRUE) {
  
  # Process variance-covariance matrix
  if (cov && !is.null(rxmod$thetaMat) && nrow(rxmod$thetaMat) > 0) {
    varcov <- rxmod$thetaMat
  } else {
    varcov <- NULL
  }
  
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
                                                 pop_parameter_regex=pop_parameter_regex,
                                                 omega_parameter_regex=omega_parameter_regex)

  # Rename THETAs in model code
  for (parameter in model@parameters@list %>% purrr::keep(~is(.x, "theta"))) {
    oldNameInCode <- parameter@name
    
    # Rebuild old name in code
    if (!is.na(parameter@comment) && parameter@comment=="Population parameter" && !is.null(pop_parameter_regex)) {
      oldNameInCode <- restorePrefixSuffix(x=oldNameInCode, regex=pop_parameter_regex)
    }
    # Replace in model code
    updatedThetaName <- sprintf("THETA_%s", replaceDotsInString(parameter@name))
    model <- model %>%
      replaceAll(VariablePattern(oldNameInCode), updatedThetaName)
    
    # Adapt varcov
    varcov <- adaptVarcovName(varcov=varcov, before=oldNameInCode, after=updatedThetaName)
  }
  
  # Rename ETAs in model code
  for (parameter in model@parameters@list %>% purrr::keep(~is(.x, "omega"))) {
    if (!parameter %>% campsismod::isDiag()) {
      # corr_ appears in variance-covariance after monolix import
      # to do later, see issue
      next
    }
    
    # Rebuild old name in code
    if (is.null(omega_parameter_regex)) {
      oldNameInCode <- parameter@name
    } else {
      oldNameInCode <- restorePrefixSuffix(x=parameter@name, regex=omega_parameter_regex)
    }
    
    # Replace in model code
    updatedEtaName <- sprintf("ETA_%s", replaceDotsInString(parameter@name))
    model <- model %>%
      replaceAll(VariablePattern(oldNameInCode), updatedEtaName)
    
    # Adapt varcov
    updatedOmegaName <- sprintf("OMEGA_%s", replaceDotsInString(parameter@name))
    varcov <- adaptVarcovName(varcov=varcov, before=oldNameInCode, after=updatedOmegaName)
  }
  
  # Replace dots in parameter names all at once
  model@parameters@list <- model@parameters@list %>%
    purrr::map(.f=~replaceDotsInParameterName(.x))
  if (!is.null(varcov)) {
    row.names(varcov) <- replaceDotsInString(row.names(varcov))
    colnames(varcov) <- replaceDotsInString(colnames(varcov))
  }
  
  # Heuristic move to MAIN block
  model <- heuristicMoveToMain(model)

  # Process error model
  model <- model %>%
    convertRxodeErrorModel(rxmod=rxmod)
  
  # Substitute duplicate equation names
  model <- substituteDuplicateEquationNames(model)

  # Replace the caret operator by pow
  model <- caretToPow(model)
  
  # Add variance-covariance matrix
  if (!is.null(varcov)) {
    model@parameters@varcov <- varcov
  }
  
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
  
  # Remove all 'rxm.' occurrences when subroutine is used
  # Matt adds this prefix when subroutines are used, I don't know the exact reason
  if (!is.null(subroutine)) {
    code <- gsub(pattern="\\brxm\\.", replacement="", x=code)
  }
  
  # Remove compartment declarations if any
  # Not sure at this stage if this is needed
  code <- gsub(pattern="^cmt\\s*\\(.*\\)\\s*$", replacement="", x=code)
  code <- code[code!=""]
  
  # Prepare subroutine model if any
  if (!is.null(subroutine)) {
    assertthat::assert_that(length(subroutine)==2L, msg="Subroutine should be a vector of 2 integers")
    subroutineModel <- getSubroutineModelForRxode2(advan=subroutine[1], trans=subroutine[2])
    if (is.null(subroutineModel)) {
      warning("ODEs are not available for the given subroutine")
    }
  } else {
    subroutineModel <- NULL
  }
  
  # Replace R assignments by equals
  code <- gsub(pattern="<-", replacement="=", x=code)
  
  # Retrieve compartments
  cmtNames <- rxmod$stateDf[, "Compartment Name"]

  # Compartment indexes
  cmtIndexes <- seq_along(cmtNames)

  # Possibly replace strange coding of compartment properties (especially with the dot)
  for (cmtIndex in cmtIndexes) {
    code <- renameCompartmentProperties(code=code, occurrence=sprintf("rxddta%s", cmtIndex),
                                        replacement=as.character(cmtIndex), prefix="A")
  }
  
  # In case a subroutine is used, cmtNames is empty
  # depot, central names are used instead
  if (!is.null(subroutineModel)) {
    for (cmtName in c("depot", "central")) {
      code <- renameCompartmentProperties(code=code, occurrence=cmtName,
                                          replacement=toupper(cmtName), prefix="")
    }
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
  
  # Replace all equations with dots in the name
  ode <- replaceDotsInVariableNames(ode)
  
  # Add ODE record to model
  model <- model %>%
    add(ode)
  
  # Add subroutine if any
  if (!is.null(subroutineModel)) {
    model <- replaceLinCmt(model=model, subroutineModel=subroutineModel)
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
  ode <- model %>% campsismod::find(OdeRecord())
  
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
    campsismod::find(OdeRecord())
  
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
    campsismod::find(OdeRecord())
  
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

detectPrefixSuffix <- function(x, regex) {
  if (is.null(regex)) {
    return(rep(FALSE, length(x)))
  }
  x_ <- gsub(pattern=regex, replacement="", x=x)
  return(x_ != x)
}

removePrefixSuffix <- function(x, regex) {
  if (is.null(regex)) {
    return(x)
  }
  x_ <- gsub(pattern=regex, replacement="", x=x)
  return(x_)
}

restorePrefixSuffix <- function(x, regex) {
  if (is.null(regex)) {
    return(x)
  }
  if (substring(regex, 1, 1)=="^") {
    return(paste0(substring(regex, 2, nchar(regex)), x))
  } else if (substring(regex, nchar(regex), nchar(regex))=="$") {
    return(paste0(x, substring(regex, 1, nchar(regex)-1)))
  } else {
    stop("regex must start with ^ or end by $")
  }
}

#' Extract all parameters from an rxode2 function object.
#' 
#' @param rxmod the rxode2 function object
#' @param pop_parameter_regex regular expression to identify the population parameters
#' @param omega_parameter_regex regular expression to identify the OMEGA parameters
#' @return a functional Campsis model
#' @export
extractParametersFromRxode <- function(rxmod, pop_parameter_regex, omega_parameter_regex) {
  parameters <- Parameters()
  
  # Retrieve THETAs
  thetas <- rxmod$theta %>%
    purrr::imap(~Theta(name=.y, value=.x))
  parameters <- parameters %>%
    add(thetas)
  
  # Take into account population parameter regex
  parameters@list <- parameters@list %>%
    purrr::map(.f=function(.x) {
      popSuffixDetected <- detectPrefixSuffix(x=.x@name, regex=pop_parameter_regex)
      if (popSuffixDetected) {
        .x@name <- removePrefixSuffix(x=.x@name, regex=pop_parameter_regex)
        .x@comment <- "Population parameter"
      }
      return(.x)
    })
  
  # Retrieve OMEGAs
  omegaMatrix <- rxmod$omega
  assertthat::assert_that(all(dimnames(omegaMatrix)[[1]]==dimnames(omegaMatrix)[[2]]))
  omegaNames <- dimnames(omegaMatrix)[[1]]
  
  # Take into account omega parameter regex
  omegaNames <- removePrefixSuffix(x=omegaNames, regex=omega_parameter_regex)

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
    campsismod::find(OdeRecord())
  
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

convertRxodeErrorModel <- function(model, rxmod) {
  ode <- model %>%
    campsismod::find(OdeRecord())
  
  errorEquationLgl <- ode@statements@list %>%
    purrr::map_lgl(.f=function(statement) {
      if (is(statement, "unknown_statement") && isRxodeErrorEquation(statement@line)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
  
  # Collect error equations
  errorEquations <- ode@statements@list[errorEquationLgl]
  
  # Remove them from the ODE record
  ode@statements@list <- ode@statements@list[!errorEquationLgl]
  
  # Update ODE record in model
  model <- model %>%
    replace(ode)
  
  # Prepare error record
  errorRecord <- ErrorRecord()
  lexer  <- rly::lex(Rxode2ErrorModelLexer)
  parser <- rly::yacc(Rxode2ErrorModelParser)

  shift <- 0L
  for (errorEquation in errorEquations) {
    errorModel <- tryCatch({
      parser$parse(errorEquation@line, lexer)
    }, error = function(e) {
      warning(e$message)
      return(NULL)
    })
    if (is.null(errorModel)) {
      next
    }
    tmp <- errorModelToCampsis(errorModel, shift=shift)
    equation <- tmp$equation
    epsilons <- tmp$eps
    errorRecord <- errorRecord %>%
      add(equation)
    shift <- shift + length(epsilons)
    for (epsilon in epsilons) {
      model <- model %>%
        add(Sigma(name=sprintf("FIX%i", epsilon), value=1, type="var", fix=TRUE))
    }
  }
  
  # Add error record to model
  model <- model %>%
    add(errorRecord)
  
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
    campsismod::find(OdeRecord()) %>%
    delete(Equation("F"))
  
  scale <- ""
  if (model %>% campsismod::contains(Equation("scale1"))) {
    scale <- "/scale1"
  } else if (model %>% campsismod::contains(Equation("scale2"))) {
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
    campsismod::find(OdeRecord()) # To replace by ODE record

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

replaceDotsInVariableNames <- function(ode) {
  variablesWithDots <- ode@statements@list %>%
    purrr::map_chr(.f=function(statement) {
      if (is(statement, "equation")) {
        lhs <- statement@lhs
        lhsSub <- replaceDotsInString(lhs)
        return(ifelse(lhs==lhsSub, "", lhs))
      } else if (is(statement, "if_statement")) {
        lhs <- statement@equation@lhs
        lhsSub <- replaceDotsInString(lhs)
        return(ifelse(lhs==lhsSub, "", lhs))
      }
      return("")
    })
  
  variablesWithDots <- variablesWithDots[variablesWithDots!=""]
  
  for (variable in variablesWithDots) {
    ode <- ode %>%
      replaceAll(VariablePattern(variable), replaceDotsInString(variable))
  }
  
  return(ode)
}

replaceDotsInParameterName <- function(parameter) {
  if (is.na(parameter@name)) {
    return(parameter)
  } else {
    parameter@name <- replaceDotsInString(parameter@name)
    return(parameter)
  }
}

replaceDotsInString <- function(x) {
  return(gsub(pattern="\\.", replacement="_", x=x))
}

renameCompartmentProperties <- function(code, occurrence, replacement, prefix) {
  # Fractions
  code <- replaceAll(object=code, pattern=VariablePattern(sprintf("rxf\\.%s\\.", occurrence)),
                     replacement=sprintf("F_%s%s", prefix, replacement))
  # Initial conditions
  code <- replaceAll(object=code, pattern=VariablePattern(sprintf("rxini\\.%s\\.", occurrence)),
                     replacement=sprintf("INIT_%s%s", prefix, replacement))
  # Infusion durations
  code <- replaceAll(object=code, pattern=VariablePattern(sprintf("rxdur\\.%s\\.", occurrence)),
                     replacement=sprintf("DUR_%s%s", prefix, replacement))
  # Infusion rates
  code <- replaceAll(object=code, pattern=VariablePattern(sprintf("rxrate\\.%s\\.", occurrence)),
                     replacement=sprintf("RATE_%s%s", prefix, replacement))
  # Lag times
  code <- replaceAll(object=code, pattern=VariablePattern(sprintf("rxalag\\.%s\\.", occurrence)),
                     replacement=sprintf("LAG_%s%s", prefix, replacement))
  
  return(code)
}

adaptVarcovName <- function(varcov, before, after) {
  if (is.null(varcov)) {
    return(varcov)
  }
  regex <- sprintf("^%s$", gsub(pattern="\\.", replacement="\\\\.", x=before))
  row.names(varcov) <- gsub(pattern=regex, replacement=after, x=row.names(varcov))
  colnames(varcov) <- gsub(pattern=regex, replacement=after, x=colnames(varcov))
  return(varcov)
}


