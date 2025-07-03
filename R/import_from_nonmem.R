
#' Extract the model from NONMEM via nonmem2rx.
#' 
#' @param ctlFile path to control stream file
#' @param extFile path to parameter estimates file, optional
#' @param covFile path to variance-covariance matrix file, optional
#' @return a functional Campsis model
#' @export
#' @importFrom digest sha1
#' @importFrom nonmem2rx nonmem2rx
#' 
importNONMEM2 <- function(ctlFile, extFile=NULL, covFile=NULL) {
 
  # Create temporary directory
  tempDir <- tempdir()
  hash <- substr(digest::sha1(paste0(Sys.time(), ctlFile)), 1, 10)
  tempDir <- file.path(tempDir, paste0("nonmem2_", hash))
  if (!dir.exists(tempDir)) {
    dir.create(tempDir)
  }
  
  # Copy control stream file to temporary directory
  ctl <- copyAndRename(file=ctlFile, tempDir=tempDir, newName="model.mod") # mod default in nonmem2rx
  
  # Replace TIME by DATASET_TIME in the control stream
  replaceDatasetTime(file=ctl)
  
  # Copy ext file to temporary directory if provided
  estimate <- !is.null(extFile) && file.exists(extFile)
  if (estimate) {
    copyAndRename(file=extFile, tempDir=tempDir, newName="model.ext") # ext default in nonmem2rx
  }
  
  # Copy ext file to temporary directory if provided
  if (!is.null(covFile) && file.exists(covFile)) {
    copyAndRename(file=covFile, tempDir=tempDir, newName="model.cov") # cov default in nonmem2rx
  }

  rxmod <- nonmem2rx::nonmem2rx(file=ctl, tolowerLhs=FALSE, thetaNames=FALSE, etaNames=FALSE,
                                cmtNames=TRUE, validate=FALSE)
  
  subroutine <- detectSubroutine(suppressWarnings(readLines(ctl)))
  # print(subroutine)
    
  # Conversion to Campsis
  model <- importRxode2(rxmod=rxmod, subroutine=subroutine, cov=FALSE)
  
  # DATASET_TIME back to TIME
  model <- model %>%
    replaceAll("DATASET_TIME", "TIME")
  
  # Post-process scale factors
  model <- postProcessScaleFactors(model)
  
  # Remove default names given by nonmem2rx importer before auto renaming
  updatedParameters <- Parameters()
  for (x in model@parameters@list) {
    if (is(x, "theta")) {
      model <- model %>%
        replaceAll(sprintf("THETA_%s", x@name), sprintf("THETA_%s", x@index))
      x@name <- as.character(NA)
    } else if (is(x, "omega") && isDiag(x)) {
      model <- model %>%
        replaceAll(sprintf("ETA_%s", x@name), sprintf("ETA_%s", x@index))
      x@name <- as.character(NA)
    }
    updatedParameters <- updatedParameters %>%
      add(x)
  }
  model@parameters <- updatedParameters
  
  # Auto mapping based on the equation names
  model <- autoRenameParameters(model)
  
  # Remove useless equations like ETA_CL=ETA_CL (in NONMEM ETA_CL=ETA(1))
  model <- removeUselessEquations(model)
  
  # Include SIGMAs (only diagonal is supported)
  model <- includeSigmas(rxmod=rxmod, model=model)
  
  # Heuristic move to error
  model <- heuristicMoveToError(model)
  
  # Add variance-covariance matrix
  if (!is.null(rxmod$thetaMat) && nrow(rxmod$thetaMat) > 0) {
    model <- processRxode2Varcov(model=model, varcov=rxmod$thetaMat)
  }
  
  return(new("campsistrans", model=list(rxmod), campsis=model, estimate=estimate))
}

#' Heuristic move from the ODE record to the Error record.
#' 
#' @param model model with all statements in the ODE block
#' @return updated model with an error block similar to NONMEM
#' 
heuristicMoveToError <- function(model) {
  ode <- model %>%
    campsismod::find(OdeRecord())

  dvIndex <- ode@statements@list %>%
    purrr::detect_index(.f=function(x) {
        if (is(x, "equation")) {
          return(x@lhs=="F")
        }
        return(FALSE)
      })
  
  odeLength <- length(ode@statements@list)
  if (dvIndex==0 || dvIndex==odeLength) {
    return(model)
  }
  
  error <- model %>% 
    campsismod::find(ErrorRecord())
  
  if (is.null(error)) {
    error <- ErrorRecord()
    model <- model %>%
      add(error)
  }
  
  # Move statements to the error record
  statementsToMove <- ode@statements@list[seq(dvIndex + 1, odeLength)]
  ode@statements@list <- ode@statements@list[seq_len(dvIndex)]
  error@statements@list <- c(statementsToMove, error@statements@list)
  
  # Update the model
  model <- model %>%
    replace(ode) %>%
    replace(error)
  
  return(model)
}

includeSigmas <- function(rxmod, model) {
  # In some cases, monolix2rx understand the NONMEM error model code
  # And it provides the error model equation in rxode2 (using the ~ operator) and adds SIGMAs
  # In that case, we do not do anything
  sigmas <- model@parameters %>%
    campsismod::select("sigma")
  if (length(sigmas@list) > 0) {
    return(model)
  }
  
  sigmaMatrix <- rxmod$sigma
  diag <- diag(sigmaMatrix)
  
  for (index1 in seq_along(diag)) {
    for (index2 in seq_along(diag)) {
      if (index2 > index1) {
        next
      }
      onDiag <- index1==index2
      omegaValue <- sigmaMatrix[index1, index2]

      if (onDiag) {
        epsName <- names(diag)[index1]
        sigma <- Sigma(name=replaceEpsInSigma(epsName), index=index1, index2=index2, value=omegaValue, fix=omegaValue==1, type="var")
      } else {
        epsName <- sprintf("eps_%i_%i", index1, index2)
        sigma <- Sigma(name=replaceEpsInSigma(epsName), index=index1, index2=index2, value=omegaValue, fix=FALSE, type="covar")
      }
      # Do not add SIGMA if off-diagonal and value is 0
      if (!onDiag && omegaValue==0) {
        next
      }
      # Add sigma
      model <- model %>%
        add(sigma)
      
      # Replace occurrences in model
      if (onDiag) {
        model <- model %>%
          replaceAll(epsName, sprintf("EPS_%s", replaceEpsInSigma(epsName)))
      }
    }
  }
  return(model)
}

replaceEpsInSigma <- function(x) {
  return(gsub(pattern="eps", replacement="RUV", x=x))
}

detectSubroutine <- function(x) {
  indexes <- grepl(pattern="^\\$(SUB|SUBR|SUBROUTINE|SUBROUTINES)\\s+.*", x=trimws(x))
  subroutines <- x[indexes]
  if (length(subroutines)==0) {
    return(NULL)
  }
  subroutine <- subroutines[1]
  advan <- suppressWarnings(as.numeric(gsub(pattern=".*ADVAN([0-9]+).*", "\\1", subroutine)))
  trans <- suppressWarnings(as.numeric(gsub(pattern=".*TRANS([0-9]+).*", "\\1", subroutine)))
  if (is.na(advan)) {
    return(NULL)
  }
  if (!(advan %in% c(1, 2, 3, 4, 11, 12))) {
    return(NULL)
  }
  if (is.na(trans)) {
    trans <- 1
  }
  return(c(advan, trans))
}

postProcessScaleFactors <- function(model) {
  main <- model %>%
    campsismod::find(MainRecord())
  if (is.null(main)) {
    return(model)
  }
  scaleEqs <- main@statements@list %>%
    purrr::keep(.p=function(x) {
      if (is(x, "equation")) {
        return(grepl(pattern="^scale[0-9]+$", x@lhs))
      }
      return(FALSE)
    })
  for (eq in scaleEqs) {
    original <- eq@lhs
    replacement <- toupper(original)
    model <- model %>%
      campsismod::replaceAll(original, replacement)
    
    # Delete scaleX_ if any
    model <- model %>%
      campsismod::delete(Equation(paste0(original, "_")))
  }
  
  # If the model contains rxLinCmt1, replace it with F
  if (model %>% campsismod::contains(Equation("rxLinCmt1"))) {
    model <- model %>%
      campsismod::delete(Equation("F")) %>%
      replaceAll("rxLinCmt1", "F")
  }
  
  return(model)
}

replaceDatasetTime <- function(file) {
  fileConn = file(file)
  lines <- suppressWarnings(readLines(con=fileConn))
  lines <- replaceAll(object=lines, pattern=VariablePattern("TIME"), replacement="DATASET_TIME")
  writeLines(text=lines, con=fileConn)
  close(fileConn)
}
