
copyAndRename <- function(file, tempDir, newName) {
  if (!file.exists(file)) {
    stop(paste0("File ", file, " could not be found"))
  }
  # Copy file to temporary directory
  file.copy(from=file, to=tempDir)
  
  # Rename file in temporary directory
  file.rename(from=file.path(tempDir, basename(file)), to=file.path(tempDir, newName))
  
  return(file.path(tempDir, newName))
}

#' Extract the model from Monolix.
#' 
#' @param mlxtranFile to mlxtran file
#' @param modelFile path to model file, optional
#' @param parametersFile path to estimated parameters file, optional
#' @param covFile path to covariance estimates file, optional
#' @param digits number of significant digits, integer or NULL to disable rounding
#' @return a functional Campsis model
#' @export
#' @importFrom digest sha1
#' @importFrom monolix2rx mlxtran monolix2rx
#' 
importMonolix <- function(mlxtranFile, modelFile=NULL, parametersFile=NULL, covFile=NULL, digits=NULL) {
  # Create temporary directory
  tempDir <- tempdir()
  hash <- substr(digest::sha1(paste0(Sys.time(), mlxtranFile)), 1, 10)
  tempDir <- file.path(tempDir, paste0("monolix_", hash))
  if (!dir.exists(tempDir)) {
    dir.create(tempDir)
  }
  
  print(gsub(pattern="\\\\", replacement="/", x=normalizePath(tempDir)))
  
  # Is there a external model?
  noExternalModel <- is.null(modelFile) || !file.exists(modelFile)
  
  # Copy mlxtran file to temporary directory
  mlxtran <- copyAndRename(file=mlxtranFile, tempDir=tempDir, newName="project.mlxtran")
  
  # Read mlxtran file and adapt the link to the model file on the fly
  mlxtranStr <- readLines(mlxtran)
  
  longitudinalIndex <- which(grepl("\\s*\\[LONGITUDINAL\\]\\s*", mlxtranStr))
  filePathIndexes <- which(grepl("\\s*file\\s*=\\s*.*", mlxtranStr))

  if (length(longitudinalIndex) > 0) {
    modelBasename <- "model.txt"
    filePathIndexes <- filePathIndexes[filePathIndexes > longitudinalIndex]
    if (length(filePathIndexes) >0 ) {
      modelFilePathIndex <- filePathIndexes[1]
      mlxtranStr[modelFilePathIndex] <- sprintf("file = '%s'", modelBasename)
    } else {
      # Bug in monolix2rx: file is needed
      mlxtranStr <- mlxtranStr %>% append(sprintf("file = '%s'", modelBasename), after=longitudinalIndex)
    }
  }
  
  # Overwrite the mlxtran file with the modified content
  writeLines(mlxtranStr, mlxtran)
  
  # Import the mlxtran file a first time to retrieve the export path
  mlxtranObj <- monolix2rx::mlxtran(file=mlxtran)
  exportDirName <- mlxtranObj$MONOLIX$SETTINGS$GLOBAL$exportpath

  # Copy model file to temporary directory if provided
  if (noExternalModel) {
    tmpFile <- tempfile("model", fileext=".txt") # Dummy model file
    writeLines("", tmpFile)
    copyAndRename(file=tmpFile, tempDir=tempDir, newName="model.txt")
  } else {
    copyAndRename(file=modelFile, tempDir=tempDir, newName="model.txt")
  }
  
  # Create export directory
  exportDir <- file.path(tempDir, exportDirName)
  if (!dir.exists(exportDir)) {
    dir.create(exportDir)
  }
  
  # Copy parameters file to export directory if provided
  if (!is.null(parametersFile) && file.exists(parametersFile)) {
    copyAndRename(file=parametersFile, tempDir=exportDir, newName="populationParameters.txt")
  }
  
  # Copy parameters file to export directory if provided
  if (!is.null(covFile) && file.exists(covFile)) {
    # Create fisher information directory
    fisherInformationDir <- file.path(exportDir, "FisherInformation")
    if (!dir.exists(fisherInformationDir)) {
      dir.create(fisherInformationDir)
    }
    copyAndRename(file=covFile, tempDir=fisherInformationDir, newName="covarianceEstimatesSA.txt")
  }

  # Convert the mlxtran object to rxode2 model
  rxmod <- monolix2rx::monolix2rx(mlxtran, envir=new.env(), thetaMatType="sa")

  # Convert the rxode2 model to a functional Campsis model
  model <- importRxode2(rxmod, rem_pop_suffix=TRUE, rem_omega_prefix=TRUE)
  
  # Move pre-equations at right place (end of MAIN, instead of end of ODE)
  preEquations <- mlxtranObj$MODEL$LONGITUDINAL$PK$preEq
  
  if (length(preEquations) > 0) {
    for (preEquation in preEquations) {
      parts <- strsplit(preEquation, split="<-")[[1]]
      if (length(parts) > 1) {
        variable <- trimws(parts[1])
        if (model %>% campsismod::contains(Equation(variable))) {
          model <- model %>%
            campsismod::move(x=Equation(variable), to=campsismod::Position(MainRecord()))
        }
      }
    }
  }
  
  # Round digits
  if (!is.null(digits)) {
    model@parameters@list <- model@parameters@list %>%
      purrr::map(.f=function(x) {
        x@value <- signif(x@value, digits=digits)
        return(x)
      })
  }
  
  # Add variance-covariance matrix
  if (!is.null(rxmod$thetaMat) && nrow(rxmod$thetaMat) > 0) {
    model@parameters@varcov <- rxmod$thetaMat
  }

  return(model)
}