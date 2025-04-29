
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
#' @param path to mlxtran file
#' @param modelFile path to model file, optional
#' @param parametersFile path to estimated parameters file, optional
#' @return a functional Campsis model
#' @export
#' @importFrom digest sha1
#' @importFrom monolix2rx mlxtran monolix2rx
#' 
importMonolix <- function(mlxtranFile, modelFile=NULL, parametersFile=NULL) {
  # browser()
  
  # Create temporary directory
  tempDir <- tempdir()
  hash <- substr(digest::sha1(mlxtranFile), 1, 10)
  tempDir <- file.path(tempDir, paste0("monolix_", hash))
  if (!dir.exists(tempDir)) {
    dir.create(tempDir)
  }
  
  # Create export directory
  exportDir <- file.path(tempDir, "export")
  if (!dir.exists(exportDir)) {
    dir.create(exportDir)
  }
  print(gsub(pattern="\\\\", replacement="/", x=normalizePath(tempDir)))
  
  noExternalModel <- is.null(modelFile) || !file.exists(modelFile)
  
  # Copy mlxtran file to temporary directory
  mlxtran <- copyAndRename(file=mlxtranFile, tempDir=tempDir, newName="project.mlxtran")
  
  # Copy model file to temporary directory if provided
  if (!noExternalModel) {
    copyAndRename(file=modelFile, tempDir=tempDir, newName="model.txt")
  }
  # Copy parameters file to export directory if provided
  if (!is.null(parametersFile) && file.exists(parametersFile)) {
    copyAndRename(file=parametersFile, tempDir=exportDir, newName="populationParameters.txt")
  }
  
  # Read mlxtran file and adapt the link to the model file on the fly
  mlxtranStr <- readLines(mlxtran)
  
  longitudinalIndex <- which(grepl("\\s*\\[LONGITUDINAL\\]\\s*", mlxtranStr))
  filePathIndexes <- which(grepl("\\s*file\\s*=\\s*.*", mlxtranStr))

  if (length(longitudinalIndex) > 0) {
    if (noExternalModel) {
      modelBasename <- "project.mlxtran"
    } else {
      modelBasename <- "model.txt"
    }
    filePathIndexes <- filePathIndexes[filePathIndexes > longitudinalIndex]
    modelFilePathIndex <- filePathIndexes[1]
    mlxtranStr[modelFilePathIndex] <- sprintf("file = '%s'", modelBasename)
  }
  
  # Overwrite the mlxtran file with the modified content
  writeLines(mlxtranStr, mlxtran)

  # Import the mlxtran file
  mlxtranObj <- monolix2rx::mlxtran(file=mlxtran)
  
  # Delete dataset file path
  mlxtranObj$DATAFILE$FILEINFO$FILEINFO$file <- ""
  
  # Set the export path to the export directory
  mlxtranObj$MONOLIX$SETTINGS$GLOBAL$exportpath <- "export"
  
  # Error in if (!file.exists(.mlxtran$MODEL$LONGITUDINAL$LONGITUDINAL$file)) {
  # file is of length 1
  if (noExternalModel) {
    mlxtranObj$MODEL$LONGITUDINAL$LONGITUDINAL$file <- "project.mlxtran"
  } else {
    mlxtranObj$MODEL$LONGITUDINAL$LONGITUDINAL$file <- "model.txt"
  }

  # Convert the mlxtran object to rxode2 model
  rxmod <- monolix2rx(mlxtranObj)
  
  # Convert the rxode2 model to a functional Campsis model
  model <- importRxode2(rxmod, rem_pop_suffix=TRUE, rem_omega_prefix=TRUE)
  
  # Move pre-equations at right place (end of MAIN, instead of end of ODE)
  preEquations <- mlxtranObj$MODEL$LONGITUDINAL$PK$preEq
  
  if (length(preEquations) > 0) {
    for (preEquation in preEquations) {
      parts <- strsplit(preEquation, split="<-")[[1]]
      if (length(parts) > 1) {
        variable <- trimws(parts[1])
        if (model %>% contains(Equation(variable))) {
          model <- model %>%
            campsismod::move(x=Equation(variable), to=campsismod::Position(MainRecord()))
        }
      }
    }
  }
  
  
  return(model)
}