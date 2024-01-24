
#_______________________________________________________________________________
#----                           campsistrans class                          ----
#_______________________________________________________________________________

setClass(
  "campsistrans",
  representation(
    model = "list", # Workaround to store Pharmpy model
    estimate = "logical",
    mapping = "ANY",
    dirname = "character",
    campsis = "campsis_model"
  )
)

#_______________________________________________________________________________
#----                              import                                   ----
#_______________________________________________________________________________

#' Import NONMEM control stream and results using Pharmpy.
#' 
#' @param file path to NONMEM control stream, character value
#' @param mapping a possible PMX mapping object
#' @param estimate use estimated parameter values or initial ones, logical value, default is FALSE
#' @param uncertainty import the variance-covariance matrix (.cov file), logical value, default is FALSE
#' @param auto_install auto install pharmpy and dependencies if not installed yet
#' @param envname virtual python environment name, can be configured in config.yml
#' @param python path to python, can be configured in config.yml
#' @param copy_dir copy directory in which the control stream is
#' @param rem_rate remote RATE in control stream automatically to avoid issues with Pharmpy.
#'  Otherwise, it will look for the dataset and possibly adapt the ODE's to add the rates.
#' @return a campsistrans object
#' @importFrom reticulate import
#' @export
importNONMEM <- function(file, mapping=NULL, estimate=FALSE, uncertainty=FALSE,
                         auto_install=TRUE, envname=getPythonEnvName(), python=getPythonPath(), copy_dir=TRUE, rem_rate=TRUE) {
  pharmpy <- importPythonPackage("pharmpy")
  if (is.null(pharmpy)) {
    if (auto_install) {
      installPython(envname=envname, python=python)
    }
    pharmpy <- reticulate::import("pharmpy")
  }
  
  dirname <- dirname(file)
  ctlBasename <- basename(file)
  
  # Copy directory (default)
  if (copy_dir) {
    allFiles <- list.files(dirname)
    tmpDir <- tempdir()
    if (!dir.exists(tmpDir)) dir.create(tmpDir)
    specificDir <- file.path(tmpDir, paste0("dir_", sample(x=1e9, 1, replace=TRUE)))
    if (!dir.exists(specificDir)) dir.create(specificDir)
    
    for (myFile in allFiles) {
      originalFilePath <- file.path(dirname, myFile)
      file.copy(originalFilePath, file.path(specificDir, myFile))
    }
    ctlPath <- file.path(specificDir, ctlBasename)
    finalDir <- specificDir
  } else {
    ctlPath <- file
    finalDir <- dirname
  }
  
  # Remove RATE from control stream
  if (rem_rate) {
    removeRateFromCtl(ctlPath)
  }

  model <- pharmpy$Model$create_model(ctlPath)
  mapping <- if (is.null(mapping)) {mapping(NULL, NULL, NULL)} else {mapping}
  
  if (uncertainty) {
    fileNoExt <- sub(pattern = "(.*)\\..*$", replacement = "\\1", ctlBasename)
    covFile <- paste0(finalDir, "/", fileNoExt, ".cov")
    if (!file.exists(covFile)) {
      stop(paste0("File ", covFile, " could not be found"))
    }
    varcov <- read.nonmemcov(file=covFile)
  } else {
    varcov <- matrix(numeric(0), nrow=0, ncol=0)
  }
  
  # Convert parameters from NONMEM to Campsis
  parameters <- convertParameters(model, mapping=mapping, estimate=estimate)
  
  # Export CAMPSIS model
  campsis <-  exportCampsisModel(model, parameters, varcov, mapping)
  
  # Substitute duplicate equation names
  campsis <- campsis %>%
    substituteDuplicateEquationNames()
  
  # In case parameters are not valid (e.g. because of the SAME omega's)
  # Try to make it valid using auto-extraction
  if (mapping$auto && !isTRUE(validObject(campsis@parameters, test=TRUE, complete=TRUE))) {
      campsis <- tryCatch(
        expr=autoExtractParameters2(campsis),
        error = function(e) {
        return(campsis)
      })
  }
  
  # Create campsistrans object
  retValue <- new(
    "campsistrans",
    model = list(model),
    estimate = estimate,
    mapping = mapping,
    dirname = dirname,
    campsis = campsis
  )
  return(retValue)
}

#'
#' Remove RATE input from $INPUT field (string-based model). 
#' 
#' @param x string value (the whole control stream)
#' @return the same string value without RATE input
#' @export
#' 
removeRateFromString <- function(x) {
  retValue <- x
  
  # RATE followed by at least one space
  retValue <- gsub(pattern="^(.*)(\\$INPUT)([^\\$]*)([[:space:]]+RATE[ ]+)(.*)", replacement="\\1\\2\\3 \\5", x=retValue)
  
  # RATE followed by a combination of break line or space
  # In that case, break line is re-added
  retValue <- gsub(pattern="^(.*)(\\$INPUT)([^\\$]*)([[:space:]]+RATE[[:space:]]+)(.*)", replacement="\\1\\2\\3 \n\\5", x=retValue)
  
  return(retValue)
}

#'
#' Remove RATE input from $INPUT field in given control stream. 
#' 
#' @param file control stream file name
#' @return nothing
#' @export
#' 
removeRateFromCtl <- function(file) {
  fileConn = file(file)
  x <- paste0(readLines(con=fileConn), collapse="\n")
  x_ <- removeRateFromString(x)
  
  writeLines(text=x_, con=fileConn)
  close(fileConn)
}

#_______________________________________________________________________________
#----                                 export                                ----
#_______________________________________________________________________________


setMethod("export", signature = c("campsistrans", "character"), definition = function(object, dest, ...) {
  # pmxmod is accepted
  if (!(dest %in% c("campsis", "pmxmod"))) {
    stop("dest must be 'campsis'")
  }
  return(object@campsis)
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

setMethod("write", signature=c("campsistrans", "character"), definition=function(object, file, ...) {
  pharmpy <- reticulate::import("pharmpy")
  model <- object@model[[1]]
  pharmpy$modeling$write_model(model=model, path=file, force=TRUE)
})
