
#_______________________________________________________________________________
#----                           campsistrans class                          ----
#_______________________________________________________________________________

setClass(
  "campsistrans",
  representation(
    model = "ANY",
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
#' @param covar_name give a name to each covariance value
#' @param covar_as_cor transform covariance values to correlation values
#' @param auto_install auto install pharmpy and dependencies if not installed yet
#' @param pharmpy_config a Pharmpy configuration object, see \link{OldPharmpyConfig} for details
#' @param copy_dir copy directory in which the control stream is
#' @param rem_rate remove RATE in control stream automatically to avoid issues with Pharmpy.
#'  Otherwise, it will look for the dataset and possibly adapt the ODE's to add the rates, default is FALSE
#' @param rem_abbr_replace remove section ABBREVIATED REPLACE, causing issue in import, default is TRUE
#' @return a campsistrans object
#' @importFrom reticulate import
#' @export
importNONMEM <- function(file, mapping=NULL, estimate=FALSE, uncertainty=FALSE,
                         covar_name=FALSE, covar_as_cor=FALSE,
                         auto_install=TRUE, pharmpy_config=UpdatedPharmpyConfig(),
                         copy_dir=FALSE, rem_rate=FALSE, rem_abbr_replace=TRUE) {
  
  pharmpy <- importPharmpyPackage(pharmpy_config)
  if (is.null(pharmpy)) {
    if (auto_install) {
      installPharmpy(pharmpy_config)
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
  
  # Adapt source control stream file based on arguments
  adaptNONMEMControlStream(file=ctlPath, rem_rate=rem_rate, rem_abbr_replace=rem_abbr_replace)

  # Create model with Pharmpy
  model <- loadCtl(path=ctlPath, estimate=estimate)
  
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
  
  # Always provide OMEGA mapping if unset
  mapping <- if (is.null(mapping)) {mapping(NULL, NULL, NULL)} else {mapping}
  if (estimate && (mapping$params %>% campsismod::select("omega") %>% length() == 0)) {
    omegaIndexes <- seq_len(getNumberOfEtas(model))
    mapping$params <- addMapping(omegaIndexes, parameters=mapping$params, type="omega") %>%
      campsismod::sort()
  }
  
  # Convert parameters from NONMEM to Campsis
  parameters <- convertParameters(model, mapping=mapping)
  
  # Export CAMPSIS model
  campsis <-  exportCampsisModel(model, parameters, varcov, mapping)
  
  # Substitute duplicate equation names
  campsis <- campsis %>%
    substituteDuplicateEquationNames()
  
  # Newind() to NEWIND
  campsis <- campsis %>%
    replaceAll(pattern=Pattern("newind\\(\\)"), replacement="NEWIND")
  
  # In case parameters are not valid (e.g. because of the SAME omega's)
  # Try to make it valid using auto-extraction
  if (mapping$auto && !isTRUE(validObject(campsis@parameters, test=TRUE, complete=TRUE))) {
      campsis <- tryCatch(
        expr=autoExtractParameters2(campsis),
        error = function(e) {
        return(campsis)
      })
  }
  
  # Give a name to each correlation
  if (covar_name) {
    campsis <- nameCovariance(model=campsis)
  }
  
  # Convert to correlation values
  if (covar_as_cor) {
    campsis <- covarToCor(model=campsis)
  }
  
  # Create campsistrans object
  retValue <- new(
    "campsistrans",
    model = model,
    estimate = estimate,
    mapping = mapping,
    dirname = dirname,
    campsis = campsis
  )
  return(retValue)
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
  model <- object@model
  pharmpy$modeling$write_model(model=model, path=file, force=TRUE)
})
