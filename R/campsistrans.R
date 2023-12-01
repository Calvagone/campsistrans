
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
#' @return a campsistrans object
#' @importFrom reticulate import
#' @export
importNONMEM <- function(file, mapping=NULL, estimate=FALSE, uncertainty=FALSE,
                         auto_install=TRUE, envname=getPythonEnvName(), python=getPythonPath()) {
  pharmpy <- importPythonPackage("pharmpy")
  if (is.null(pharmpy)) {
    if (auto_install) {
      installPython(envname=envname, python=python)
    }
    pharmpy <- reticulate::import("pharmpy")
  }

  model <- pharmpy$Model$create_model(file)
  mapping <- if (is.null(mapping)) {mapping(NULL, NULL, NULL)} else {mapping}
  dirname <- dirname(file)
  
  if (uncertainty) {
    fileNoExt <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
    covFile <- paste0(dirname, "/", fileNoExt, ".cov")
    if (!file.exists(covFile)) {
      stop(paste0("File ", covFile, " could not be found"))
    }
    varcov <- read.nonmemcov(file=covFile)
  } else {
    varcov <- matrix(numeric(0), nrow=0, ncol=0)
  }
  
  # Convert parameters from NONMEM to pmxmod
  parameters <- convertParameters(model, mapping=mapping, estimate=estimate)
  
  # Export CAMPSIS model
  campsis <-  exportCampsisModel(model, parameters, varcov, mapping)
  
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
