
#_______________________________________________________________________________
#----                           campsistrans class                          ----
#_______________________________________________________________________________

setClass(
  "campsistrans",
  representation(
    model = "list", # Workaround to store Pharmpy model
    params = "parameters",
    estimate = "logical",
    varcov = "matrix",
    mapping = "ANY",
    dirname = "character"
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
#' @return a campsistrans object
#' @importFrom reticulate import
#' @export
importNONMEM <- function(file, mapping=NULL, estimate=FALSE, uncertainty=FALSE) {
  pharmpy <- reticulate::import("pharmpy")
  #model <- pharmpy$Model(file)
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
  
  # Create campsistrans object
  retValue <- new(
    "campsistrans",
    model = list(model),
    params = parameters,
    estimate = estimate,
    varcov = varcov,
    mapping = mapping,
    dirname = dirname
  )
  return(retValue)
}

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

setMethod("write", signature=c("campsistrans", "character"), definition=function(object, file, ...) {
  # pharmpy <- reticulate::import("pharmpy")
  # pharmpy$
  pharmpyModel <- object@model[[1]]
  # pharmpyModel <- campsistrans@model[[1]]
  pharmpyModel$write(path=file, force=TRUE)
})
