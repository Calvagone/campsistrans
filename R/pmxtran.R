
#_______________________________________________________________________________
#----                           pmxtran class                               ----
#_______________________________________________________________________________

setClass(
  "pmxtran",
  representation(
    model = "list", # Workaround to store Pharmpy model
    params = "parameters",
    estimate = "logical",
    varcov = "matrix"
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
#' @return a PMXtran object
#' @importFrom reticulate import
#' @export
importNONMEM <- function(file, mapping=NULL, estimate=FALSE, uncertainty=FALSE) {
  pharmpy <- reticulate::import("pharmpy")
  model <- pharmpy$Model(file)
  mapping <- if (is.null(mapping)) {mapping(NULL, NULL, NULL)} else {mapping}
  if (uncertainty) {
    fileNoExt <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
    dirname <- dirname(file)
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
  
  # Create pmxtran object
  pmxtran <- new(
    "pmxtran",
    model = list(model),
    params = parameters,
    estimate = estimate,
    varcov = varcov
  )
  return(pmxtran)
}

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

setMethod("write", signature=c("pmxtran", "character"), definition=function(object, file, ...) {
  # USE source.write to avoid call to update_source
  #x$model$source$write(file, force=TRUE)
  ctl <- as.character(object$model[[1]])
  fileConn <- file(file)
  writeLines(text=ctl, fileConn)
  close(fileConn)
})
