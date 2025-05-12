
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
  
  # Copy ext file to temporary directory if provided
  if (!is.null(extFile) && file.exists(extFile)) {
    copyAndRename(file=extFile, tempDir=tempDir, newName="model.ext") # ext default in nonmem2rx
  }
  # Copy parameters file to export directory if provided
  if (!is.null(covFile) && file.exists(covFile)) {
    copyAndRename(file=covFile, tempDir=tempDir, newName="model.cov") # cov default in nonmem2rx
  }
  
  rxmod <- nonmem2rx::nonmem2rx(file=ctl, tolowerLhs=FALSE, thetaNames=FALSE, etaNames=FALSE,
                                cmtNames=TRUE, validate=FALSE)
  
  # Conversion to Campsis
  model <- importRxode2(rxmod)
  
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
  model <- model %>% 
    autoRenameParameters()
   
  return(model)
}
