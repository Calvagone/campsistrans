
#' Import NONMEM control stream and results using Pharmpy.
#' 
#' @param x path to NONMEM control stream
#' @param mapping a possible PMX mapping object
#' @param estimate if TRUE, estimated values are imported, if FALSE, initial values are used
#' @return a PMXtran object
#' @importFrom reticulate import
#' @export
importNONMEM <- function(x, mapping=NULL, estimate=FALSE) {
  pharmpy <- reticulate::import("pharmpy")
  model <- pharmpy$Model(x)
  mapping <- if (is.null(mapping)) {mapping(NULL, NULL, NULL)} else {mapping}
  retValue <- structure(list(
    model=model,
    params=extractParameters(model, mapping=mapping, estimate=estimate)
  ), class="pmx_tran")
  
  return(retValue)
}

#' Write PMXtran object to NONMEM control stream file.
#' 
#' @param pmxtran PMXtran object
#' @param path path to desired control stream file
#' @export
modelToNONMEM <- function(pmxtran, path) {
  assertthat::assert_that(inherits(pmxtran, "pmx_tran"),
                          msg="not a pmxtran object")
  if (file.exists(path)) {
    file.remove(path)
  }
  pmxtran$model$write(path)
}
