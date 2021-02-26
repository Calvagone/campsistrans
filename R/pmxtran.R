
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
    params=extractParameters(model, mapping=mapping, estimate=estimate),
    estimate=estimate
  ), class="pmxtran")
  
  return(retValue)
}

#' Write PMXtran object to NONMEM control stream file.
#' 
#' @param x PMXtran object
#' @param file path to exported NONMEM control stream
#' @param ... ignored
#' @export
write.pmxtran <- function(x, file, ...) {
  x$model$write(file, force=TRUE)
}
