
#_______________________________________________________________________________
#----                        rxode2_error_model class                       ----
#_______________________________________________________________________________

#' Rxode2 error model class.
#' 
#' @slot add additive error related variable
#' @slot prop proportional error related variable
#' @slot combined1 combined1 enabled
#' @slot combined2 combined2 enabled
#' @slot endpoint related endpoint
#' @export
setClass(
  "rxode2_error_model",
  representation(
    add="character",
    prop="character",
    combined1="logical",
    combined2="logical",
    endpoint="character"
  ), prototype=list(add=character(0), prop=character(0), combined1=FALSE, combined2=FALSE, endpoint=character(0))
)

#'
#' Create a new Rxode2 error model.
#' 
#' @param add additive error related variable
#' @param prop proportional error related variable
#' @param combined1 combined1 enabled
#' @param combined2 combined2 enabled
#' @param endpoint related endpoint
#' @return an Rxode2 error model
#' @export
Rxode2ErrorModel <- function(add=NULL, prop=NULL, combined1=FALSE, combined2=FALSE, endpoint=NULL) {
  if (is.null(add)) {
    add <- character(0)
  }
  if (is.null(prop)) {
    prop <- character(0)
  }
  if (is.null(endpoint)) {
    endpoint <- character(0)
  }
  return(new("rxode2_error_model", add=add, prop=prop, combined1=combined1, combined2=combined2, endpoint=endpoint))
}
