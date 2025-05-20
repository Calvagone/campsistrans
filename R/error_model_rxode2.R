
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

errorModelToCampsis <- function(x, shift) {
  assertthat::assert_that(length(x@add) <= 1, msg="Slot 'add' should be length <= 1")
  assertthat::assert_that(length(x@prop)<= 1, msg="Slot 'prop' should be length <= 1")
  
  endpoint<- x@endpoint
  endpointErr <- sprintf("%s_err", endpoint)
  
  if (x@combined1 || x@combined2) {
    assertthat::assert_that(length(x@add)==1, msg="Slot 'add' should be length 1")
    assertthat::assert_that(length(x@prop)==1, msg="Slot 'prop' should be length 1")

    if (x@combined1) {
      eq <- Equation(lhs=endpointErr, rhs=sprintf("%s + (%s + %s*%s)*EPS_FIX%i", endpoint, x@add, x@prop, endpoint, 1L+shift))
      return(list(equation=eq, eps=c(1)+shift))
    } else {
      eq <- Equation(lhs=endpointErr, rhs=sprintf("%s + sqrt(%s^2 + (%s^2)*(%s^2))*EPS_FIX%i", endpoint, x@add, x@prop, endpoint, 1L+shift))
      return(list(equation=eq, eps=c(1)+shift))
    }
  } else {
    if (length(x@add)==1 && length(x@prop)==0) {
      eq <- Equation(lhs=endpointErr, rhs=sprintf("%s + %s*EPS_FIX%i", endpoint, x@add, 1L+shift))
      return(list(equation=eq, eps=c(1)+shift))
      
    } else if(length(x@add)==0 && length(x@prop)==1) {
      eq <- Equation(lhs=endpointErr, rhs=sprintf("%s + %s*%s*EPS_FIX%i", endpoint, x@prop, endpoint, 1L+shift))
      return(list(equation=eq, eps=c(1)+shift))
      
    } else if(length(x@add)==1 && length(x@prop)==1) {
      eq <- Equation(lhs=endpointErr, rhs=sprintf("%s + %s*EPS_FIX%i + %s*%s*EPS_FIX%i", endpoint, x@add, x@prop, endpoint, 1L+shift, 2L+shift))
      return(list(equation=eq, eps=c(1, 2)+shift))
    }
  }
  
  # If no error model is defined, return the endpoint
  eq <- Equation(lhs=endpointErr, rhs=sprintf("%s"))
  return(list(equation=eq, eps=NULL))
}
