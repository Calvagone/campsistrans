
#' 
#' Complex if statement class.
#' 
#' @slot list list of IF statements
#' @export
setClass(
  "complex_if_statement",
  representation(
    list = "list"
  ),
  contains = "model_statement",
  validity = function(object) {
    return(TRUE)
  }
)

#' 
#' Create a new complex IF statement.
#' 
#' @return an complex IF statement
#' @export
ComplexIfStatement <- function() {
  return(new("complex_if_statement"))
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

setMethod("add", signature=c("complex_if_statement", "if_statement"), definition=function(object, x) {
  object@list <- object@list %>% append(x)
  return(object)
})

#' 
#' Else-if statement class.
#' 
#' @slot list list of IF statements
#' @export
setClass(
  "else_if_statement",
  representation(
  ),
  contains = "if_statement",
  validity = function(object) {
    return(TRUE)
  }
)

#' 
#' Create a new ELSE-IF statement.
#' 
#' @param condition condition, single character string
#' @param equation equation if condition is met
#' @param comment comment if any, single character string
#' @return an ELSE-IF statement
#' @export
ElseIfStatement <- function(condition, equation, comment=as.character(NA)) {
  return(new("else_if_statement", condition=condition, equation=equation, comment=comment))
}

#' 
#' Else statement class.
#' 
#' @slot list list of IF statements
#' @export
setClass(
  "else_statement",
  representation(
  ),
  contains = "if_statement",
  validity = function(object) {
    return(TRUE)
  }
)

#' 
#' Create a new ELSE statement.
#' 
#' @param equation equation if condition is met
#' @param comment comment if any, single character string
#' @return an ELSE statement
#' @export
ElseStatement <- function(equation, comment=as.character(NA)) {
  return(new("else_statement", condition="", equation=equation, comment=comment))
}
