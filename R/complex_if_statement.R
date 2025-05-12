
#' 
#' Complex if-else statement class.
#' 
#' @slot list list of IF statements
#' @export
setClass(
  "complex_if_else_statement",
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
ComplexIfElseStatement <- function() {
  return(new("complex_if_else_statement"))
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

setMethod("add", signature=c("complex_if_else_statement", "extended_if_statement"), definition=function(object, x) {
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
  contains = "extended_if_statement",
  validity = function(object) {
    return(TRUE)
  }
)

#' 
#' Create a new ELSE-IF statement.
#' 
#' @param condition condition, single character string
#' @param statements embedded statements
#' @return an ELSE-IF statement
#' @export
ElseIfStatement <- function(condition, statements) {
  return(new("else_if_statement", condition=condition, statements=statements))
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
  contains = "extended_if_statement",
  validity = function(object) {
    return(TRUE)
  }
)

#' 
#' Create a new ELSE statement.
#' 
#' @param statements embedded statements
#' @return an ELSE statement
#' @export
ElseStatement <- function(statements) {
  return(new("else_statement", condition="", statements=statements))
}
