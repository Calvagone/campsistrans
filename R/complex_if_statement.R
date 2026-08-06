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
#----                            get_name                                   ----
#_______________________________________________________________________________

setMethod(
  "get_name",
  signature = c("complex_if_else_statement"),
  definition = function(x) {
    return(sprintf(
      "COMPLEX IF STATEMENT: %s",
      x@list %>%
        purrr::map_chr(.f = function(statement) {
          return(statement %>% get_name())
        }) %>%
        paste0(collapse = " / ")
    ))
  }
)

#_______________________________________________________________________________
#----                            replace_all                                ----
#_______________________________________________________________________________

setMethod(
  "replace_all",
  signature = c("complex_if_else_statement", "pattern", "character"),
  definition = function(object, pattern, replacement, ...) {
    object@list <- object@list %>%
      purrr::map(
        ~ campsismod::replace_all(
          object = .x,
          pattern = pattern,
          replacement = replacement,
          ...
        )
      )
    return(object)
  }
)

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

setMethod(
  "add",
  signature = c("complex_if_else_statement", "extended_if_statement"),
  definition = function(object, x) {
    object@list <- object@list %>% append(x)
    return(object)
  }
)

#'
#' Else-if statement class.
#'
#' @export
setClass(
  "else_if_statement",
  representation(),
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
  return(new(
    "else_if_statement",
    condition = condition,
    statements = statements
  ))
}

#'
#' Else statement class.
#'
#' @export
setClass(
  "else_statement",
  representation(),
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
  return(new("else_statement", condition = "", statements = statements))
}

#_______________________________________________________________________________
#----                             to_string                                 ----
#_______________________________________________________________________________

setMethod(
  "to_string",
  signature = c("complex_if_else_statement"),
  definition = function(object, ...) {
    retValue <- NULL
    for (elem in object@list) {
      retValue <- retValue %>% append(campsismod::to_string(elem, ...))
    }
    return(retValue)
  }
)
