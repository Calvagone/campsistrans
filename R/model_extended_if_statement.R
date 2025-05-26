
#_______________________________________________________________________________
#----                       extended_if_statement class                     ----
#_______________________________________________________________________________

#' 
#' Extended if-statement class.
#' 
#' @slot condition if-statement condition
#' @slot statements list of statements
#' @export
setClass(
  "extended_if_statement",
  representation(
    condition = "character",
    statements = "model_statements"
  ),
  contains = "model_statement",
  validity = function(object) {
    return(campsismod:::expectOne(object, "condition"))
  }
)

#' 
#' Create a new IF-statement.
#' 
#' @param condition condition, single character string
#' @param statements list of statements
#' @return an extended if-statement
#' @export
ExtendedIfStatement <- function(condition, statements) {
  return(new("extended_if_statement", condition=condition, statements=statements))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

setMethod("getName", signature = c("extended_if_statement"), definition = function(x) {
  return(sprintf("EXTENDED IF (%s): %s", x@condition, x@statements@list %>% purrr::map_chr(.f=function(statement) {
    return(statement %>% getName())
  }) %>% paste(collapse=", ")))
})

#_______________________________________________________________________________
#----                             replaceAll                                ----
#_______________________________________________________________________________

setMethod("replaceAll", signature=c("extended_if_statement", "pattern", "character"), definition=function(object, pattern, replacement, ...) {
  object@condition <- object@condition %>% replaceAll(pattern=pattern, replacement=replacement, ...)
  object@statements <- object@statements %>% replaceAll(pattern=pattern, replacement=replacement, ...)
  return(object)
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

setMethod("toString", signature=c("extended_if_statement"), definition=function(object, ...) {
  stop("Unsupported yet")
})
