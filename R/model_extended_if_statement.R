
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
  object@condition <- object@condition %>%
    campsismod::replaceAll(pattern=pattern, replacement=replacement, ...)
  object@statements@list <- object@statements@list %>%
    purrr::map(~campsismod::replaceAll(object=.x, pattern=pattern, replacement=replacement, ...))
  return(object)
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

#'@importFrom campsismod is_rxode
setMethod("toString", signature=c("extended_if_statement"), definition=function(object, ...) {
  dest <- campsismod::processExtraArg(args=list(...), name="dest", default="campsis")
  indent <- "  "
  statementsStr <- object@statements@list %>% 
    purrr::map_chr(.f=function(statement) {
      return(paste0(indent, statement %>% campsismod::toString(dest=dest, init=FALSE)))
    }) %>% 
    paste0(collapse="\n")

  if (is(object, "else_if_statement")) {
    condition <- sprintf(" (%s)", object@condition)
    ifStr <- "else if"
  } else if(is(object, "else_statement")) {
    condition <- ""
    ifStr <- "else"
  } else if (is(object, "extended_if_statement")) {
    condition <- sprintf(" (%s)", object@condition)
    ifStr <- "if"
  } else {
    stop("Should never occur")
  }
  
  if (dest=="campsis" || campsismod::is_rxode(dest) || dest=="mrgsolve") {
    retValue <- sprintf("%s%s {\n%s\n}", ifStr, condition, statementsStr)
  } else if (dest=="NONMEM") {
    retValue <- sprintf("%s%s {\n%s\n}", toupper(ifStr), condition, statementsStr)
  } else {
    stop("Only rxode2 (previously RxODE), mrgsolve or campsis are supported")
  }
  
  return(retValue)
})
