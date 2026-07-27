
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
#----                            get_name                                   ----
#_______________________________________________________________________________

setMethod("get_name", signature = c("extended_if_statement"), definition = function(x) {
  return(sprintf("EXTENDED IF (%s): %s", x@condition, x@statements@list %>% purrr::map_chr(.f=function(statement) {
    return(statement %>% get_name())
  }) %>% paste(collapse=", ")))
})

#_______________________________________________________________________________
#----                            replace_all                                ----
#_______________________________________________________________________________

setMethod("replace_all", signature=c("extended_if_statement", "pattern", "character"), definition=function(object, pattern, replacement, ...) {
  object@condition <- object@condition %>%
    campsismod::replace_all(pattern=pattern, replacement=replacement, ...)
  object@statements@list <- object@statements@list %>%
    purrr::map(~campsismod::replace_all(object=.x, pattern=pattern, replacement=replacement, ...))
  return(object)
})

#_______________________________________________________________________________
#----                             to_string                                 ----
#_______________________________________________________________________________

#'@importFrom campsismod is_rxode
setMethod("to_string", signature=c("extended_if_statement"), definition=function(object, ...) {
  dest <- campsismod::process_extra_arg(args=list(...), name="dest", default="campsis")
  indent <- "  "
  statementsStr <- object@statements@list %>% 
    purrr::map_chr(.f=function(statement) {
      return(paste0(indent, statement %>% campsismod::to_string(dest=dest, init=FALSE)))
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
