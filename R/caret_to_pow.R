#' 
#' Convert the caret operator into the pow function.
#'
#'@param x string to transform
#'@return transformed expression 
#'
caretToPowCore <- function(x) {
  # See https://stackoverflow.com/questions/40606723/substitute-the-power-symbol-with-cs-pow-syntax-in-mathematical-expression
  e <- parse(text=x)
  
  # a recursive function
  fun <- function(e) {    
    # check if you are at the end of the tree's branch
    if (is.name(e) || is.atomic(e)) { 
      # replace ^
      if (e == quote(`^`)) return(quote(pow))
      return(e)
    }
    # follow the tree with recursion
    for (i in seq_along(e)) e[[i]] <- fun(e[[i]])
    return(e)    
  }
  
  # deparse to get a character string    
  return(deparse1(fun(e)[[1]]))
}

#_______________________________________________________________________________
#----                           caretToPow                                  ----
#_______________________________________________________________________________

#' Convert the caret operator into the pow function.
#' 
#' @param x object to transform
#' @export
#' @rdname caretToPow
caretToPow <- function(x) {
  stop("No default function is provided")
}

setGeneric("caretToPow", function(x) {
  standardGeneric("caretToPow")
})

setMethod("caretToPow", signature=c("model_statement"), definition=function(x) {
  return(x)
})

setMethod("caretToPow", signature=c("character"), definition=function(x) {
  assertthat::assert_that(length(x)==1, msg="x should be length 1")
  containsCaret <- grepl(pattern="\\^", x=x)
  if (containsCaret) {
    x <- tryCatch({
      caretToPowCore(x)
    }, error = function(e) {
      warning("Problem detected when converting caret to pow, aborted.")
      return(x)
    })
  }
  return(x)
})

setMethod("caretToPow", signature=c("equation"), definition=function(x) {
  x@rhs <- caretToPow(x@rhs)
  return(x)
})

setMethod("caretToPow", signature=c("if_statement"), definition=function(x) {
  x@condition <- caretToPow(x@condition)
  x@equation <- caretToPow(x@equation)
  return(x)
})

setMethod("caretToPow", signature=c("code_record"), definition=function(x) {
  x@statements@list <- x@statements@list %>%
    purrr::map(~caretToPow(.x))
  return(x)
})

setMethod("caretToPow", signature=c("code_records"), definition=function(x) {
  x@list <- x@list %>%
    purrr::map(~caretToPow(.x))
  return(x)
})

setMethod("caretToPow", signature=c("compartment_property"), definition=function(x) {
  x@rhs <- caretToPow(x@rhs)
  return(x)
})

setMethod("caretToPow", signature=c("campsis_model"), definition=function(x) {
  x@model@list <- x@model@list %>%
    purrr::map(~caretToPow(.x))
  x@compartments@properties@list %>%
    purrr::map(~caretToPow(.x))
  return(x)
})

