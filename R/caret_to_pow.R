#' 
#' Convert the caret operator into the pow function.
#'
#'@param expr expression to transform
#'@return transformed expression 
#'@importFrom symengine get_args get_type 
#'
caretToPowCore <- function(expr) {
  # Power detected
  if (class(expr) == "Basic" && symengine::get_type(expr) == "Pow") {
    base <- caretToPowCore(symengine::get_args(expr)[[1]])
    exponent <- caretToPowCore(symengine::get_args(expr)[[2]])
    return(sprintf("pow(%s, %s)", base, exponent))
  }
  # Addition, multiplication, subtraction, division, detected, etc
  if (class(expr) == "Basic" && length(symengine::get_args(expr)) > 0) {
    args <- lapply(symengine::get_args(expr), caretToPowCore)
    op <- symengine::get_type(expr)
    op_str <- switch(op,
                     "Add" = " + ",
                     "Mul" = " * ",
                     "Sub" = " - ",
                     "Div" = " / ",
                     "" # fallback
    )
    return(paste(args, collapse = op_str))
  }
  # Variable or number
  return(as.character(expr))
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

#'@importFrom symengine S
setMethod("caretToPow", signature=c("character"), definition=function(x) {
  assertthat::assert_that(length(x)==1, msg="x should be length 1")
  containsCaret <- grepl(pattern="\\^", x=x)
  if (containsCaret) {
    x <- tryCatch({
      caretToPowCore(symengine::S(x))
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

