
checkParameter <- function(x) {
  errors <- character()
  
  # Mandatory
  errors <- addError(checkLength(x, "index"), errors)
  errors <- addError(checkLength(x, "fix"), errors)
  errors <- addError(checkLength(x, "value"), errors)
  
  # Optional
  errors <- addError(checkLengthOptional(x, "name"), errors)
  errors <- addError(checkLengthOptional(x, "suffix"), errors)
  
  if (length(errors) == 0) TRUE else errors
}

checkLength <- function(x, slot, expected=1) {
  lengthSlot <- length(eval(parse(text = paste0("x@", slot))))
  error <- NULL
  if (lengthSlot != expected) {
    error <- paste0(slot, " is length ", lengthSlot, ". Should be ", expected)
  }
  return(error)
}

checkLengthOptional <- function(x, slot, expected=1) {
  error <- NULL
  if (!is.null(checkLength(x, slot, expected=0))) {
    error <- checkLength(x, slot, expected=1)
  }
  return(error)
}

addError <- function(error, errors) {
  if (is.null(error)) {
    return(errors)
  } else {
    return(c(errors, error))
  }
}

setClass(
  "parameter",
  representation(
    name = "character",   # Optional
    index = "integer",    # Mandatory
    suffix = "character", # Optional
    fix = "logical",      # Mandatory
    value = "numeric"     # Mandatory
  ),
  validity = checkParameter
)

setClass(
  "single_array_parameter",
  representation(
  ),
  validity = checkParameter,
  contains = "parameter"
)

setClass(
  "theta",
  representation(
  ),
  validity = checkParameter,
  contains = "single_array_parameter"
)

setClass(
  "double_array_parameter",
  representation(
    index2 = "integer"
  ),
  validity = checkParameter,
  contains = "single_array_parameter"
)

isDiag <- function(object) TRUE

setGeneric("isDiag", function(object) {
  standardGeneric("isDiag")
})

setMethod("isDiag", signature(object = "double_array_parameter"), function(object) {
  return(object@index==object@index2)
})

setClass(
  "omega",
  representation(
  ),
  validity = checkParameter,
  contains = "double_array_parameter"
)

setClass(
  "sigma",
  representation(
  ),
  validity = checkParameter,
  contains = "double_array_parameter"
)

setClass(
  "parameters",
  representation(
    list = "list"
  )
)


#' Filter.
#' 
#' @param object generic object
#' @param type parameter type: theta, omega or sigma
#' @return filtered object
#' @export
filter <- function(object, type) {
  stop("No default function is provided")
}

setGeneric("filter", function(object, type) {
  standardGeneric("filter")
})

setMethod("filter", signature=c("parameters", "character"), definition=function(object, type) {
  object@list <- object@list %>% purrr::keep(~as.character(class(.x))==type)
  return(object)
})


#' Max index.
#' 
#' @param object generic object
#' @param type parameter type: theta, omega or sigma
#' @return filtered object
#' @export
maxIndex <- function(object, type) object

setGeneric("maxIndex", function(object, type) {
  standardGeneric("maxIndex")
})

setMethod("maxIndex", signature=c("parameters", "character"), definition=function(object, type) {
  return(object@list %>% purrr::map_int(~.x@index) %>% max())
})

#' Get parameter function (single index).
#' 
#' @param object generic object
#' @param type parameter type: theta
#' @param index first index
#' @return filtered object
#' @export
getParameter <- function(object, type, index) {
  stop("No default function is provided")
}

#' Get parameter function (double index).
#' 
#' @param object generic object
#' @param type parameter type: omega or sigma
#' @param index first index
#' @param index2 second index
#' @return filtered object
#' @export
getParameter <- function(object, type, index, index2) {
  stop("No default function is provided")
}

setGeneric("getParameter", function(object, type, index) {
  standardGeneric("getParameter")
})

setGeneric("getParameter", function(object, type, index, index2) {
  standardGeneric("getParameter")
})

setMethod("getParameter", signature=c("parameters", "character", "integer"), definition=function(object, type, index) {
  return(getParameter(object, type=type, index=index, index2=integer()))
})

setMethod("getParameter", signature=c("parameters", "character", "integer", "integer"), definition=function(object, type, index, index2) {
  subList <- object %>% filter(type=type)
  if (type=="theta") {
    parameter <- subList@list %>% purrr::keep(~(.x@index==index))
  } else {
    parameter <- subList@list %>% purrr::keep(~(.x@index==index)&(.x@index2==index2))
  }
  if (length(parameter) >= 1) {
    parameter <- parameter[[1]]
  }
  return(parameter)
})




