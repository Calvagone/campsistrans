
checkParameter <- function(object) {
  errors <- character()
  
  # Mandatory
  errors <- addError(checkLength(object, "index"), errors)
  errors <- addError(checkLength(object, "fix"), errors)
  errors <- addError(checkLength(object, "value"), errors)
  
  # Optional
  errors <- addError(checkLengthOptional(object, "name"), errors)
  errors <- addError(checkLengthOptional(object, "suffix"), errors)
  
  if (length(errors) == 0) TRUE else errors
}

checkLength <- function(object, slot, expected=1) {
  lengthSlot <- length(eval(parse(text = paste0("object@", slot))))
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
    name = "character",   # Optional, either name is provided or suffix
    index = "integer",    # Mandatory
    suffix = "character", # Optional, either name is provided or suffix
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

#_______________________________________________________________________________
#----                                 filter                                ----
#_______________________________________________________________________________

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

#_______________________________________________________________________________
#----                                maxIndex                               ----
#_______________________________________________________________________________

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
  return((object %>% filter(type=type))@list %>% purrr::map_int(~.x@index) %>% max())
})

#_______________________________________________________________________________
#----                             getParameter                              ----
#_______________________________________________________________________________

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

#_______________________________________________________________________________
#----                            getNONMEMName                              ----
#_______________________________________________________________________________

#' Get NONMEM name.
#' 
#' @param object generic object
#' @return NONMEM name
#' @export
getNONMEMName <- function(object) {
  stop("No default function is provided")
}

setGeneric("getNONMEMName", function(object) {
  standardGeneric("getNONMEMName")
})

setMethod("getNONMEMName", signature=c("theta"), definition=function(object) {
  return(paste0("THETA(", object@index, ")"))
})

setMethod("getNONMEMName", signature=c("omega"), definition=function(object) {
  return(paste0("OMEGA(", object@index, ",", object@index2, ")"))
})

setMethod("getNONMEMName", signature=c("sigma"), definition=function(object) {
  return(paste0("OMEGA(", object@index, ",", object@index2, ")"))
})

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

#' Get name in model code.
#' 
#' @param object generic object
#' @param type parameter type
#' @param index parameter index
#' @return name
#' @export
getName <- function(object, type, index) {
  stop("No default function is provided")
}

setGeneric("getName", function(object) {
  standardGeneric("getName")
})

getBestName <- function(prefix, name, suffix, index) {
  retValue <- ""
  if (length(name)==0) {
    if (length(suffix)==0) {
      retValue <- paste0(prefix, "_", index)
    } else {
      retValue <- paste0(prefix, "_", suffix)
    }
  } else {
    retValue <- name
  }
  return(retValue)
}

setMethod("getName", signature=c("theta"), definition=function(object) {
  return(getBestName("THETA", object@name, object@suffix, object@index))
})

setMethod("getName", signature=c("omega"), definition=function(object) {
  return(getBestName("ETA", object@name, object@suffix, object@index))
})

setMethod("getName", signature=c("sigma"), definition=function(object) {
  return(getBestName("EPS", object@name, object@suffix, object@index))
})




