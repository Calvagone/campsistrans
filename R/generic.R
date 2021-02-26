
#' Write is a generic function to write models.
#'
#' @param x object to write
#' @param file destination file
#' @param ... ignored
#' @export
write <- function(x, file, ...) {
  UseMethod("write")
}

#' Default write function.
#'
#' @param x object to write
#' @param file destination file
#' @param ... ignored
#' @export
write.default <- function(x, file, ...) {
  stop("Write function class ", class(x), " not supported.")
}