
#' Print symbolic expressions or conditions.
#' 
#' @param x SymPy expression or condition
#' @param output type of desired output
#' @return code
#' @importFrom reticulate import py_capture_output
#' @export
printSymPy <- function(x, output = "C") {
  sympy <- reticulate::import("sympy")
  if (output == "C") {
    print <- reticulate::py_capture_output(sympy$print_ccode(x))
    print <- gsub("[\r\n]", "", print)
  } else {
    print <- as.character(x)
  }
}
