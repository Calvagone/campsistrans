
# setwd("C:/prj/campsistrans/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsistrans/tests/")
# testFolder <- "C:/prj/campsistrans/tests/testthat/"

# 1) Install Python 3.9 (custom installation to C/Python/Python39/, with PIP, add python to PATH)
# 2) Possibly remove python environment: reticulate::virtualenv_remove(getPythonEnvName())
# 3) This environment will be re-installed when importNONMEM is called!
# That's all!

# # Load Pharmy through reticulate
# pharmpy <- reticulate::import("pharmpy")
# 
# # Check Pharmpy version
# version <- pharmpy["__version__"]

skipTests <- function(name, default) {
  option <- getCampsistransOption()
  if (is.null(option)) {
    return(default)
  } else {
    value <- option[[name]]
    if (is.null(value)) {
      return(default)
    } else {
      return(value)
    }
  }
}

skipPharmpyTests <- function() {
  return(skipTests(name="SKIP_PHARMPY_TESTS", default=campsis::onCI()))
}

getCampsistransOption <- function() {
  return(getOption("campsistrans.options"))
}
