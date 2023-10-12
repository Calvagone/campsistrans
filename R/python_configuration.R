
#'
#' Get path to python executable. This path can be changed in config.yml. 
#' 
#' @return path to python
#' @importFrom config get
#' @export
#' 
getPythonPath <- function() {
  config <- config::get()
  local <- .Platform$OS.type != "unix"
  
  if (local) {
    pythonPath <- config$python_windows
  } else {
    pythonPath <- config$python_linux
  }
  return(pythonPath)
}

#'
#' Get default python environment name.
#' @export
#' 
getPythonEnvName <- function() {
  return("python3_env_with_pharmpy_v0_46")
}

# reticulate::virtualenv_remove(getPythonEnvName())

#'
#' Install python environment and pharmpy.
#' 
#' @param envname name of the virtual environment name
#' @param python path to distribution of python (3.9 is recommended)
#' @return virtual environment
#' @importFrom reticulate virtualenv_create virtualenv_install use_virtualenv
#' @export
#' 
installPython <- function(envname, python) {
  env <- reticulate::virtualenv_create(envname=envname, python=python) 
  reticulate::virtualenv_install(envname, packages=c("pharmpy-core@git+https://github.com/Calvagone/pharmpy@5ef5633ba837cf2f0cede1c90b608064057392ec")) # Bump version: 0.45.0 → 0.46.0
  reticulate::use_virtualenv(envname, required=TRUE)
}

#'
#' Import python package without error.
#' 
#' @return NULL if an error occurs
#' @importFrom reticulate import use_virtualenv
#' @export
#' 
importPythonPackage <- function(package) {
  env <- tryCatch(expr={
    reticulate::use_virtualenv(getPythonEnvName(), required=TRUE)
  }, error=function(cond) {
    return(NULL)
  })
  if (is.null(env)) {
    return(NULL)
  }
  module <- tryCatch(expr={
    reticulate::import(package)
  }, error=function(cond) {
    return(NULL)
  })
  return(module)
}

#'
#' Install python in the background.
#' 
#' @return callr S4 object
#' @importFrom callr r_bg
#' @importFrom config get
#' @export
#' 
installPythonInBackground <- function() {
  config <- config::get()
  x <- callr::r_bg(
    func=installPython,
    args=list(envname=getPythonEnvName(), python=getPythonPath()),
    supervise = TRUE,
    stdout=config$python_installation_stdout,
    stderr=config$python_installation_stderr
  )
  return(x)
}
