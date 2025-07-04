
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
  return("python3_env_pharmpy_v0_46")
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
  requirements <- campsistrans::getPharmpyRequirements()
  reqPath <- tempfile(fileext=".txt")
  fileConn <- file(reqPath)
  writeLines(requirements, fileConn)
  close(fileConn)
  env <- reticulate::virtualenv_create(envname=envname, python=python, requirements=reqPath)
  reticulate::use_virtualenv(envname, required=TRUE)
}

#'
#' Import python package without error.
#' 
#' @param package package name
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
