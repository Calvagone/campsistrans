#_______________________________________________________________________________
#----                         pharmpy_config class                          ----
#_______________________________________________________________________________

setClass(
  "pharmpy_config",
  representation(
    envname = "character",
    python = "character",
    requirements = "character"
  )
)

#' Old Pharmpy config.
#' 
#' @export
OldPharmpyConfig <- function() {
  config <- config::get()
  unix <- .Platform$OS.type == "unix"
  pythonPath <- ifelse(unix, config$python_linux, config$python_windows_39)
  return(new("pharmpy_config", envname="python3_env_pharmpy_v0_46", python=pythonPath,
             requirements=getPharmpyRequirementsOld()))
}

#' Updated Pharmpy config.
#' 
#' @export
UpdatedPharmpyConfig <- function() {
  config <- config::get()
  unix <- .Platform$OS.type == "unix"
  pythonPath <- ifelse(unix, config$python_linux, config$python_windows_313)
  return(new("pharmpy_config", envname="python3_env_pharmpy_v1_7_2", python=pythonPath,
             requirements=getPharmpyRequirementsUpdated()))
}

#_______________________________________________________________________________
#----                       Installation utilities                          ----
#_______________________________________________________________________________


#'
#' Install Pharmpy environment from R.
#' 
#' @param config Pharmpy configuration object, default is UpdatedPharmpyConfig()
#' @return virtual environment
#' @importFrom reticulate virtualenv_create virtualenv_install use_virtualenv
#' @export
#' 
installPharmpy <- function(config=UpdatedPharmpyConfig()) {
  requirements <- config@requirements
  reqPath <- tempfile(fileext=".txt")
  fileConn <- file(reqPath)
  writeLines(requirements, fileConn)
  close(fileConn)
  env <- reticulate::virtualenv_create(envname=config@envname, python=config@python, requirements=reqPath)
  reticulate::use_virtualenv(config@envname, required=TRUE)
}

#'
#' Import python package without error.
#' 
#' @param config pharmpy config, default is UpdatedPharmpyConfig()
#' @return NULL if an error occurs
#' @importFrom reticulate import use_virtualenv
#' @export
#' 
importPharmpyPackage <- function(config=UpdatedPharmpyConfig()) {
  env <- tryCatch(expr={
    reticulate::use_virtualenv(config@envname, required=TRUE)
  }, error=function(cond) {
    return(NULL)
  })
  if (is.null(env)) {
    return(NULL)
  }
  module <- tryCatch(expr={
    reticulate::import("pharmpy")
  }, error=function(cond) {
    return(NULL)
  })
  return(module)
}

#'
#' Install python in the background.
#' 
#' @param config Pharmpy configuration object, default is UpdatedPharmpyConfig()
#' @return callr S4 object
#' @importFrom callr r_bg
#' @importFrom config get
#' @export
#' 
installPharmpyInBackground <- function(config=UpdatedPharmpyConfig()) {
  pkgconfig <- config::get()
  x <- callr::r_bg(
    func=installPharmpy,
    args=list(config=config),
    supervise = TRUE,
    stdout=pkgconfig$python_installation_stdout,
    stderr=pkgconfig$python_installation_stderr
  )
  return(x)
}
