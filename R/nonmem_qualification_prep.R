
#' Standardise dataset (dataframe) received from CAMPSIS.
#' This process will be implemented later on in CAMPSIS (using export dest='NONMEM').
#' 
#' @param dataset dataframe or tibble
#' @return a standardised dataset
#' @importFrom dplyr mutate_at
#' @importFrom tibble add_column as_tibble
#' @export
standardiseDataset <- function(dataset) {
  # A couple of checks
  if (!is(dataset, "data.frame")) {
    stop("dataset must be a data frame")
  }
  if (!is(dataset, "tbl_df")) {
    dataset <- dataset %>% tibble::as_tibble()
  }
  
  # Add DV column if not present
  if (!("DV" %in% colnames(dataset))) {
    dataset <- dataset %>% tibble::add_column(DV=".", .after="MDV")
  }
  
  # Export to character dataframe
  dataset <- dataset %>% dplyr::mutate_at(.vars=colnames(dataset), .funs=as.character)
  
  # Replace NAs by dots
  dataset <- dataset %>% dplyr::mutate_at(.vars=colnames(dataset), .funs=~ifelse(is.na(.x), ".", .x))
  
  return(dataset)
}

updateCtl <- function(model, control_stream) {
  # Replace control stream with updated one
  internals <- model$internals$replace(control_stream=control_stream)
  model <- model$replace(
    internals = internals
  )
  
  # Update source
  model <- model$update_source()
  
  return(model)
}

#' Load control stream with Pharmpy.
#' 
#' @param path path to original control stream
#' @param estimate reuse estimated parameters from the model fit, default is TRUE
#' @return the pharmpy model
#' @export
loadCtl <- function(path, estimate) {
  pharmpy <- importPharmpyPackage(UpdatedPharmpyConfig())
  model <- pharmpy$modeling$read_model(path)
  
  # Retrieve the parameter estimates
  if (estimate) {
    results <- pharmpy$tools$read_modelfit_results(path)
    parameter_estimates <- results$parameter_estimates
    
    # Replace initial estimates with the parameter estimates
    parameters <- model$parameters
    parameters <- parameters$set_initial_estimates(as.list(parameter_estimates))
    
    # Replace parameters in original model
    model <- model$replace(parameters=parameters)
  }
  
  return(model)
}

#' Prepare simulation control stream for qualification and execute it with NONMEM.
#' 
#' @param file path to control stream file
#' @param updateInits update initial conditions in the control stream, default is TRUE (lst required)
#' @param model Campsis model that was previously imported (used for mapping the ETAs)
#' @param dataset simulation dataset, data frame
#' @param variables variables to output
#' @param folder where to write the control stream
#' @param reexecuteNONMEM re-execute NONMEM if results already exist, default is TRUE
#' @return the path to the simulation control stream
#' @export
executeSimulationCtl <- function(file, updateInits=TRUE, model, dataset, variables, folder, reexecuteNONMEM=TRUE) {
  # Create fresh folder if NONMEM is to be re-executed
  if (reexecuteNONMEM) {
    if (dir.exists(folder)) {
      unlink(folder, recursive=TRUE)
    }
    dir.create(folder, recursive=TRUE)
  }
  
  # Update inits
  if (updateInits) {
    path <- updateInitsNONMEM(ctl=file)
  } else {
    path <- file
  }
  
  # Read estimation control stream
  ctl <- readLines(path) %>%
    removeNONMEMComments() %>%
    paste0(collapse="\n")
  
  # Remove all tables
  ctl <- removeNONMEMBlock(x=ctl, name="TABLE", first=FALSE)
  
  # Remove all estimation blocks
  ctl <- removeNONMEMBlock(x=ctl, name="ESTIMATION", first=FALSE)
  
  # Remove all covariance blocks
  ctl <- removeNONMEMBlock(x=ctl, name="COVARIANCE", first=FALSE)
  
  # Retrieve equations block and adapt
  for (blockName in c("PK", "ERROR")) {
    currentBlock <- extractNONMEMBlock(x=ctl, name=blockName, first=TRUE, raise_error=FALSE)
    if (length(currentBlock) > 0) {
      ctl <- replaceNONMEMBlock(x=ctl, name=blockName, content=setEtasAsCovariates(model, currentBlock@content))
    }
  }

  # Prepare single TABLE to output
  variablesDataset <- colnames(dataset)
  defaultVariables <- c("ID", "TIME", "EVID")
  defaultVariables <- defaultVariables[defaultVariables %in% variablesDataset]
  allVariables <- unique(c(defaultVariables, variables))
  tableStr <- sprintf("$TABLE %s FILE=output.tab ONEHEADER NOAPPEND NOPRINT\n", paste0(allVariables, collapse=" "))
  ctl <- paste0(ctl, "\n", tableStr)
  
  # Add SIMULATION block
  ctl <- paste0(ctl, "\n", "$SIMULATION (1234) SUBPROBLEMS=1 ONLYSIMULATION")
  
  # Replace INPUT
  ctl <- replaceNONMEMBlock(x=ctl, name="INPUT", content=colnames(dataset) %>% paste0(collapse=" "))
  
  # Replace DATA
  ctl <- replaceNONMEMBlock(x=ctl, name="DATA", content="dataset.csv IGNORE=@")

  # Write control stream
  ctlFile <- file.path(folder, "model.mod")
  fileConn <- file(ctlFile)
  writeLines(text=ctl, fileConn)
  close(fileConn)
  
  # Write NONMEM dataset
  write.csv(dataset, file=file.path(folder, "dataset.csv"), quote=FALSE, row.names=FALSE)
  
  # Execute NONMEM
  results <- executeNONMEM(folder=folder, reexecuteNONMEM=reexecuteNONMEM)
  
  return(results)
}


#' Execute NONMEM. PsN will be called automatically by R. 
#' Prepared control stream 'model.mod' is executed automatically and NONMEM results
#' are returned in the form of a data frame.
#' 
#' @param folder qualification folder where the control stream is
#' @param reexecuteNONMEM force re-execute NONMEM if results already exist
#' @param ctl_name control stream name, default is 'model.mod'
#' @export
executeNONMEM <- function(folder, reexecuteNONMEM=T, ctl_name="model.mod") {
  tabFile <- paste0(folder, "/", "output.tab")
  if (!file.exists(tabFile) || reexecuteNONMEM) {
    system("cmd.exe", input=paste0("cd ","\"", folder, "\"", " & ", "execute ", ctl_name))
    unlink(paste0(folder, "/", "modelfit_dir1"), recursive=TRUE)
  }
  nonmem <- read.nonmem(tabFile)[[1]] %>% as.data.frame()
  return(nonmem)
}

#' Execute NONMEM. PsN will be called automatically by R. 
#' Prepared control stream 'model.mod' is executed automatically and NONMEM results
#' are returned in the form of a data frame.
#' 
#' @param ctl path to control stream
#' @return output path to PsN output
#' @export
updateInitsNONMEM <- function(ctl) {
  if (!file.exists(ctl)) {
    stop("Control stream file does not exist: ", ctl)
  }
  ctl <- normalizePath(ctl, winslash="/")
  folder <- dirname(ctl)
  ctlName <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(ctl))
  lst <- file.path(folder, paste0(ctlName, ".lst"))
  
  if (!file.exists(lst)) {
    stop("Control stream file does not contain results: ", lst) 
  }
  output <- sprintf("%s_updated.mod", ctlName)
  system("cmd.exe", input=sprintf("cd \"%s\" & update_inits %s %s -output_model=\"%s\"", 
                                  folder, basename(ctl), basename(lst), output))
  output <- file.path(folder, output)
  if (!file.exists(output)) {
    stop("Control stream file was not updated: ", output)
  }
  return(output)
}

#' Set ETAs as covariates in NONMEM block.
#' 
#' @param model Campsis parameters
#' @param content NONMEM content
#' @return updated content with ETAs replaced by their names
#' 
setEtasAsCovariates <- function(model, content) {
  omegas <- model@parameters %>%
    campsismod::select("omega")
  omegas@list <- omegas@list %>%
    purrr::keep(~campsismod::isDiag(.x))
  
  for (omega in omegas@list) {
    etaName <- campsismod::getNameInModel(omega)
    index <- omega@index
    content <- gsub(pattern=sprintf("(?<![A-Z0-9_])ETA\\(%i\\)", index), replacement=etaName, x=content, perl=TRUE)
  }
  
  temp <- strsplit(content, split="=")
  lhsRhsSame <- temp %>% purrr::map_lgl(.f=function(x) {
    if (length(x) == 2) {
      return(trimws(x[1]) == trimws(x[2]))
    } else {
      return(FALSE)
    }
  })
  
  return(content[!lhsRhsSame])
}
