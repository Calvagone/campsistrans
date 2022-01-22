
#' Import a NONMEM dataset and prepare it for the qualification.
#'
#' @param campsistrans campsistrans object
#' @param covariates covariates vector. If provided, only these covariates are kept in dataset.
#'  NULL is default (all covariates are kept)
#' @return a data frame
#' @importFrom dplyr all_of relocate rename_at select
#' @importFrom purrr keep map_chr
#' @export
importDataset <- function(campsistrans, covariates=NULL) {
  pharmpy <- campsistrans@model[[1]]
  
  # Loading dataset
  data <- pharmpy$control_stream$get_records("DATA")
  if (data %>% length() == 0) {
    stop("No DATA section in control stream")
  }
  data <- data[[1]]
  dataset <- read.csv(file=paste0(campsistrans@dirname, "/", data$filename))
  columnNames <- colnames(dataset)
  columnNamesLength <- columnNames %>% length()
  
  # Looking at INPUT definition
  input <- pharmpy$control_stream$get_records("INPUT")
  if (input %>% length() == 0) {
    stop("No INPUT section in control stream")
  }
  input <- input[[1]]
  
  # All options in INPUT
  options <- input$all_options
  optionsLength <- options %>% length()
  
  if (optionsLength != columnNamesLength) {
    stop(paste0("INPUT has ", optionsLength, " entries while dataset has ", columnNamesLength, " columns."))
  }
  
  optionKeys <- options %>% purrr::map_chr(~.x$key)
  optionValues <- options %>% purrr::map_chr(~ifelse(is.null(.x$value), NA, .x$value))
  
  # Copy original dataset
  dataset_ <- dataset
  
  # Overwrite column headers with the keys
  colnames(dataset_) <- optionKeys
  
  # DROP column indexes
  optionsToDrop <- options %>% purrr::keep(~(!is.null(.x$value) && .x$value == "DROP"))
  for (optionToDrop in optionsToDrop) {
    dataset_ <- dataset_ %>% dplyr::select(-dplyr::all_of(optionToDrop))
  }
  
  # Rename necessary columns
  optionsToRename <- options %>% purrr::keep(~(!is.null(.x$value) && .x$value != "DROP"))
  for (optionToRename in optionsToRename) {
    dataset_ <- dataset_ %>% dplyr::rename_at(.vars=optionToRename$key, .funs=~optionToRename$value)
  }
  
  # NONMEM important variables
  nmVariables <- c("ID","TIME","DV","MDV","EVID", "AMT", "CMT", "RATE")
  
  # Remove unnecessary columns
  if (!is.null(covariates)) {
    dataset_ <- dataset_ %>% dplyr::select(dplyr::all_of(c(nmVariables, covariates)))
  }
  
  # Standardise dataset
  dataset_ <- dataset_ %>% dplyr::relocate(dplyr::any_of(nmVariables))
  
  return(dataset_)
}

#' Import ETA's.
#'
#' @param x imported NONMEM dataset
#' @param file TAB file with estimated ETA's
#' @param model CAMPSIS model
#' @param id mapping ID name
#' @return updated data frame
#' @importFrom dplyr filter filter_at group_by_at left_join pull rename_at select_at ungroup
#' @importFrom purrr map_chr
#' @export
importETAs <- function(x, file, model, id="ID") {
  
  # ID name to map dataset with ETA's
  mappingIDName <- id
  
  # Import estimated subjects
  tab <- read.nonmem(file=file)[[1]] %>%
    dplyr::group_by_at(mappingIDName) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::ungroup()
  
  # Standardise ETA names
  tabNames <- colnames(tab)
  etaNames <- tabNames[grep("^(ET\\d+)|(ETA\\d+)$", tabNames)]
  tab <- tab %>% dplyr::select_at(c(mappingIDName, etaNames))
  tab <- tab %>% dplyr::rename_at(.vars=etaNames, .funs=function(etaName) {
    etaNumber <- as.numeric(sub(pattern = "(ET|ETA)", replacement = "", etaName))
    retValue <- etaNumber %>% purrr::map_chr(.f = function(eta) {
      omega <- model@parameters %>% getByIndex(Omega(index=eta, index2=eta))
      paste0("ETA_", omega@name)
    })
    return(retValue)
  })
  
  # Left join
  uniqueIDs <- unique(tab %>% dplyr::pull(mappingIDName))
  x_ <- x %>% dplyr::filter_at(.vars=mappingIDName, .vars_predicate=~.x %in% uniqueIDs) %>%
    dplyr::left_join(tab, by=mappingIDName)
  
  return(x_)
}

#' Add simulation ID column.
#'
#' @param dataset NONMEM dataset
#' @param id 
#' @return updated data frame
#' @importFrom dplyr arrange group_by group_indices rename_at select
#' @export
addSimulationIDColumn <- function(dataset, id="ID") {
  if ("ID" %in% colnames(dataset) && id != "ID") {
    dataset <- dataset %>% dplyr::select(-ID)
  }
  if (!("ORIGINAL_ID" %in% colnames(dataset))) {
    dataset <- dataset %>% dplyr::rename_at(.vars=id, .funs=function(x){"ORIGINAL_ID"})
  }
  dataset <- dataset %>% dplyr::arrange(ORIGINAL_ID) # Not sure this line is needed
  dataset <- dataset %>% tibble::add_column(ID=dataset %>% dplyr::group_by(ORIGINAL_ID) %>%
                                              dplyr::group_indices(), .before="ORIGINAL_ID")
  dataset <- dataset %>% dplyr::arrange(ID) # This may be needed in some cases
  return(dataset)
}
