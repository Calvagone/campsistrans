
#' Standardise NONMEM dataset.
#'
#' @param dataset NONMEM dataset
#' @return updated dataset
#' @export
standardiseNMDataset <- function(dataset) {
  mandatoryNMVariables <- c("ID","TIME","DV","AMT")
  columnNames <- colnames(dataset)
  
  if (!all(mandatoryNMVariables %in% columnNames)) {
    stop("NONMEM dataset must have the mandatory columns ID, TIME, AMT and DV")
  }

  if (!("MDV" %in% columnNames)) {
    dataset$MDV <- ifelse(dataset$AMT==0, 0, 1)
  }
  if (!("EVID" %in% columnNames)) {
    dataset$EVID <- dataset$MDV
  }
  if (!("RATE" %in% columnNames)) {
    dataset$RATE <- 0
  }
  if (!("CMT" %in% columnNames)) {
    dataset$CMT <- 1 # TO DO, use DEFDOSE and DEFOBS
  }

  return(dataset)
}

#' Import a NONMEM dataset and prepare it for the qualification.
#'
#' @param file path to NONMEM control stream (dataset path will be automatically extracted)
#' @param covariates covariates vector. If provided, only these covariates are kept in dataset.
#'  NULL is default (all covariates are kept)
#' @param etas import estimated ETA's that were output by $TABLE in control stream.
#' By default, ETA's are not imported. Please set it to TRUE to enable this feature.
#' @param etas_zero if previous argument is set to FALSE, etas_zero set to TRUE will
#' all ETA's from model to 0 (useful to simulate model without IIV)
#' @param table_no table number to look for the ETAs, default is 1
#' @param campsis_id rework ID column for simulation with CAMPSIS (ID must start at 1 and must be consecutive).
#' Default is FALSE. Please set it to TRUE if you wish a simulation ID. If TRUE, original ID column is
#' preserved in column 'ORIGINAL_ID'.
#' @return a data frame
#' @importFrom campsismod getNameInModel isDiag
#' @importFrom dplyr all_of relocate rename_at select
#' @importFrom purrr keep map_chr
#' @export
importDataset <- function(file, covariates=NULL, etas=FALSE, table_no=1, etas_zero=FALSE, campsis_id=FALSE) {
  
  path <- file.path("C:/Calvagone/Clients/Kynexis/24PXC0289_PKPD/runPKPMPD007/NONMEM/", "runPKPMPD007_QUAL.mod")
  covariates <- NULL
  
  # Read control stream
  ctlLines <- readLines(path) %>%
    removeNONMEMComments()
  ctl <- paste0(ctlLines_, collapse="\n")

  # Looking at DATA block
  data <- extractNONMEMBlock(x=ctl, name="DATA")
  datasetFilename <- gsub(pattern="^(.*?\\.(CSV|csv)).*", replacement="\\1", x=data@content[1])
  datasetPath <- file.path(dirname(path), datasetFilename)
  if (!file.exists(datasetPath)) {
    stop(paste0("Dataset file '", datasetFilename, "' not found in directory: ", dirname(path)))
  }
  dataset <- read.csv(file=datasetPath)
  columnNames <- colnames(dataset)
  columnNamesLength <- columnNames %>% length()
  
  # Looking at INPUT block
  input <- extractNONMEMBlock(x=ctl, name="INPUT")
  inputs <- paste0(input@content, collapse=" ") %>%
    strsplit(inputs, split="\\s+")
  inputs <- inputs[[1]]
  options <- list()
  for (tmpInput in inputs) {
    if (grepl("=", tmpInput)) {
      parts <- strsplit(tmpInput, split="=")[[1]]
      key <- parts[1] %>% trimws()
      value <- parts[2] %>% trimws()
      options <- append(options, list(list(key=key, value=value)))
    } else {
      options <- append(options, list(list(key=tmpInput, value=NULL)))
    }
  }
  optionsLength <- options %>% length()
  
  if (optionsLength != columnNamesLength) {
    stop(paste0("INPUT has ", optionsLength, " entries while dataset has ", columnNamesLength, " columns."))
  }
  
  optionKeys <- options %>% purrr::map_chr(~.x$key)
  optionValues <- options %>% purrr::map_chr(~ifelse(is.null(.x$value), NA, .x$value))
  
  # Overwrite column headers with the keys
  colnames(dataset) <- optionKeys
  
  # DROP column indexes
  optionsToDrop <- options %>% purrr::keep(~(!is.null(.x$value) && .x$value == "DROP"))
  for (optionToDrop in optionsToDrop) {
    dataset <- dataset %>% dplyr::select(-dplyr::all_of(optionToDrop$key))
  }
  
  # Rename necessary columns
  optionsToRename <- options %>% purrr::keep(~(!is.null(.x$value) && .x$value != "DROP"))
  for (optionToRename in optionsToRename) {
    dataset <- dataset %>% dplyr::rename_at(.vars=optionToRename$key, .funs=~optionToRename$value)
  }
  
  # First standardise NONMEM dataset
  dataset <- standardiseNMDataset(dataset)
  
  # NONMEM important variables
  nmVariables <- c("ID","TIME","DV","MDV","EVID", "AMT", "CMT", "RATE")
  
  # Remove unnecessary columns
  if (!is.null(covariates)) {
    dataset <- dataset %>%
      dplyr::select(dplyr::all_of(c(nmVariables, covariates)))
  }
  
  # Standardise dataset
  dataset <- dataset %>%
    dplyr::relocate(dplyr::any_of(nmVariables))
  
  # Import ETAs if it was required (default is FALSE)
  if (etas) {
    table <- getRecordAt(pharmpy, name="TABLE", index=table_no)
    tabFilename <- table$path %>% as.character()
    dataset <- dataset %>% importETAs(file=paste0(campsistrans@dirname, "/", tabFilename),
                                      model=campsistrans@campsis)
  } else {
    # If etas_zero, all ETAs are added to dataset and set to 0
    if (etas_zero) {
      for (omega in campsistrans@campsis@parameters %>% campsismod::select("omega") %>% .@list) {
        if (campsismod::isDiag(omega)) {
          dataset[omega %>% campsismod::getNameInModel()] <- 0
        }
      }
    }
  }
  
  # Simulation ID column
  if (campsis_id) {
    dataset <- dataset %>% addSimulationIDColumn()
  }
  
  return(dataset)
}

#' Get indexed NONMEM record from the NONMEM control stream for the given section name.
#'
#' @param pharmpy pharmpy model
#' @param name NONMEM section name
#' @param index index of the record, default is 1
#' @param stop_if_not_found throw an error if no section was found
#' @return a record
#' 
getRecordAt <- function(pharmpy, name, index=1, stop_if_not_found=TRUE) {
  records <- pharmpy$internals$control_stream$get_records(name)
  if (records %>% length() == 0) {
    stop(paste0("No ", name, " section in control stream"))
  }
  record <- records[[index]]
  return(record)
}

#' Import ETA's.
#'
#' @param x imported NONMEM dataset
#' @param file TAB file with estimated ETA's
#' @param model CAMPSIS model
#' @param id mapping ID name
#' @return updated data frame
#' @importFrom dplyr filter filter_at group_by_at left_join pull rename_at row_number select_at ungroup
#' @importFrom purrr map_chr
#' @export
importETAs <- function(x, file, model, id="ID") {
  
  # ID name to map dataset with ETA's
  mappingIDName <- id
  
  # Import estimated subjects
  tab <- read.nonmem(file=file)[[1]] %>%
    dplyr::group_by_at(mappingIDName) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
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

#' Add simulation ID column (id's starting at 1 and consecutive). Original column ID
#' will be replaced by the new simulation ID column, after being renamed into
#' ORIGINAL_ID column.
#'
#' @param dataset NONMEM dataset
#' @param id current identifier column, default is 'ID'
#' @return updated data frame
#' @importFrom dplyr arrange group_by group_indices rename_at select
#' @export
addSimulationIDColumn <- function(dataset, id="ID") {
  if ("ID" %in% colnames(dataset) && id != "ID") {
    dataset <- dataset %>% dplyr::select(-ID)
  }
  # Current ID is renamed into ORIGINAL_ID
  if (!("ORIGINAL_ID" %in% colnames(dataset))) {
    dataset <- dataset %>% dplyr::rename_at(.vars=id, .funs=function(x){"ORIGINAL_ID"})
  }
  # Arrange rows by ORIGINAL_ID
  dataset <- dataset %>% dplyr::arrange(ORIGINAL_ID)
  # Add simulation ID column
  dataset <- dataset %>% tibble::add_column(ID=dataset %>% dplyr::group_by(ORIGINAL_ID) %>%
                                              dplyr::group_indices(), .before="ORIGINAL_ID")
  # Arrange rows by ID
  dataset <- dataset %>% dplyr::arrange(ID) 
  return(dataset)
}
