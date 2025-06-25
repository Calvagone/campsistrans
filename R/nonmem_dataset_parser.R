
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
#' @param etas import estimated ETAs that were output by $TABLE in control stream.
#' By default, ETAs are not imported. Please set it to TRUE to enable this feature.
#' @param etas_zero if previous argument is set to FALSE, etas_zero set to TRUE will
#' all ETAs from model to 0 (useful to simulate model without IIV)
#' @param table_no table number to look for the ETAs, if NULL, first table with ETAS(1:LAST) occurrence will be used
#' @param campsis Campsis model object, if provided, ETAs in the dataset will be renamed according to the Campsis model
#' @param campsis_id rework ID column for simulation with Campsis (ID must start at 1 and must be consecutive).
#' Default is FALSE. Please set it to TRUE if you wish a simulation ID. If TRUE, original ID column is
#' preserved in column 'ORIGINAL_ID'.
#' @return a data frame
#' @importFrom campsismod getNameInModel isDiag
#' @importFrom dplyr all_of relocate rename_at select
#' @importFrom purrr keep map_chr
#' @export
importDataset <- function(file, covariates=NULL, etas=FALSE, table_no=NULL, etas_zero=FALSE, campsis=NULL, campsis_id=FALSE) {
  
  # NONMEM directory
  nmDir <- dirname(file)
  
  # Read control stream
  ctlLines <- readLines(file) %>%
    removeNONMEMComments()
  ctl <- paste0(ctlLines, collapse="\n")

  # Looking at DATA block
  data <- extractNONMEMBlock(x=ctl, name="DATA")
  datasetFilename <- gsub(pattern="^(.*?\\.(CSV|csv)).*", replacement="\\1", x=data@content[1])
  datasetPath <- file.path(nmDir, datasetFilename)
  if (!file.exists(datasetPath)) {
    stop(paste0("Dataset file '", datasetFilename, "' not found in directory: ", nmDir))
  }
  dataset <- read.csv(file=datasetPath)
  columnNames <- colnames(dataset)
  columnNamesLength <- columnNames %>% length()
  
  # Looking at INPUT block
  input <- extractNONMEMBlock(x=ctl, name="INPUT")
  options <- extractOptions(input)
  optionsLength <- options %>% length()
  
  if (optionsLength != columnNamesLength) {
    stop(paste0("INPUT has ", optionsLength, " entries while dataset has ", columnNamesLength, " columns."))
  }
  
  # Overwrite column headers with the keys
  colnames(dataset) <- names(options)
  
  # DROP column indexes
  optionsToDrop <- options %>%
    purrr::keep(~(!is.na(.x) && .x == "DROP"))
  dataset <- dataset %>%
    dplyr::select(!dplyr::all_of(names(optionsToDrop)))
  
  # Rename necessary columns
  optionsToRename <- options %>%
    purrr::keep(~(!is.na(.x) && .x != "DROP"))
  dataset <- dataset %>%
    dplyr::rename_at(.vars=names(optionsToRename), .funs=~as.character(optionsToRename))

  # First standardise NONMEM dataset
  dataset <- standardiseNMDataset(dataset)
  
  # NONMEM important variables
  nmVariables <- c("ID","TIME","DV","MDV","EVID","AMT","CMT","RATE")
  
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
    tables <- extractNONMEMBlock(x=ctl, name="TABLE", first=FALSE)
    etaTable <- NULL
    if (is.null(table_no)) {
      for (table in tables) {
        options <- extractOptions(table)
        keys <- names(options)
        if (("ID" %in% keys) && ("ETAS(1:LAST)" %in% keys)) {
          etaTable <- table
          break
        }
      }
    } else {
      etaTable <- tables[[table_no]]
    }
    if (is.null(etaTable)) {
      stop("No appropriate table found in control stream with ETAS(1:LAST) option. Please provide argument 'table_no'.")
    }
    options <- extractOptions(etaTable)
    etaFileFilename <- options$FILE
    if (is.null(etaFileFilename)) {
      stop("No FILE option in TABLE section with ETAS(1:LAST) option.")
    }
    etaFilePath <- file.path(nmDir, etaFileFilename)
    if (!file.exists(etaFilePath)) {
      stop(paste0("File with ETAs '", etaFileFilename, "' not found in directory: ", nmDir))
    }

    dataset <- dataset %>%
      importETAs(file=etaFilePath, model=campsis)
  } else {
    # If etas_zero, all ETAs are added to dataset and set to 0
    if (etas_zero && !is.null(campsis)) {
      for (omega in campsis@parameters %>% campsismod::select("omega") %>% .@list) {
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

#' Extract options from NONMEM block.
#'
#' @param input input NONMEM block (i.e. INPUT, TABLE, etc.)
#' @return a list of options (key/value form, value=NA if no value is provided)
#' @export
#' 
extractOptions <- function(input) {
  inputs <- paste0(input@content, collapse=" ") %>%
    strsplit(split="\\s+")
  inputs <- inputs[[1]]
  options <- list()
  for (tmpInput in inputs) {
    if (grepl("=", tmpInput)) {
      parts <- strsplit(tmpInput, split="=")[[1]]
      key <- parts[1] %>% trimws()
      value <- parts[2] %>% trimws()
      options[[key]] <- value
    } else {
      options[[tmpInput]] <- NA
    }
  }
  return(options)
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
  
  # Detect ETAs
  tabNames <- colnames(tab)
  etaNames <- tabNames[grep("^(ET\\d+)|(ETA\\d+)$", tabNames)]
  tab <- tab %>%
    dplyr::select_at(c(mappingIDName, etaNames))
  
  # Standardise ETA names thanks to Campsis model
  if (!is.null(model)) {
    tab <- tab %>% dplyr::rename_at(.vars=etaNames, .funs=function(etaName) {
      etaNumber <- as.numeric(sub(pattern = "(ET|ETA)", replacement = "", etaName))
      retValue <- etaNumber %>% purrr::map_chr(.f = function(eta) {
        omega <- model@parameters %>% getByIndex(Omega(index=eta, index2=eta))
        paste0("ETA_", omega@name)
      })
      return(retValue)
    })
  }

  # Left join with dataset
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
    dataset <- dataset %>%
      dplyr::rename_at(.vars=id, .funs=function(x){"ORIGINAL_ID"})
  }
  # Arrange rows by ORIGINAL_ID
  dataset <- dataset %>%
    dplyr::arrange(ORIGINAL_ID)
  # Add simulation ID column
  dataset <- dataset %>%
    tibble::add_column(ID=dataset %>% dplyr::group_by(ORIGINAL_ID) %>%
                                              dplyr::group_indices(), .before="ORIGINAL_ID")
  # Arrange rows by ID
  dataset <- dataset %>% dplyr::arrange(ID) 
  return(dataset)
}
