
#' Import NONMEM predictions.
#'
#' @param file path to NONMEM control stream (dataset path will be automatically extracted)
#' @param output output(s) of interest, character vector
#' @param table_no table number to look for the predictions, if NULL, first table 
#' containing ID, TIME and the output(s) of interest will be used
#' @param obs_only if TRUE, only observations (MDV=0) will be returned, default is TRUE
#' @return a data frame with ID and the output(s) of interest
#' @importFrom dplyr select filter all_of rename
#' @export
importPredictions <- function(file, output, table_no=NULL, obs_only=TRUE) {
  
  # NONMEM directory
  nmDir <- dirname(file)
  
  # Read control stream
  ctlLines <- readLines(file) %>%
    removeNONMEMComments()
  ctl <- paste0(ctlLines, collapse="\n")
  
  # Required variables
  requiredVariables <- c("ID", "TIME", output)
  
  # Looking at the TABLE block(s)
  tables <- extractNONMEMBlock(x=ctl, name="TABLE", first=FALSE)
  table <- NULL
  if (is.null(table_no)) {
    for (tmpTable in tables) {
      options <- extractOptions(tmpTable)
      keys <- names(options)
      if (all(requiredVariables %in% keys)) {
        table <- tmpTable
        break
      }
    }
  } else {
    table <- tables[[table_no]]
  }
  if (is.null(table)) {
    stop("No suitable TABLE block found in the control stream.")
  }
  
  options <- extractOptions(table)
  tableFilename <- options$FILE
  if (is.null(tableFilename)) {
    stop("No FILE option in TABLE section.")
  }
  tablePath <- file.path(nmDir, tableFilename)
  if (!file.exists(tablePath)) {
    stop(paste0("Table with predictions '", tableFilename, "' not found in directory: ", nmDir))
  }
  # Read predictions table
  predictions <- read.nonmem(file=tablePath)[[1]]

  # Argument obs_only
  if (obs_only) {
    if ("MDV" %in% colnames(predictions)) {
      predictions <- predictions %>%
        dplyr::filter(MDV == 0)
    } else if ("EVID" %in% colnames(predictions)) {
      predictions <- predictions %>%
        dplyr::filter(EVID == 0)
    } else {
      stop("Neither MDV nor EVID columns found in the predictions table. Cannot filter observations.")
    }
  }
  
  # Select only required variables
  predictions <- predictions %>%
    dplyr::select(dplyr::all_of(requiredVariables))
  
  return(predictions)
}
