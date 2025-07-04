
#' Get the path to a NONMEM control stream template, embedded in this package.
#' This template file will be created in a temporary folder.
#' A CSV dataset will be created as well (needed for pharmpy).
#' 
#' @param advan ADVAN number
#' @param trans TRANS number
#' @return the path to this template
#' @export
getNONMEMModelTemplate <- function(advan, trans) {
  dir <- tempdir()
  file <- tempfile(pattern = "template", tmpdir=dir, fileext=".mod")
  csvFile <- file.path(dir, "dataset.csv")
  file.create(csvFile)
  modelContent <- campsistrans::model_library[[paste0("advan", advan, "_trans", trans)]]
  modelContent <- gsub("\r\n", "\n", modelContent)
  
  # Add 2 lines programmatically
  modelContent <- paste0(modelContent, "$SIMULATION (1234) ONLYSIM NSUB=1\n")
  modelContent <- paste0(modelContent, "$TABLE ID TIME EVID MDV DV AMT CMT CP FILE=output.tab ONEHEADER NOAPPEND NOPRINT\n")

  # Write model
  fileConn <- file(file)
  writeLines(text=modelContent, fileConn)
  close(fileConn)
  
  # Add real CSV data to make pharmpy happy
  csvContent <- "ID,TIME,MDV,DV,AMT,RATE,CMT\n1,0,1,.,1000,0,1"
  fileConn <- file(csvFile)
  writeLines(text=csvContent, fileConn)
  close(fileConn)
  
  return(file)
}
