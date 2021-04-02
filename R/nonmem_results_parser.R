
#' Read all sorts of NONMEM tables (extensions ext, tab, cov, etc, ...).
#'
#' @param file the file to be read
#' @param varvov must be set to TRUE for .cov files, default is FALSE. It simply indicates
#' the first column is not numeric.
#' @return a list of data frame (one dataframe per table)
#' @export
read.nonmem <- function(file, varcov=FALSE) {
  
  # Read all lines from NONMEM results file (*.ext, *.tab, *.cov, etc, ...)
  fileConn <- file(file)
  allLines <- readLines(con=fileConn)
  close(fileConn)
  
  # Retrieve all table indexes
  allTableIndexes <- grep("^TABLE NO\\. .*$", x=allLines, ignore.case=T)
  
  if (length(allTableIndexes)==0) {
    stop("No NONMEM table was recognised")
  }
  
  # Extract tables
  tables <- purrr::map2(.x=allTableIndexes, .y=c(allTableIndexes[-1], length(allLines)+1),
                        .f=~read.nonmemtable(allLines[seq(.x, .y-1, by=1)], varcov=varcov))
  return(tables)
}

#' Read a NONMEM table.
#'
#' @param content table content
#' @param varvov must be set to TRUE for .cov files, default is FALSE. It simply indicates
#' the first column is not numeric.
#' @return a dataframe
#' @importFrom purrr map_df
read.nonmemtable <- function(content, varcov=FALSE) {
  
  # Skip first line
  content <- content[-1]
  
  # Retrieve headers
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  headers <- trim(content[1])
  headers <- gsub("\\s+", " ", headers)
  headers <- strsplit(headers, " ")[[1]]
  content <- content[-1]
  
  # Extract values
  data <- content %>% purrr::map_df(.f=function(.x) {
    .x <- trim(.x)
    .x <- gsub("\\s+", " ", .x)
    .x <- strsplit(.x, " ")[[1]]
    names(.x) <- headers
    return(.x)
  })
  
  # If cov file, first column is NAME, as.numeric() must not be called
  if (varcov) {
    headers <- headers[-1]
  }
  data <- data %>% dplyr::mutate_at(.vars=headers, .funs=as.numeric)
  
  return(data)
}

#' Read NONMEM variance-covariance matrix files (extension .cov).
#'
#' @param file the cov file to be read
#' @return the raw variance covariance matrix, matrix
#' @importFrom assertthat assert_that
#' @export
read.varcov <- function(file) {
  varcov <- read.nonmem(file=file, varcov=TRUE)
  assertthat::assert_that(length(varcov)==1, msg=paste0("There must be exactly 1 table in file ", file))
  
  # Take first table
  varcov <- varcov[[1]]
  
  # Check first column in data frame
  assertthat::assert_that(colnames(varcov)[1]=="NAME", msg="First column must be 'NAME'")
  
  # Then we can remove the NAME column from the parameter names
  parameterNames <- colnames(varcov)[-1]
  
  # Check names in 'NAME' are the same as column names
  assertthat::assert_that(all(varcov$NAME==parameterNames),
                          msg="Inconsistent variance-covariance matrix, row names are different than column names")
  
  # Then we can tranform the data frame to a matrix
  varcov <- varcov %>% dplyr::select(-NAME) %>% as.matrix()
  row.names(varcov) <- parameterNames
  
  return(varcov)
}

