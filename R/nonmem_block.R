#_______________________________________________________________________________
#----                           nonmem_block class                          ----
#_______________________________________________________________________________

setClass(
  "nonmem_block",
  representation(
    name = "character",
    startIndex = "integer",
    endIndex = "integer",
    content = "character",
    content_on_first_line = "logical"
  )
)

#' Extract NONMEM block.
#' 
#' @param x NONMEM control stream as a character string
#' @param name name of the NONMEM block to extract
#' @param first if TRUE, return only the first block found, otherwise return all blocks
#' @param raise_error if TRUE, raise an error if no block is found, otherwise return an empty list
#' @return a list of nonmem_block objects, or a single nonmem_block object if first=TRUE
#' @export
extractNONMEMBlock <- function(x, name, first=TRUE, raise_error=TRUE) {
  lines <- strsplit(x, split="\n")[[1]]
  blockIndexes <- grepl(pattern="^\\s*\\$([A-Z]+)", x=lines)
  
  blockIndexesStr <- lines[blockIndexes]
  blockIndexes <- which(blockIndexes)
  
  blockRegex <- getNONMEMBlockRegex(name=name)
  specificBlockIndexes <- grepl(pattern=sprintf("^\\s*\\$(%s)", blockRegex), x=blockIndexesStr) %>%
    which()
  
  retValue <- list()
  for (specificBlockIndex in specificBlockIndexes) {
    startIndex <- blockIndexes[specificBlockIndex]
    endIndex <- length(lines)
    if (length(blockIndexes) >= specificBlockIndex + 1) {
      endIndex <- blockIndexes[specificBlockIndex + 1]-1
    }
    content <- lines[startIndex:endIndex]
    firstLine <- gsub(pattern="^\\s*\\$([A-Z]+)", replacement="", x=content[1]) %>%
      trimws()
    if (firstLine == "") {
      content <- content[-1] # Remove the first line if it is empty
      content_on_first_line <- FALSE
    } else {
      content[1] <- firstLine # Remove the $NAME from the first line
      content_on_first_line <- TRUE
    }
    
    # Trim white spaces
    content <- content %>%
      trimws()
    
    # Remove empty lines
    content <- content[content != ""] # Remove empty lines
    
    retValue <- retValue %>%
      append(new("nonmem_block", name=name,
                 startIndex=as.integer(startIndex),
                 endIndex=as.integer(endIndex),
                 content=content,
                 content_on_first_line=content_on_first_line))
  }
  if (length(retValue)==0) {
    if (raise_error) {
      stop(sprintf("No NONMEM block '%s' found", name))
    } else {
      return(retValue)
    }
  }
  if (first) {
    retValue <- retValue[[1]]
  }
  
  return(retValue)
}

#' Remove NONMEM block.
#' 
#' @param x NONMEM control stream as a character string
#' @param name name of the NONMEM block to extract
#' @param first if TRUE, only the first block will be removed, otherwise all blocks will be removed
#' @return control stream as a character string
#' @export
removeNONMEMBlock <- function(x, name, first=TRUE) {
  blocks <- extractNONMEMBlock(x=x, name=name, first=first, raise_error=FALSE)
  lines <- strsplit(x, split="\n")[[1]]
  for (block in rev(blocks)) {
    startIndex <- block@startIndex
    endIndex <- block@endIndex
    lines <- lines[-(startIndex:endIndex)]
  }
  retValue <- paste0(lines, collapse="\n")
  return(retValue)
}

#' Replace NONMEM block.
#' 
#' @param x NONMEM control stream as a character string
#' @param name name of the NONMEM block to replace
#' @param content updated content, character vector
#' @return control stream as a character string
#' @export
replaceNONMEMBlock <- function(x, name, content) {
  block <- extractNONMEMBlock(x=x, name=name, first=TRUE)
  lines <- strsplit(x, split="\n")[[1]]
  startIndex <- block@startIndex
  endIndex <- block@endIndex
  
  # Remove the block
  lines <- lines[-(startIndex:endIndex)]
  
  # Append new content
  if (block@content_on_first_line) {
    content[1] <- paste0("$", name, " ", content[1])
  } else {
    content <- c(paste0("$", name), content)
  }
  lines <- lines %>% append(content, after=startIndex-1)
  
  retValue <- paste0(lines, collapse="\n")
  return(retValue)
}

getNONMEMBlockRegex <- function(name) {
  if (name=="SIZES") {
    retValue <- "SIZES | SIZE"
    
  } else if (name=="PROBLEM") {
    retValue <- "PROBLEM | PROB"
    
  } else if (name=="INPUT") {
    retValue <- "INPUT"
    
  } else if (name=="DATA") {
    retValue <- "DATA"
    
  } else if (name=="SUBROUTINES") {
    retValue <- "SUBROUTINES | SUBROUTINE | SUBR"
    
  } else if (name=="MODEL") {
    retValue <- "MODEL"
    
  } else if (name=="ABBREVIATED") {
    retValue <- "ABBREVIATED | ABBR"
    
  } else if (name=="PK") {
    retValue <- "PK"
    
  } else if (name=="PRED") {
    retValue <- "PRED"
    
  } else if (name=="DES") {
    retValue <- "DES"
    
  } else if (name=="ERROR") {
    retValue <- "ERROR"
    
  } else if (name=="THETA") {
    retValue <- "THETA"
    
  } else if (name=="OMEGA") {
    retValue <- "OMEGA"
    
  } else if (name=="ESTIMATION") {
    retValue <- "ESTIMATION | ESTIMATE | EST"
  
  } else if (name=="COVARIANCE") {
    retValue <- "COVARIANCE | COV"
    
  } else if (name=="TABLE") {
    retValue <- "TABLE | TAB"
    
  } else {
    stop(sprintf("Unknown NONMEM block name '%s'", name))
  }
  return(retValue)
}
