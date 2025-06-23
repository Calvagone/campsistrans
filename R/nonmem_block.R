#_______________________________________________________________________________
#----                           nonmem_block class                          ----
#_______________________________________________________________________________

setClass(
  "nonmem_block",
  representation(
    name = "character",
    startIndex = "integer",
    endIndex = "integer",
    content = "character"
  )
)

#' Extract NONMEM block.
#' 
#' @param x NONMEM control stream as a character string
#' @param name name of the NONMEM block to extract
#' @param first if TRUE, return only the first block found, otherwise return all blocks
#' @return a list of nonmem_block objects, or a single nonmem_block object if first=TRUE
#' @export
extractNONMEMBlock <- function(x, name, first=TRUE) {
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
    content[1] <- gsub(pattern="^\\s*\\$([A-Z]+)", replacement="", x=content[1])
    
    # Trim white spaces
    content <- content %>%
      trimws()
    
    # Remove empty lines
    content <- content[content != ""] # Remove empty lines
    
    retValue <- retValue %>%
      append(new("nonmem_block", name=name,
                 startIndex=as.integer(startIndex),
                 endIndex=as.integer(endIndex),
                 content=content))
  }
  if (length(list)==0) {
    stop(sprintf("No NONMEM block '%s' found", name))
  }
  if (first) {
    retValue <- retValue[[1]]
  }
  
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
    what <- "ESTIMATION | ESTIMATE | EST"
    
  } else if (name=="TABLE") {
    retValue <- "TABLE | TAB"
    
  } else {
    stop(sprintf("Unknown NONMEM block name '%s'", name))
  }
  return(retValue)
}
