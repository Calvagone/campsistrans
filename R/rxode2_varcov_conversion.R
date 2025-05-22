
rxode2ParameterToCampsisParameter <- function(x) {
  if (grepl(pattern="^theta[0-9]+$", x=x)) {
    index <- as.numeric(gsub(pattern="theta", replacement="", x=x))
    return(Theta(index=index))
  } else if (grepl(pattern="^eta[0-9]+$", x=x)) {
    index <- as.numeric(gsub(pattern="eta", replacement="", x=x))
    return(Omega(index=index, index2=index))
  } else if (grepl(pattern="^eps[0-9]+$", x=x)) {
    index <- as.numeric(gsub(pattern="eps", replacement="", x=x))
    return(Sigma(index=index, index2=index))
  } else if (grepl(pattern="^omega\\.[0-9]+\\.[0-9]+$", x=x)) {
    indexes <- gsub(pattern="omega\\.", replacement="", x=x)
    indexes <- strsplit(x=indexes, split="\\.")
    return(Omega(index=indexes[[1]][1], index2=indexes[[1]][2], type="covar"))
  } else if (grepl(pattern="^sigma\\.[0-9]+\\.[0-9]+$", x=x)) {
    indexes <- gsub(pattern="sigma\\.", replacement="", x=x)
    indexes <- strsplit(x=indexes, split="\\.")
    return(Sigma(index=indexes[[1]][1], index2=indexes[[1]][2], type="covar"))
  } else {
    stop(sprintf("Non standard parameter name '%s' detected in variance-covariance matrix", x))
  }
}

processRxode2Varcov <- function(model, varcov) {
  varcovNames <- colnames(varcov)
  
  updatedVarcovNames <- varcovNames %>% purrr::map_chr(.f=function(x) {
    parameter <- rxode2ParameterToCampsisParameter(x)
    parameter_ <- model@parameters %>%
      campsismod::getByIndex(parameter)
    
    if (length(parameter_)==0) {
      return("")
    } else {
      return(parameter_ %>% getName())
    }
  })
  
  indexes <- which(updatedVarcovNames=="")
  for (index in indexes) {
    row <- varcov[index,]
    column <- varcov[,index]
    if (any(c(row, column)!=0)) {
      warning(sprintf("Removing non-zero row/colum '%s' from variance-covariance matrix", varcovNames[index]))
    }
  }
  varcov <- varcov[-indexes, -indexes]
  row.names(varcov) <- updatedVarcovNames[-indexes]
  colnames(varcov) <- updatedVarcovNames[-indexes]
  
  # Deduce fixed parameters and remove zeroes from variance-covariance matrix
  fixIndexes <- NULL
  for (index in seq_len(nrow(varcov))) {
    row <- varcov[index,]
    column <- varcov[,index]
    if (all(c(row, column)==0)) {
      fixIndexes <- c(fixIndexes, index)
    }
  }
  if (length(fixIndexes) > 0) {
    fixNames <- row.names(varcov)[fixIndexes]
    model@parameters@list <- model@parameters@list %>%
      purrr::map(.f=function(parameter) {
        if (parameter %>% getName() %in% fixNames) {
          parameter@fix <- TRUE
        }
        return(parameter)
      })
    varcov <- varcov[-fixIndexes, -fixIndexes]
  }
  model@parameters@varcov <- varcov
  return(model)
}
