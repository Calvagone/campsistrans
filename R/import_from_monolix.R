
#' Extract the model from Monolix.
#' 
#' @param path to mlxtran file
#' @return a functional Campsis model
#' @export
#' @importFrom monolix2rx mlxtran monolix2rx
#' 
importMonolix <- function(file) {
  mlxtran <- monolix2rx::mlxtran(file=file)
  mlxtran$MODEL$LONGITUDINAL$LONGITUDINAL$file <- basename(file)
  
  rxModel <- monolix2rx(mlxtran)
  
  return(importRxode2(rxModel))
}