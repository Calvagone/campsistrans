
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
  
  rxmod <- monolix2rx(mlxtran)
  
  model <- importRxode2(rxmod, rem_pop_suffix=TRUE, rem_omega_prefix=TRUE)
  
  return(model)
}