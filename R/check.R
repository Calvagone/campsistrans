get_slot_length <- function(object, slot) {
  return(length(eval(parse(text = paste0("object@", slot)))))
}

check_length <- function(object, slot, expected = 1) {
  lengthSlot <- get_slot_length(object, slot)
  error <- character()
  if (lengthSlot != expected) {
    error <- paste0(slot, " is length ", lengthSlot, ". Should be ", expected, ".")
  }
  return(error)
}

expect_one <- function(object, slot) {
  return(check_length(object, slot, expected = 1))
}