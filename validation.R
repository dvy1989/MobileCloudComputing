validate_model <- function(calculated_labels, real_labels){
  matches <- length(which(calculated_labels == real_labels))
  return (matches/length(calculated_labels))
}