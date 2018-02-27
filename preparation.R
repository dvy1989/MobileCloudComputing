# Author: Vladimir Dobrodeev
# Script, preparing dataset for further processing
# Here script removes from dataset all application with less, than 2 occurences

library("dplyr")

prepare_dataset <- function(dataset){
  dataset <- dataset %>% group_by(app_id) %>% filter(n() >= 50)
  dataset <- setDT(dataset)
  # Has no affect for calculation
  # Done for simplicity
  dataset <- dataset[, user_id:=as.numeric(user_id)]
  dataset <- dataset[, base_station:=as.numeric(base_station)]
  return(dataset)
}