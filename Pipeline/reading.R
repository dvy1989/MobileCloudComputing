# Author: Vladimir Dobrodeev
# Script for reading dataset from file

library("data.table")

read_dataset <- function(fileName){
  dataset <- fread(fileName, sep = " ", stringsAsFactors = FALSE, 
                   col.names = c("user_id", "time", "base_station", "traffic_volume", "app_id"))
  dataset<-dataset[, traffic_volume:=as.numeric(traffic_volume)]
  # Removing tabs
  dataset<-dataset[, user_id:=gsub("\t", "", user_id)]      
  dataset<-dataset[, app_id:=gsub("\t", "", app_id)]
  return(dataset)
}