# Author: Vladimir Dobrodeev
# The script performs training

library("FNN")

field.set.1 <- c("traffic_volume")
field.set.2 <- c("traffic_volume", "base_station")
field.set.3 <- c("traffic_volume", "user_id")
field.set.4 <- c("traffic_volume", "user_id", "base_station")
field.set.5 <- c("traffic_volume", "base_station", "time")
field.set.6 <- c("traffic_volume", "user_id", "time")

test_model <- function(train, test, fields){
  calculated_labels <- knn(train = train[fields], 
                           test = test[fields],
                           cl = train$app_id,
                           k = 35,
                           prob = TRUE)
  return(calculated_labels)
}