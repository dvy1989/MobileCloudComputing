source("reading.R")
source("preparation.R")
source("testtrain.R")
source("modelling.R")
source("validation.R")

library("dplyr")

start <- Sys.time()

# READING DATA
dataset <- read_dataset("0421.txt")

# PREPARING DATA
prepared_set <- prepare_dataset(dataset)

# MAKING TRAIN AND TEST SETS
train_and_test_sets <- train_and_test(prepared_set)
train <- train_and_test_sets[[1]]
test <- train_and_test_sets[[2]]

# TESTING MODEL
calculated_labels <- test_model(train, test, field.set.2)

#VALIDATION
print(validate_model(calculated_labels, test$app_id))

finish <- Sys.time()

print(finish - start)