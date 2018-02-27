train_and_test <- function(dataset){
  set.seed(3500)
  
  labeled_set <- dataset %>% group_by(app_id) %>% do(mutate(., label = sample(c("train", "test"), nrow(.), replace = TRUE, prob = c(0.8,0.2))))
  
  train <- labeled_set[labeled_set$label=="train",1:5]
  test <- labeled_set[labeled_set$label=="test",1:5]
  
  list(train, test)
}