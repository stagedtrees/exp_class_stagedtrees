library(rpart)

predict_tree <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train, type = "prob")
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob[, 2], 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test, type = "prob")
    factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
  }else{
    predict(model, newdata = test, type = "class")
  }
}


cl_tree_1 <- function(train, test, optimizecutoff = FALSE, ...) {
  model <- rpart(answer ~ ., data = train, method = "class", cp = 0)
  predict_tree(model, train, test, optimizecutoff)
}

cl_tree_2 <- function(train, test, optimizecutoff = FALSE, ...) {
  model <- rpart(answer ~ ., data = train, method = "class", cp = 0.05)
  predict_tree(model, train, test, optimizecutoff)
}
