library(ipred)

predict_bagging <- function(model, train, test, optimizecutoff){
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


bagging_basic <- function(train, test, optimizecutoff = FALSE, ...) {
  model <- ipred::bagging(answer ~ ., data = train)
  predict_bagging(model, train, test, optimizecutoff)
}
