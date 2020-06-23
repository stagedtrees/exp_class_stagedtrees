library(randomForest)


predict_rf <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train, type = "prob")
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob[, 2], 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test, type = "prob")
    factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
  }else{
    predict(model, newdata = test, type = "response")
  }
}


rf_basic <- function(train, test, optimizecutoff = FALSE){
  model <- randomForest(answer ~ ., data = train, ntree = 100)
  predict_rf(model, train, test, optimizecutoff)
}
