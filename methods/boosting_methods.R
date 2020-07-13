library(adabag)

predict_boosting <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train)$prob
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob[, 2], 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test)$prob
    factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
  }else{
    factor(predict(model, newdata = test)$class, levels = levels(train$answer))
  }
}


boosting_basic <- function(train, test, optimizecutoff = FALSE, ...) {
  model <- boosting(answer ~ ., data = train, mfinal = 20)
  predict_boosting(model, train, test, optimizecutoff)
}
