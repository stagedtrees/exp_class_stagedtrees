library(ada)

predict_adaboost <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train, type = "probs")
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob[, 2], 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test, type = "probs")
    factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
  }else{
  predict(model, newdata = test, type = "vector")
  }
}


adaboost_basic <- function(train, test, optimizecutoff = FALSE, ...) {
  model <- ada::ada(answer ~ ., data = train, iter = 100)
  predict_adaboost(model, train, test, optimizecutoff)
}
