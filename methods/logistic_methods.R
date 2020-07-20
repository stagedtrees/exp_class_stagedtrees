library(nnet)

predict_logistic <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train, type = "probs")
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob, 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test, type = "probs")
    pred <- factor(ifelse(prob >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
  }
  else{
    cutoff <- 0.5
    prob <- predict(model, newdata = test, type = "probs")
    pred <- factor(ifelse(prob >= cutoff, levels(train$answer)[2], 
                          levels(train$answer)[1]), levels = levels(train$answer))
  }
  return(list(pred = pred, prob = prob, cutoff = cutoff))
}

logistic_basic <- function(train, test, optimizecutoff = FALSE, ...) {
  model <- multinom(answer ~ ., data = train, trace = FALSE)
  predict_logistic(model, train, test, optimizecutoff)
}

