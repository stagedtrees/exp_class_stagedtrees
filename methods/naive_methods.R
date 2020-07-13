library(e1071)

predict_naive <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train, type = "raw")
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob[, 2], 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test, type = "raw")
    factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
  }else{
    predict(model, newdata = test, type = "class")
  }
}


naive_bayes_1 <- function(train, test, optimizecutoff = FALSE, ...) {
  model <- e1071::naiveBayes(answer ~ ., data = train)
  predict_naive(model, train, test, optimizecutoff)
}



