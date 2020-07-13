library(klaR)

predict_rda <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train)$posterior
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob[, 2], 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test)$posterior
    factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
  }else{
    predict(model, newdata = test)$class
  }
}


regularized_da <- function(train, test, optimizecutoff = FALSE, ...) {
  model <- rda(answer ~ ., data = train)
  predict_rda(model, train, test, optimizecutoff)
}
