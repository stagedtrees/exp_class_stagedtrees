library(klaR)

predict_naive_klaR <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train)$posterior
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob[, 2], 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test)$posterior
    factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
  }else{
    predict(model, newdata = train)$class
  }
}


naive_bayes_2 <- function(train, test, optimizecutoff = FALSE, ...) {
  model <- klaR::NaiveBayes(answer ~ ., data = train)
  predict_naive_klaR(model, train, test, optimizecutoff)
}
