
predict_glm <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train, type = "response")
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob, 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test, type = "response")
    pred <- factor(ifelse(prob >= cutoff, levels(train$answer)[2], 
                          levels(train$answer)[1]), levels = levels(train$answer))
  }
  else{
    cutoff <- 0.5
    prob <- predict(model, newdata = test, type = "response")
    pred <- factor(ifelse(prob >= cutoff, levels(train$answer)[2], 
                          levels(train$answer)[1]), levels = levels(train$answer))
  }
  return(list(pred = pred, prob = prob, cutoff = cutoff))
}


glm_binomial <- function(train, test, optimizecutoff = FALSE){
  suppressWarnings(model <- glm(answer ~ ., data = train, family = binomial))
  suppressWarnings(predict_glm(model, train, test, optimizecutoff))
}
