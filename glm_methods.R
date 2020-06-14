
predict_glm <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train, type = "response" )
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob, 
                                              optimiseFor = "Both")
  }
  else{
    cutoff <- 0.5
  }
  prob <- predict(model, newdata = test, type = "response")
  factor(ifelse(prob >= cutoff, levels(train$answer)[2], 
                levels(train$answer)[1]), levels = levels(train$answer))
}


glm_binomial <- function(train, test, optimizecutoff = FALSE){
  model <- glm(answer ~ ., data = train, family = binomial)
  predict_glm(model, train, test, optimizecutoff)
}