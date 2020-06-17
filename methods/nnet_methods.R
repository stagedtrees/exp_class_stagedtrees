library(nnet)

predict_nnet <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train, type = "raw" )
    
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob, 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test, type = "raw")
    factor(ifelse(prob >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
  }else{
    predict(model, newdata = test, type = "class")
  }
}


nnet_basic <- function(train, test, optimizecutoff = FALSE, ...){
  model <- nnet(answer ~ ., data = train, size = 20, 
       decay = 0.01, maxit = 5000, linout = FALSE, MaxNWts = 5000, trace = FALSE)
  predict_nnet(model, train, test, optimizecutoff)
}
