library(nnet)

predict_nnet <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train, type = "raw")
    
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob, 
                                              optimiseFor = "Both")
    prob <- c(predict(model, newdata = test, type = "raw"))
    pred <- factor(ifelse(prob >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
  }
  else{
    cutoff <- 0.5
    prob <- c(predict(model, newdata = test, type = "raw"))
    pred <- factor(ifelse(prob >= cutoff, levels(train$answer)[2], 
                          levels(train$answer)[1]), levels = levels(train$answer))
  }
  return(list(pred = pred, prob = prob, cutoff = cutoff))
}


nnet_1 <- function(train, test, optimizecutoff = FALSE, ...){
  model <- nnet(answer ~ ., data = train, size = 20, 
       decay = 0.01, maxit = 5000, linout = FALSE, MaxNWts = 5000, trace = FALSE)
  predict_nnet(model, train, test, optimizecutoff)
}

nnet_2 <- function(train, test, optimizecutoff = FALSE, ...){
  model <- nnet(answer ~ ., data = train, size = 10, 
                decay = 0.001, maxit = 5000, linout = FALSE, MaxNWts = 5000, trace = FALSE)
  predict_nnet(model, train, test, optimizecutoff)
}




