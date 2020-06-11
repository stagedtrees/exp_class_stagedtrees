library(bnlearn)

predict_bn <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- t(attr(predict(model, node = "answer", data = train, prob = TRUE), "prob"))
    attr(prob, "dimnames") <- NULL
    cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob[, 2], optimiseFor = "Both")
    prob <- t(attr(predict(model, node = "answer", data = test, prob = TRUE), "prob"))
    attr(prob, "dimnames") <- NULL
    factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                              levels(train$answer)[1]), levels = levels(train$answer))
    
  }else{
    prob <- t(attr(predict(model, node = "answer", data = test, prob = TRUE), "prob"))
    attr(prob, "dimnames") <- NULL
    factor(ifelse(prob[, 2] >= 0.5, levels(train$answer)[2], 
                              levels(train$answer)[1]), levels = levels(train$answer))
  }
  
}

bn_tabu <- function(train, test, optimizecutoff = FALSE){
  bn <- bnlearn::tabu(train)
  bn.fit <- bn.fit(bn, train)
  predict_bn(bn.fit, train, test, optimizecutoff)
}

