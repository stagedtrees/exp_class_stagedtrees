library(bnlearn)

predict_bn <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- t(attr(predict(model, node = "answer", data = train, prob = TRUE), "prob"))
    attr(prob, "dimnames") <- NULL
    cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob[, 2], optimiseFor = "Both")
    prob <- t(attr(predict(model, node = "answer", data = validation, prob = TRUE), "prob"))
    attr(prob, "dimnames") <- NULL
    pred_m12 <- factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                              levels(train$answer)[1]), levels = levels(train$answer))
    
  }else{
    
  }
  
}

bn_tabu <- function(train, test, optimizzcutoff = FALSE){
  bn <- bnlearn::tabu(train)
  bn.fit <- bn.fit(bn, train)
  predict_bn(bn.fit, train, test, optimizecutoff)
}