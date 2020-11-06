library(bnlearn)

predict_bn <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- t(attr(predict(model, node = "answer", data = train, prob = TRUE), "prob"))
    attr(prob, "dimnames") <- NULL
    cutoff <- InformationValue::optimalCutoff(as.numeric(train$answer) - 1, prob[, 2], optimiseFor = "Both")
    prob <- t(attr(predict(model, node = "answer", data = test, prob = TRUE), "prob"))
    attr(prob, "dimnames") <- NULL
    pred <- factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                              levels(train$answer)[1]), levels = levels(train$answer))
    prob <- prob[, 2]
  }
  else{
    cutoff <- 0.5
    prob <- t(attr(predict(model, node = "answer", data = test, prob = TRUE), "prob"))
    attr(prob, "dimnames") <- NULL
    pred <- factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
    prob <- prob[, 2]
  }
  return(list(pred = pred, prob = prob, cutoff = cutoff))
}

bn_tabu <- function(train, test, optimizecutoff = FALSE){
  set.seed(2020)
  bn <- bnlearn::tabu(train)
  bn.fit <- bn.fit(bn, train)
  predict_bn(bn.fit, train, test, optimizecutoff)
}

bn_hc <- function(train, test, optimizecutoff = FALSE) {
  set.seed(2020)
  bn <- bnlearn::hc(train)
  bn.fit <- bn.fit(bn, train)
  predict_bn(bn.fit, train, test, optimizecutoff)
}



