library(stagedtrees)

predict_st <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train, class = "answer", prob = TRUE )
    
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob, 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test, class = "answer", prob = TRUE )
    factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
  }else{
    predict(model, newdata = test, class = "answer")
  }
}

st_full <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::full(train, lambda = 1)
  predict_st(model, train, test, optimizecutoff)
}

st_indep <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::indep(train, lambda = 1)
  predict_st(model, train, test, optimizecutoff)
}

st_hc_indep <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::hc.sevt(stagedtrees::indep(train, lambda = 1))
  predict_st(model, train, test, optimizecutoff)
  
}

st_hc_full <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::hc.sevt(stagedtrees::full(train, lambda = 1))
  predict_st(model, train, test, optimizecutoff)
}

st_fbhc <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::fbhc.sevt(stagedtrees::full(train, lambda = 1))
  predict_st(model, train, test, optimizecutoff)
}


st_bj_kl <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::bj.sevt(stagedtrees::full(train, lambda = 1), distance = kl, thr = 0.2)
  predict_st(model, train, test, optimizecutoff)
}

st_bj_tv <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::bj.sevt(stagedtrees::full(train, lambda = 1), distance = tv, thr = 0.2)
  predict_st(model, train, test, optimizecutoff)
}

st_bj_cd <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::bj.sevt(stagedtrees::full(train, lambda = 1), distance = cd, thr = 0.2)
  predict_st(model, train, test, optimizecutoff)
}

st_naive <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::naive.sevt(stagedtrees::full(train, lambda = 1), distance = kl)
  predict_st(model, train, test, optimizecutoff)
}
