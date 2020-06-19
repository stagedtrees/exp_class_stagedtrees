library(stagedtrees)
library(infotheo)


ordering_cmi <- function(train){
  
}

## normalized mi
ordering_mi <- function(train){
  ms <- sapply(names(train)[-1], function(v){
    infotheo::mutinformation(X = train$answer, Y = train[[v]]) / 
      infotheo::entropy(train[[v]])
  })
  names(train)[c(1, 1 + order(ms, decreasing = TRUE))]
}

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
  model <- stagedtrees::full(train, lambda = 1, join_zero = TRUE, order = ordering_mi(train))
  predict_st(model, train, test, optimizecutoff)
}

st_indep <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::indep(train, lambda = 1, order = ordering_mi(train))
  predict_st(model, train, test, optimizecutoff)
}

st_hc_indep <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(stagedtrees::indep(train, lambda = 1,
                                                     order = ordering_mi(train)), name = "NA")
  model <- stagedtrees::hc.sevt(model)
  predict_st(model, train, test, optimizecutoff)
  
}

st_hc_full <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::hc.sevt(full(train, join_zero = TRUE, lambda = 1, 
                                     order = ordering_mi(train)))
  predict_st(model, train, test, optimizecutoff)
}

st_fbhc_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::fbhc.sevt(full(train, join_zero = TRUE, lambda = 1, 
                                       order = ordering_mi(train)))
  predict_st(model, train, test, optimizecutoff)
}

st_fbhc <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::fbhc.sevt(full(train, join_zero = TRUE, lambda = 1))
  predict_st(model, train, test, optimizecutoff)
}

st_bhc_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::bhc.sevt(full(train, join_zero = TRUE, lambda = 1, 
                                      order = ordering_mi(train)))
  predict_st(model, train, test, optimizecutoff)
}

st_bj_kl_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::bj.sevt(full(train, join_zero = TRUE, lambda = 1, 
                                     order = ordering_mi(train)), distance = kl, thr = 0.2)
  predict_st(model, train, test, optimizecutoff)
}

st_bj_tv_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::bj.sevt(full(train, join_zero = TRUE, lambda = 1, 
                                     order = ordering_mi(train)), distance = tv, thr = 0.2)
  predict_st(model, train, test, optimizecutoff)
}

st_bj_cd_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::bj.sevt(full(train, join_zero = TRUE, lambda = 1, 
                                     order = ordering_mi(train)), distance = cd, thr = 0.2)
  predict_st(model, train, test, optimizecutoff)
}

st_naive_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::naive.sevt(full(train, join_zero = TRUE, lambda = 1, 
                                        order = ordering_mi(train)),
                                   distance = kl)
  predict_st(model, train, test, optimizecutoff)
}


st_naive <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::naive.sevt(full(train, join_zero = TRUE, lambda = 1),
                                   distance = kl)
  predict_st(model, train, test, optimizecutoff)
}