library(stagedtrees)
library(infotheo)

conditional_information <- function (X, Y, S = NULL, method = "emp") 
{
  if (is.null(S)) {
    Ires <- mutinformation(X, Y, method)
  }
  else {
    U <- data.frame(S, X, Y)
    Hysx <- entropy(U, method)
    Hsx <- entropy(U[, c(1:NCOL(S), NCOL(S)+1)], method)
    Hys <- entropy(U[, c(1:NCOL(S), NCOL(U))], method)
    Hs <- entropy(S, method)
    Ires <- Hys - Hs - Hysx + Hsx
  }
  if(Ires < 0 & Ires > -0.001) {
    Ires <- 0
  }
  Ires
}


## maximising conditional mutual information with class
ordering_cmi <- function(train){
  n <- ncol(train) - 1
  free <- names(train)[-1]
  ms <- sapply(free, function(v){
    infotheo::mutinformation(X = train$answer, Y = train[[v]]) /  infotheo::entropy(train[[v]])
  })
  selected <- names(which.max(ms))
  free <- free[-which.max(ms)]
  for (i in 2:n){
    ms <- sapply(free, function(v){
      conditional_information(X = train$answer, 
                              Y = train[[v]],
                              S = train[, selected]) /  infotheo::entropy(train[[v]])
    })
    selected <- c(selected, names(which.max(ms)))
    free <- free[-which.max(ms)]
  }
  return(c("answer", selected))
}


## minimizing conditional entropy
ordering_ch <- function(train){
  n <- ncol(train) - 1
  free <- names(train)[-1]
  ms <- sapply(free, function(v){
    infotheo::condentropy(X = train[[v]], Y = train$answer)
  })
  selected <- c("answer", names(which.min(ms)))
  free <- free[-which.min(ms)]
  for (i in 2:n){
    ms <- sapply(free, function(v){
      infotheo::condentropy(X = train[[v]], 
                                Y = train[,selected])
    })
    selected <- c(selected, names(which.min(ms)))
    free <- free[-which.min(ms)]
    
  }
  return(selected)
}

## maximising normalized mi
ordering_mi <- function(train){
  ms <- sapply(names(train)[-1], function(v){
    infotheo::mutinformation(X = train$answer, Y = train[[v]]) / 
      infotheo::entropy(train[[v]])
  })
  names(train)[c(1, 1 + order(ms, decreasing = TRUE))]
}

predict_st <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train, class = "answer", prob = TRUE)
    
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob[, 2], 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test, class = "answer", prob = TRUE)
    pred <- factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
    prob <- prob[, 2]
  }
  else{
    cutoff <- 0.5
    prob <- predict(model, newdata = test, class = "answer", prob = TRUE)
    pred <- factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                          levels(train$answer)[1]), levels = levels(train$answer))
    prob <- prob[, 2]
  }
  return(list(pred = pred, prob = prob, cutoff = cutoff))
}

st_full <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::full(train, lambda = 1, join_zero = TRUE)
  predict_st(model, train, test, optimizecutoff)
}

st_full_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::full(train, lambda = 1, join_zero = TRUE, order = ordering_mi(train))
  predict_st(model, train, test, optimizecutoff)
}

st_full_ch <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::full(train, lambda = 1, join_zero = TRUE, order = ordering_ch(train))
  predict_st(model, train, test, optimizecutoff)
}

st_full_cmi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::full(train, lambda = 1, join_zero = TRUE, order = ordering_cmi(train))
  predict_st(model, train, test, optimizecutoff)
}

st_indep <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::indep(train, lambda = 1)
  predict_st(model, train, test, optimizecutoff)
}

st_indep_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::indep(train, lambda = 1, order = ordering_mi(train))
  predict_st(model, train, test, optimizecutoff)
}

st_indep_ch <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::indep(train, lambda = 1, order = ordering_ch(train))
  predict_st(model, train, test, optimizecutoff)
}

st_indep_cmi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::indep(train, lambda = 1, order = ordering_cmi(train))
  predict_st(model, train, test, optimizecutoff)
}

st_hc_indep_5 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- colnames(train)
  if(length(order_var) > 5) { order_var <- order_var[1:5] }
  model <- stagedtrees::join_zero(stagedtrees::indep(train, lambda = 1,
                                                     order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_indep_mi_5 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_mi(train)
  if(length(order_var) > 5) { order_var <- order_var[1:5] }
  model <- stagedtrees::join_zero(stagedtrees::indep(train, lambda = 1,
                                                     order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_indep_ch_5 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_ch(train)
  if(length(order_var) > 5) { order_var <- order_var[1:5] }
  model <- stagedtrees::join_zero(stagedtrees::indep(train, lambda = 1,
                                                     order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_indep_cmi_5 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_cmi(train)
  if(length(order_var) > 5) { order_var <- order_var[1:5] }
  model <- stagedtrees::join_zero(stagedtrees::indep(train, lambda = 1,
                                                     order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_indep_7 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- colnames(train)
  if(length(order_var) > 7) { order_var <- order_var[1:7] }
  model <- stagedtrees::join_zero(stagedtrees::indep(train, lambda = 1,
                                                     order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_indep_mi_7 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_mi(train)
  if(length(order_var) > 7) { order_var <- order_var[1:7] }
  model <- stagedtrees::join_zero(stagedtrees::indep(train, lambda = 1,
                                                     order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_indep_ch_7 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_ch(train)
  if(length(order_var) > 7) { order_var <- order_var[1:7] }
  model <- stagedtrees::join_zero(stagedtrees::indep(train, lambda = 1,
                                                     order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_indep_cmi_7 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_cmi(train)
  if(length(order_var) > 7) { order_var <- order_var[1:7] }
  model <- stagedtrees::join_zero(stagedtrees::indep(train, lambda = 1,
                                                     order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_full_5 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- colnames(train)
  if(length(order_var) > 5) { order_var <- order_var[1:5] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, 
                                     order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_full_mi_5 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_mi(train)
  if(length(order_var) > 5) { order_var <- order_var[1:5] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, 
                                       order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_full_cmi_5 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_cmi(train)
  if(length(order_var) > 5) { order_var <- order_var[1:5] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, 
                                       order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_full_ch_5 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_ch(train)
  if(length(order_var) > 5) { order_var <- order_var[1:5] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, 
                                       order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_full_7 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- colnames(train)
  if(length(order_var) > 7) { order_var <- order_var[1:7] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, 
                                       order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_full_cmi_7 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_cmi(train)
  if(length(order_var) > 7) { order_var <- order_var[1:7] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, 
                                       order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_full_mi_7 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_mi(train)
  if(length(order_var) > 7) { order_var <- order_var[1:7] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, 
                                       order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_hc_full_ch_7 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_ch(train)
  if(length(order_var) > 7) { order_var <- order_var[1:7] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, 
                                       order = order_var), name = "NA")
  model <- stagedtrees::hc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_fbhc <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1), name = "NA")
  model <- stagedtrees::fbhc.sevt(model)
  predict_st(model, train, test, optimizecutoff)
}

st_fbhc_ch <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_ch(train)), name = "NA")
  model <- stagedtrees::fbhc.sevt(model)
  predict_st(model, train, test, optimizecutoff)
}

st_fbhc_cmi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_cmi(train)), name = "NA")
  model <- stagedtrees::fbhc.sevt(model)
  predict_st(model, train, test, optimizecutoff)
}

st_fbhc_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_mi(train)), name = "NA")
  model <- stagedtrees::fbhc.sevt(model)
  predict_st(model, train, test, optimizecutoff)
}

st_bhc_5 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- colnames(train)
  if(length(order_var) > 5) { order_var <- order_var[1:5] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = order_var), name = "NA")
  model <- stagedtrees::bhc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bhc_mi_5 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_mi(train)
  if(length(order_var) > 5) { order_var <- order_var[1:5] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = order_var), name = "NA")
  model <- stagedtrees::bhc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bhc_cmi_5 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_cmi(train)
  if(length(order_var) > 5) { order_var <- order_var[1:5] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = order_var), name = "NA")
  model <- stagedtrees::bhc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bhc_ch_5 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_ch(train)
  if(length(order_var) > 5) { order_var <- order_var[1:5] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = order_var), name = "NA")
  model <- stagedtrees::bhc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bhc_7 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- colnames(train)
  if(length(order_var) > 7) { order_var <- order_var[1:7] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = order_var), name = "NA")
  model <- stagedtrees::bhc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bhc_mi_7 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_mi(train)
  if(length(order_var) > 7) { order_var <- order_var[1:7] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = order_var), name = "NA")
  model <- stagedtrees::bhc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bhc_cmi_7 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_cmi(train)
  if(length(order_var) > 7) { order_var <- order_var[1:7] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = order_var), name = "NA")
  model <- stagedtrees::bhc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bhc_ch_7 <- function(train, test, optimizecutoff = FALSE, ...){
  order_var <- ordering_ch(train)
  if(length(order_var) > 7) { order_var <- order_var[1:7] }
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = order_var), name = "NA")
  model <- stagedtrees::bhc.sevt(model, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bj_kl <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1), name = "NA")
  model <- stagedtrees::bj.sevt(model, distance = kl, thr = 0.2, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bj_kl_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_mi(train)), name = "NA")
  model <- stagedtrees::bj.sevt(model, distance = kl, thr = 0.2, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bj_kl_cmi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_cmi(train)), name = "NA")
  model <- stagedtrees::bj.sevt(model, distance = kl, thr = 0.2, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bj_kl_ch <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_ch(train)), name = "NA")
  model <- stagedtrees::bj.sevt(model, distance = kl, thr = 0.2, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bj_tv <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1), name = "NA")
  model <- stagedtrees::bj.sevt(model, distance = tv, thr = 0.2, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bj_tv_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_mi(train)), name = "NA")
  model <- stagedtrees::bj.sevt(model, distance = tv, thr = 0.2, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bj_tv_cmi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_cmi(train)), name = "NA")
  model <- stagedtrees::bj.sevt(model, distance = tv, thr = 0.2, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bj_tv_ch <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_ch(train)), name = "NA")
  model <- stagedtrees::bj.sevt(model, distance = tv, thr = 0.2, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bj_cd <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1), name = "NA")
  model <- stagedtrees::bj.sevt(model, distance = cd, thr = 0.2, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bj_cd_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_mi(train)), name = "NA")
  model <- stagedtrees::bj.sevt(model, distance = cd, thr = 0.2, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bj_cd_cmi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_cmi(train)), name = "NA")
  model <- stagedtrees::bj.sevt(model, distance = cd, thr = 0.2, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_bj_cd_ch <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_ch(train)), name = "NA")
  model <- stagedtrees::bj.sevt(model, distance = cd, thr = 0.2, ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_naive <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1), name = "NA")
  model <- stagedtrees::naive.sevt(model, distance = kl, method = "mcquitty", ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_naive_mi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_mi(train)), name = "NA")
  model <- stagedtrees::naive.sevt(model, distance = kl, method = "mcquitty", ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_naive_cmi <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_cmi(train)), name = "NA")
  model <- stagedtrees::naive.sevt(model, distance = kl, method = "mcquitty", ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_naive_ch <- function(train, test, optimizecutoff = FALSE, ...){
  model <- stagedtrees::join_zero(full(train, lambda = 1, order = ordering_ch(train)), name = "NA")
  model <- stagedtrees::naive.sevt(model, distance = kl, method = "mcquitty", ignore = "NA")
  predict_st(model, train, test, optimizecutoff)
}

st_naive_order <- function(train, test, optimizecutoff = FALSE, order){
  model <- stagedtrees::naive.sevt(full(train, join_zero = TRUE, lambda = 1, order = order),
                                   distance = kl, method = "mcquitty")
  predict_st(model, train, test, optimizecutoff)
}

st_fbhc_order <- function(train, test, optimizecutoff = FALSE, order, ...){
  model <- stagedtrees::fbhc.sevt(full(train, join_zero = TRUE, lambda = 1, 
				       order = order))
  predict_st(model, train, test, optimizecutoff)
}

st_bj_kl_order <- function(train, test, optimizecutoff = FALSE, order, ...){
  model <- stagedtrees::bj.sevt(full(train, join_zero = TRUE, lambda = 1, 
                                     order = order), distance = kl, thr = 0.2)
  predict_st(model, train, test, optimizecutoff)
}
