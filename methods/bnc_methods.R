library(bnclassify)

predict_bnc <- function(model, train, test, optimizecutoff){
  if (optimizecutoff){
    prob <- predict(model, newdata = train,  prob = TRUE )
    
    cutoff <- InformationValue::optimalCutoff(actuals = as.numeric(train$answer) - 1, 
                                              predictedScores = prob, 
                                              optimiseFor = "Both")
    prob <- predict(model, newdata = test, prob = TRUE )
    factor(ifelse(prob[, 2] >= cutoff, levels(train$answer)[2], 
                  levels(train$answer)[1]), levels = levels(train$answer))
  }else{
    predict(model, newdata = test)
  }
}

bnc_nb <- function(train, test, optimizecutoff = FALSE){
  model <- bnclassify::nb(class = "answer", dataset = train)
  model <- lp(model, dataset = train, smooth = 1)
  predict_bnc(model, train, test, optimizecutoff)
}

bnc_tan_cl <- function(train, test, optimizecutoff = FALSE){
  model <- bnclassify::tan_cl(class = "answer", dataset = train)
  model <- lp(model, dataset = train, smooth = 1)
  predict_bnc(model, train, test, optimizecutoff)
}

bnc_tan_hc <- function(train, test, optimizecutoff = FALSE){
  model <- bnclassify::tan_hc(class = "answer", dataset = train, k = 10)
  model <- lp(model, dataset = train, smooth = 1)
  predict_bnc(model, train, test, optimizecutoff)
}

bnc_fssj <- function(train, test, optimizecutoff = FALSE){
  model <- bnclassify::fssj(class = "answer", dataset = train, 
                            k = 10, smooth = 1)
  model <- lp(model, dataset = train, smooth = 1)
  predict_bnc(model, train, test, optimizecutoff)
}

bnc_bsej <- function(train, test, optimizecutoff = FALSE){
  model <- bnclassify::bsej(class = "answer", dataset = train, 
                            k = 10, smooth = 1)
  model <- lp(model, dataset = train, smooth = 1)
  predict_bnc(model, train, test, optimizecutoff)
}


bnc_3db <- function(train, test, optimizecutoff = FALSE){
  model <- bnclassify::kdb(class = "answer", dataset = train, 
                           k = 10, smooth = 1, kdbk = 3)
  model <- lp(model, dataset = train, smooth = 1)
  predict_bnc(model, train, test, optimizecutoff)
}

bnc_4db <- function(train, test, optimizecutoff = FALSE){
  model <- bnclassify::kdb(class = "answer", dataset = train, 
                           k = 10, smooth = 1, kdbk = 4)
  model <- lp(model, dataset = train, smooth = 1)
  predict_bnc(model, train, test, optimizecutoff)
}
