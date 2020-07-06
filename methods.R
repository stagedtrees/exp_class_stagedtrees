### all classifers are a function of train and test (plu additional args)
### and return predictions


source("methods/st_methods.R")
source("methods/bn_methods.R")
source("methods/bnc_methods.R")
source("methods/nnet_methods.R")
source("methods/glm_methods.R")
source("methods/rf_methods.R")

classifiers <- c(
  ## st_methods (stagedtrees) 
  "st_full", "st_indep",  
  #"st_hc_indep",
  "st_fbhc", 
  "st_fbhc_mi", 
  "st_fbhc_cmi", 
  "st_fbhc_ch", 
  "st_bhc", 
  "st_bj_kl", "st_bj_tv", "st_bj_cd", 
  "st_naive",
  "st_naive_mi",
  "st_naive_cmi",
  "st_naive_ch",
  ## bn_methods (bnlearn)
  "bn_tabu", 
  ## bnc_methods (bnclassify)
  "bnc_nb", "bnc_tan_cl", "bnc_tan_hc", "bnc_fssj", "bnc_bsej",  
  "bnc_3db", 
  ## nnet_methods (nnet)
  "nnet_basic",
  ## glm_methods (glm)
  "glm_binomial",
  ## rf_methods (randomForest)
  "rf+basic",
  "simple" ## simple
)


simple <- function(train, test, ...){
  m <- names(which.max(table(train$answer)))
  r <- rep(m, nrow(test))
  factor(r, levels = levels(train$answer))
}

