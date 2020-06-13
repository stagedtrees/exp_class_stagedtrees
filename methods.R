### all classifers are a function of train and test (plu additional args)
### and return predictions


source("st_methods.R")
source("bn_methods.R")
source("bnc_methods.R")

classifiers <- c(
  ## st_methods (stagedtrees) 
  "st_full", "st_indep",  
  "st_hc_indep",
  "st_fbhc", "st_bhc", 
  "st_bj_kl", "st_bj_tv", "st_bj_cd", 
  "st_naive",
  ## bn_methods (bnlearn)
  "bn_tabu", 
  ## bnc_methods (bnclassify)
  "bnc_nb", "bnc_tan_cl", "bnc_tan_hc", "bnc_fssj", "bnc_bsej",  
  "bnc_3db", 
  "simple" ## simple
)


simple <- function(train, test, ...){
  m <- names(which.max(table(train$answer)))
  r <- rep(m, nrow(test))
  factor(r, levels = levels(train$answer))
}

