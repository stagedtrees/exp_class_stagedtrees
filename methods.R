### all classifers are a function of train and test (plus additional args)
### and return probabilities, predictions and cutoff.

source("methods/st_methods.R")
source("methods/bn_methods.R")
source("methods/bnc_methods.R")
source("methods/nnet_methods.R")
source("methods/glm_methods.R")
source("methods/rf_methods.R")
source("methods/logistic_methods.R")
source("methods/tree_methods.R")
source("methods/discriminant_analysis_methods.R")
source("methods/naive_methods.R")
source("methods/naive_2_methods.R")
source("methods/boosting_methods.R")
source("methods/bagging_methods.R")
source("methods/adaboost_methods.R")
source("methods/svm_methods.R")
source("methods/gam_methods.R")

classifiers <- c(
  "st_full", "st_full_mi", "st_full_cmi", "st_full_ch",
  "st_indep", "st_indep_mi", "st_indep_cmi", "st_indep_ch",
  "st_hc_indep_5", "st_hc_indep_mi_5", "st_hc_indep_cmi_5", "st_hc_indep_ch_5",
  "st_hc_indep_7", "st_hc_indep_mi_7", "st_hc_indep_cmi_7", "st_hc_indep_ch_7",
  "st_hc_full_5", "st_hc_full_mi_5", "st_hc_full_cmi_5", "st_hc_full_ch_5",
  # "st_hc_full_7", "st_hc_full_mi_7", "st_hc_full_cmi_7", "st_hc_full_ch_7",
  "st_fbhc", "st_fbhc_mi", "st_fbhc_cmi", "st_fbhc_ch",
  "st_bhc_5", "st_bhc_mi_5", "st_bhc_cmi_5", "st_bhc_ch_5",
  "st_bhc_7", "st_bhc_mi_7", "st_bhc_cmi_7", "st_bhc_ch_7",
  "st_bj_kl", "st_bj_kl_mi", "st_bj_kl_cmi", "st_bj_kl_ch",
  "st_bj_tv", "st_bj_tv_mi", "st_bj_tv_cmi", "st_bj_tv_ch",
  "st_bj_cd", "st_bj_cd_mi", "st_bj_cd_cmi", "st_bj_cd_ch",
  "st_naive", "st_naive_mi", "st_naive_cmi", "st_naive_ch",
  "bn_tabu",
  "bn_hc",
  "bnc_nb",
  "bnc_tan_cl",
  "bnc_tan_hc",
  "bnc_fssj",
  "bnc_bsej",
  "bnc_3db",
  "nnet_1",
  "nnet_2",
  "rf_1",
  "rf_2",
  "glm_binomial",
  "logistic_basic",
  "naive_bayes_1",
  "naive_bayes_2",
  "cl_tree_1",
  "cl_tree_2",
  "regularized_da",
  "boosting_basic",
  "bagging_basic",
  "svm_basic",
  "gam_basic",
  "adaboost_basic",
  "simple"
)


simple <- function(train, test, ...){
  m <- names(which.max(table(train$answer)))
  r <- rep(m, NROW(test))
  prob <- ifelse(as.integer(which.max(table(train$answer))) == 2, 1, 0)
  prob <- rep(prob, NROW(test))
  pred <- factor(r, levels = levels(train$answer))
  cutoff <- 0.5
  return(list(pred = pred, prob = prob, cutoff = cutoff))
}

