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

classifiers <- c("st_full_cmi", "st_indep_cmi", "st_hc_indep_cmi_5", "st_hc_indep_cmi_7", 
                 "st_hc_full_cmi_5", "st_fbhc_cmi", "st_bhc_cmi_5", "st_bhc_cmi_7", 
                 "st_bj_kl_01_cmi", "st_bj_kl_05_cmi", "st_bj_kl_20_cmi",
                 "st_naive_cmi", "st_kmeans_cmi", "bn_tabu", "bn_hc","bnc_3db", "bnc_nb", "bnc_tan_hc",
                 "nnet_1", "rf_1", "logistic_basic", "cl_tree_1", "regularized_da", "naive_bayes_1",
                 "boosting_basic", "bagging_basic", "svm_basic", "gam_basic", "adaboost_basic")


