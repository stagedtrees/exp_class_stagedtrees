datasets <- read.table("binary_fast_datasets_names.tsv")[, 1]
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

source("statistics.R")
# k <- 5
nreps <- 10

TABLE <- array(
  data = NA,
  dim = c(
    length(statistics) - 1,
    length(datasets),
    length(classifiers),
    nreps
  ),
  dimnames = list(
    stat = statistics[-length(statistics)],
    data = datasets,
    classifier = classifiers,
    rep = 1:nreps
  )
)

ROC_CURVE <- array(
  data = rep(NA, 200),
  dim = c(
    2,
    length(datasets),
    length(classifiers),
    nreps,
    200  # we have to store multiple values of specificities and sensitivities to plot Roc Curves.
  ),
  dimnames = list(
    stat = c("specificities", "sensitivities"),
    data = datasets,
    classifier = classifiers,
    rep = 1:nreps,
    values = 1:200
  )
)



for (d in datasets) {
  res_path <- paste0("results/", d, "/")
  data <- readRDS(paste0("datasets/", d, ".rds"))
  split_path <- paste0("splits/", d, "/")
  for (r in 1:nreps) {  
    id_test <- readRDS(paste0(split_path, r, "_id_test.rds"))
    true <- data$answer[id_test]
    for (c_name in classifiers) {
      filename <- paste0(res_path, c_name, "_", r, ".rds" )
      if (file.exists(filename)){
        res <- readRDS(filename)
        for (stat in statistics[-length(statistics)]){
          stat_fun <- get(stat)
          TABLE[stat, d, c_name, r] <- stat_fun(res, true)
        }
        stat_fun <- get(statistics[length(statistics)])
        auxiliar <- matrix(unlist(stat_fun(res, true)), nrow = 2, byrow = TRUE)
        ROC_CURVE[1:2, d, c_name, r, 1:NCOL(auxiliar)] <- auxiliar
      }
    }
  }
}

saveRDS(TABLE, "TABLE.rds")
saveRDS(ROC_CURVE, "ROC_CURVE.rds")

