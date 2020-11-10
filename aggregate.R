datasets <- read.table("binary_fast_datasets_names.tsv")[, 1]
classifiers <- c("st_full_cmi", "st_indep_cmi", "st_hc_indep_cmi_5", "st_hc_indep_cmi_7", 
                 "st_hc_full_cmi_5", "st_fbhc_cmi", "st_bhc_cmi_5", "st_bhc_cmi_7", 
                 "st_bj_kl_01_cmi", "st_bj_kl_05_cmi", "st_bj_kl_20_cmi",
                 "st_naive_cmi", "st_kmeans_cmi", "bn_tabu", "bn_hc","bnc_3db", "bnc_nb", "bnc_tan_hc",
                 "nnet_1", "rf_1", "logistic_basic", "cl_tree_1", "regularized_da", "naive_bayes_1",
                 "boosting_basic", "bagging_basic", "svm_basic", "gam_basic", "adaboost_basic")

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

