datasets <- read.table("binary_datasets_names.tsv")[,1]
classifiers <- c(
  "st_full",
  "st_indep",
  "st_hc_full_mi",
  "st_hc_indep_mi",
  "st_fbhc",
  "st_fbhc_mi",
  "st_fbhc_cmi",
  "st_fbhc_ch",
  "st_bhc_mi",
  "st_bhc_cmi",
  "st_bj_kl_mi",
  "st_bj_tv_mi",
  "st_bj_cd_mi",
  "st_naive",
  "st_naive_mi",
  "st_naive_cmi",
  "st_naive_ch",
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
k <- 10
nreps <- 10

TABLE <- array(
  data = NA,
  dim = c(
    length(statistics),
    length(datasets),
    length(classifiers),
    nreps
  ),
  dimnames = list(
    stat = statistics,
    data = datasets,
    classifier = classifiers,
    rep = 1:nreps
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
        for (stat in statistics){
          stat_fun <- get(stat)
          TABLE[stat, d, c_name, r] <- stat_fun(res, true)
        }
      }
        
    }
  }
}

saveRDS(TABLE, "TABLE.rds")

