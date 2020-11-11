datasets <- read.table("binary_fast_datasets_names.tsv")[, 1]

classifiers <- c("st_full_cmi", "st_indep_cmi", "st_hc_indep_cmi_5", "st_hc_indep_cmi_7", 
                 "st_hc_full_cmi_5", "st_fbhc_cmi", "st_bhc_cmi_5", "st_bhc_cmi_7", 
                 "st_bj_kl_01_cmi", "st_bj_kl_05_cmi", "st_bj_kl_20_cmi",
                 "st_naive_cmi", "st_kmeans_cmi", "bn_tabu", "bn_hc","bnc_3db", "bnc_nb", "bnc_tan_hc",
                 "nnet_1", "rf_1", "logistic_basic", "cl_tree_1", "regularized_da", "naive_bayes_1",
                 "boosting_basic", "bagging_basic", "svm_basic", "gam_basic", "adaboost_basic")

source("statistics.R")
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

AVG <- apply(TABLE, c(1,2,3), mean, na.rm = TRUE)

constantwise_function <- function(x, x.val, y.val) {
  x.val <- sort(as.numeric(na.exclude(1 - x.val)))
  y.val <- sort(as.numeric(na.exclude(y.val)))
  out <- 0
  
  for(i in 1:(length(x.val) - 1)) {
    if(x > x.val[i] & x <= x.val[i+1]) {
      out <- ((y.val[i] - y.val[i+1]) / (x.val[i] - x.val[i+1])) * x + y.val[i] - 
        ((y.val[i] - y.val[i+1]) / (x.val[i] - x.val[i+1])) * x.val[i]
    }
  }
  return(out)
}

constantwise_function <- Vectorize(constantwise_function, "x")

interpolation <- function(x.val, y.val) {
  y <- NULL
  count <- 0
  for(i in c(seq(0, 0.3, length.out = 70), seq(0.3, 0.8, length.out = 24), 
             seq(0.8, 1, length.out = 24))) {
    count <- count + 1
    y[count] <- constantwise_function(i, x.val, y.val)
  }
  return(c(0, y, 1))
}

interpolated_sensitivity <- array(
  data = rep(NA, 120),
  dim = c(
    length(datasets),
    length(classifiers),
    nreps,
    120 
  ),
  dimnames = list(
    data = datasets,
    classifier = classifiers,
    rep = 1:nreps,
    values = 1:120
  )
)

for(i in 1:length(datasets)) {
  for(j in 1:length(classifiers)) {
    for(k in 1:nreps) {
      interpolated_sensitivity[i, j, k, ] <- interpolation(ROC_CURVE[1, i, j, k, ], ROC_CURVE[2, i, j, k, ])
    }
  }
}

AVG_ROC_CURVE <- apply(interpolated_sensitivity, c(1,2,4), mean)


saveRDS(TABLE, "TABLE.rds")
saveRDS(ROC_CURVE, "ROC_CURVE.rds")

saveRDS(AVG, "AVG.rds")
saveRDS(AVG_ROC_CURVE, "AVG_ROC_CURVE.rds")



