datasets <- read.table("binary_datasets_names.tsv")[,1]
classifiers <- c(
  "st_full",
  "st_indep",
  "st_hc_indep",
  "st_fbhc",
  "st_fbhc_mi",
  "st_fbhc_cmi",
  "st_fbhc_ch",
  "st_bhc",
  ## st_methods (stagedtrees)
  "st_bj_kl",
  "st_bj_tv",
  "st_bj_cd",
  "st_naive",
  "st_naive_mi",
  "st_naive_cmi",
  "st_naive_ch",
  ### st_methods  (stagedtrees)
  "bn_tabu",
  ## bn_methods (bnlearn)
  "bnc_nb",
  "bnc_tan_cl",
  "bnc_tan_hc",
  "bnc_fssj",
  "bnc_bsej",
  ## bnc_methods (bnclassify)
  "bnc_3db",
  ## nnet_methods (nnet)
  "nnet_basic",
  ## glm_methods (glm)
  "glm_binomial",
  "simple" ## simple
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
