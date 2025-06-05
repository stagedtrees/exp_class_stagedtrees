datasets <- read.table("binary_datasets_names.tsv")[, 1]

classifiers <- c("st_bhc_3db",
                 "st_bhc_5db",
                 "st_bhc_tan_cl",
                 "st_bhc_tan_hc",
                 "st_kmeans_cmi",
                 "st_hclust_cmi",
                 #"st_bhc_cmi",
                 #"bn_tabu", 
                 "bnc_3db", 
                 "bnc_5db", 
                 #"bnc_nb", 
                 "bnc_tan_hc",
                 "bnc_tan_cl",
                 "rf_1")



source("statistics.R")
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

AVG <- apply(TABLE, c(1,2,3), mean, na.rm = TRUE)

saveRDS(TABLE, "TABLE.rds")

saveRDS(AVG, "AVG.rds")


