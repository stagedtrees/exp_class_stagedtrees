source("methods.R")
datasets <- readRDS("binary_fast_datasets_names.rds")
k <- 10
nreps <- 10
dir.create("results/", showWarnings = FALSE)
for (d in datasets){
   res_path <- paste0("results/",d,"/") 
   dir.create(res_path, showWarnings = FALSE)
   data <- readRDS(paste0("datasets/", d, ".rds"))
   split_path <- paste0("splits/", d, "/")
  for (r in 1:nreps){
    id_test <- readRDS(paste0(split_path, r, "_id_test.rds"))
    train <- data[-id_test,]
    test <- data[id_test,]
    for (c_name in classifiers){
      c_fun <- get(c_name)
      time <- system.time(predict <- c_fun(train, test))[3]
      filename <- paste0(res_path, c_name, "_", r, ".rds" )
      message(filename)
      saveRDS(list(time = time, predict = predict), file = filename)
    }
  }
}



