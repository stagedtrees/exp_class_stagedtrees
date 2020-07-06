args <- commandArgs(trailingOnly = TRUE)
datasets <- read.table("binary_fast_datasets_names.tsv")[,1]
source("methods.R")
if (length(args) > 0){
  if (base::endsWith(args[1], ".tsv")){ 
    datasets <- read.table(args[1])[,1]    
  }else{ ##it is the name of a dataset
    datasets <- args[1]
  }
}
if (length(args) > 1){
  torun <- unlist(sapply(args[-1], function(x){
    if (base::endsWith(x, "_")){
      classifiers[base::startsWith(classifiers, x)]
    }else{
      x
    }
  }))
}else{
  torun <- classifiers
}

nreps <- 20

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
    for (c_name in torun){
      message(c_name)
      c_fun <- get(c_name)
      time <- system.time(predict <- c_fun(train, test, optimizecutoff = FALSE))[3]
      filename <- paste0(res_path, c_name, "_", r, ".rds" )
      message(filename)
      saveRDS(list(time = time, predict = predict), file = filename)
    }
  }
}



