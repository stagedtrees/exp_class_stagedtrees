args <- commandArgs(trailingOnly = TRUE)
datasets <- factor(read.table("binary_fast_datasets_names.tsv")[, 1])
source("methods.R")
if (length(args) > 0){
  if (base::endsWith(args[1], ".tsv")){ 
    datasets <- read.table(args[1])[,1]    
  }else{ ## it is the name of a dataset
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

nreps <- 1

# dir.create("results_2_0_0/", showWarnings = FALSE)
set.seed(2020)
for (d in datasets){
  message(d)
  timestamp()
   res_path <- paste0("results_2_0_0/",d,"/") 
   dir.create(res_path, showWarnings = FALSE)
   data <- readRDS(paste0("datasets/", d, ".rds"))
   split_path <- paste0("splits/", d, "/")
  for (r in 9:10){ 
    # message(r)
    id_test <- readRDS(paste0(split_path, r, "_id_test.rds"))
    train <- data[-id_test,]
    test <- data[id_test,]
    for (c_name in torun){
      if(d == "tic_tac_toe" & any(c_name == c("st_hc_indep_7", "st_hc_indep_mi_7", "st_hc_indep_cmi_7", "st_hc_indep_ch_7", 
                                              "st_bhc_7", "st_bhc_mi_7", "st_bhc_cmi_7", "st_bhc_ch_7",
                                              "st_hc_full_5", "st_hc_full_mi_5", "st_hc_full_cmi_5", "st_hc_full_ch_5")))
      {
        next
      }
      # message(c_name)
      c_fun <- get(c_name)
      time <- system.time(predict <- c_fun(train, test, optimizecutoff = TRUE))[3]
      filename <- paste0(res_path, c_name, "_", r, ".rds")
      if(time > 60) { cat(c_name, ":", time, "\n") }
      saveRDS(list(time = time, predict = predict$pred, probability = predict$prob, 
                   cutoff = predict$cutoff), file = filename)
    }
  }
}



