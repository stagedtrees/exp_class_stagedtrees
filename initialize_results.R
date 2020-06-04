#############################  INITIALIZE RESULTS OBJECT  #############################  



# load the .R file with the code to install and load all the needed libraries
# source("/Users/federico/Dropbox/Gherardo:Manuele/Classification Paper/install_load_packages.R")

load("/Users/federico/Dropbox/Gherardo:Manuele/Classification Paper/datasets.RData")

all_datasets <- list(abalone, Asym, breast_cancer, breast_cancer_wisconsin, car,chds, 
                     chestSim500, energy1, energy2, FallEld, fertility, house_votes, indian_liver, iris,                   
                     magic04, monks1, monks2, monks3, nursery, PhDArticles, Pokemon, post_operative,
                     puffin, reinis, selfy, tae, tic_tac_toe, Titanic)
attr(all_datasets, "names") <- ls()[-2]
# categories_answer <- sapply(all_datasets, function(x) length(levels(x[, 1])))
binary_datasets <- all_datasets[-c(1, 5, 14, 19, 20, 22, 25, 26)]  # 20 binary datasets

rm(list = setdiff(ls(), c("all_datasets", "binary_datasets")))


#############################  OBJECT TO STORE ALL THE RESULTS WE NEED  #############################  


# results: object with all results for all binary datasets.
results <- rep(list(list()), length(binary_datasets))
for(i in 1:length(results)) results[[i]] <- rep(list(list()), 6)
attr(results, "names") <- noquote(names(binary_datasets))
for(i in 1:length(results)) attr(results[[i]], "names") <- c("computational_time", "summary", "roc_curve",
                                                             "train", "test", "validation")

for(i in 1:length(results)) results[[i]][[1]] <- matrix(NA, nrow = 37, ncol = 10, 
                                                        dimnames = list(c("m_full", "m_indep", sprintf("m%d", c(1:6, 11:39))), 
                                                                        sprintf("sample_%d", 1:10)))

for(i in 1:length(results)) results[[i]][[2]] <- rep(list(list()), 10)
for(i in 1:length(results)) {
  for(j in 1:10) {
    results[[i]][[2]][[j]] <- matrix(NA, nrow = 37, ncol = 14, dimnames = list(c("m_full", "m_indep", sprintf("m%d", c(1:6, 11:39))), 
                                                                               c("TN", "FN", "FP", "TP", "accuracy", "error", "FNR", "FPR", "sensitivity",
                                                                                 "specificity", "precision", "F1-score", "AUC", "threshold")))
  }
} 
for(i in 1:length(results)) {
  attr(results[[i]][[2]], "names") <- sprintf("sample_%d", 1:10)
}

for(i in 1:length(results)) {
  results[[i]][[3]] <- rep(list(list()), 2)
  attr(results[[i]][[3]], "names") <- c("specificity", "sensitivity")
  for(j in 1:10) {
    results[[i]][[3]][[1]] <- rep(list(list()), 10)
    results[[i]][[3]][[2]] <- rep(list(list()), 10)
    for(k in 1:10) {
      results[[i]][[3]][[1]][[k]] <- matrix(NA, nrow = 32, ncol = 100, dimnames = list(c("m_full", "m_indep", 
                                                                                         sprintf("m%d", c(1:6, 11:31, 36, 37, 39))), sprintf("value%d", 1:100)))
      results[[i]][[3]][[2]][[k]] <- matrix(NA, nrow = 32, ncol = 100, dimnames = list(c("m_full", "m_indep", 
                                                                                         sprintf("m%d", c(1:6, 11:31, 36, 37, 39))), sprintf("value%d", 1:100)))
    }
  }
  attr(results[[i]][[3]][[1]], "names") <- sprintf("sample_%d", 1:10)
  attr(results[[i]][[3]][[2]], "names") <- sprintf("sample_%d", 1:10)
} 


nrow_binary_datasets <- sapply(all_datasets[names(all_datasets) %in% names(binary_datasets)], dim)[1, ]
nrow_tr <- floor(nrow_binary_datasets * 0.6)
nrow_test <- floor(nrow_binary_datasets * 0.2)
nrow_val <- floor(nrow_binary_datasets * 0.2)
# rbind(nrow_binary_datasets, c(nrow_tr + nrow_test + nrow_val))
for(i in 1:length(nrow_tr)) {
  if(nrow_binary_datasets[i] == (nrow_tr[i] + nrow_test[i] + nrow_val[i] - 1)) {
    nrow_test[i] <- nrow_test[i] - 1
  }
  else if(nrow_binary_datasets[i] == (nrow_tr[i] + nrow_test[i] + nrow_val[i] + 1)) {
    nrow_test[i] <- nrow_test[i] + 1
  }
  else if(nrow_binary_datasets[i] == (nrow_tr[i] + nrow_test[i] + nrow_val[i] + 2)) {
    nrow_test[i] <- nrow_test[i] + 2
  }
  else if(nrow_binary_datasets[i] == (nrow_tr[i] + nrow_test[i] + nrow_val[i] - 2)) {
    nrow_test[i] <- nrow_test[i] -2 
  }
}
# rbind(nrow_binary_datasets, c(nrow_tr + nrow_test + nrow_val))
for(i in 1:length(results)) results[[i]][[4]] <- matrix(NA, nrow = nrow_tr[i], ncol = 10, 
                                                        dimnames = list(sprintf("id_%d", 1:nrow_tr[i]), 
                                                                        sprintf("sample_%d", 1:10)))
for(i in 1:length(results)) results[[i]][[5]] <- matrix(NA, nrow = nrow_test[i], ncol = 10, 
                                                        dimnames = list(sprintf("id_%d", 1:nrow_test[i]), 
                                                                        sprintf("sample_%d", 1:10)))
for(i in 1:length(results)) results[[i]][[6]] <- matrix(NA, nrow = nrow_val[i], ncol = 10, 
                                                        dimnames = list(sprintf("id_%d", 1:nrow_val[i]), 
                                                                        sprintf("sample_%d", 1:10)))

rm(i); rm(j); rm(k); rm(nrow_tr); rm(nrow_test); rm(nrow_val); rm(nrow_binary_datasets)





