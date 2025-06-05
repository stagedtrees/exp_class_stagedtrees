library("stagedtrees")
source("methods.R")
source("statistics.R")

nreps <- 100
n <- 10000
ps <- 2:15
ks <- 2
stats <- c("time", "accuracy", "f1")

torun <- c("st_bhc_3db",
           "st_bhc_tan_cl",
           "st_kmeans_cmi",
            "bnc_3db", 
            "bnc_nb", 
            "bnc_tan_hc")

TABLE <- array(
  data = NA,
  dim = c(
    length(stats),
    length(torun),
    length(ps),
    length(ks),
    nreps
  ),
  dimnames = list(
    stat = stats,
    classifier = torun,
    p = ps,
    k = ks,
    rep = 1:nreps
  )
)

for (p in ps){
  tree <- lapply(1:p, function(i) paste0(1:k))
  names(tree) <- paste0("X",1:p)
  tree <- c(list(answer = c("0","1")), tree)
  pb <- txtProgressBar(min = 1, max = nreps, style = 3)
  message(p)
  for (rep in 1:nreps){
    setTxtProgressBar(pb, rep)
    todo <- TRUE
    while (todo) {
      model <- random_sevt(x = tree, q = 0.5)
      train <- sample_from(model, size =  n)
      test <- sample_from(model, size =  n)
      #test <- expand.grid(model$tree[-1])
      #test$answer <- predict(model, test)
      if (length(unique(train$answer)) == 2){
        todo <- FALSE
      }
    }
    for (c_name in torun){
      #message(c_name)
      c_fun <- get(c_name)
      time_ <- system.time(predict <- c_fun(train, test))[3]
      res <- list(time = time_, predict = predict)
      for (stat in stats){
        stat_fun <- get(stat)
        TABLE[stat, c_name, paste0(p), paste0(k), rep] <- stat_fun(res, test$answer)
      }
    }
  }
  close(pb)
}


saveRDS(TABLE, "TABLE_SIMULATION.rds")
