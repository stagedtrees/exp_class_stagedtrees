library("stagedtrees")
source("methods.R")
source("statistics.R")

nreps <- 100
n <- 1000
ps <- 2:15
ks <- 2:3
stats <- c("time", "accuracy", "f1")

torun <- c("st_kmeans_cmi", "st_fbhc_cmi", 
           "bnc_nb", "nnet_1", "rf_1", "logistic_basic")
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

for (k in ks){
for (p in ps){
  tree <- lapply(1:p, function(i) c(1:k))
  names(tree) <- paste0("X",1:p)
  tree <- c(list(answer = c(0,1)), tree)
  pb <- txtProgressBar(min = 1, max = nreps, style = 3)
  message(p)
  for (rep in 1:nreps){
    setTxtProgressBar(pb, rep)
    todo <- TRUE
    while (todo) {
      model <- random_sevt(x = tree, q = 0.7)
      train <- sample_from(model, nsim = n)
      test <- sample_from(model, nsim = n)
      if (length(unique(train$answer)) == 2){
        todo <- FALSE
      }
    }
    for (c_name in torun){
      #message(c_name)
      c_fun <- get(c_name)
      time_ <- system.time(predict <- c_fun(train, test, optimizecutoff = FALSE))[3]
      res <- list(time = time_, predict = predict$pred, probability = predict$prob, 
                   cutoff = predict$cutoff)
      for (stat in stats){
        stat_fun <- get(stat)
        TABLE[stat, c_name, paste0(p), paste0(k), rep] <- stat_fun(res, test$answer)
      }
    }
  }
  close(pb)
}
}

saveRDS(TABLE, "TABLS.rds")
AVG <- apply(TABLE, 1:4, mean, na.rm = TRUE)


library(ggplot2)
library(reshape2)

AVG['time',,,] <- log(AVG['time',,,], base = 10)
saveRDS(AVG, file = "AVG.rds")

dd <- melt(AVG,value.name = "value", na.rm = TRUE)
levels(dd$stat)[1] <- "log-time"
levels(dd$classifier) <- c("ST_NAIVE_KM", "ST_FBHC", "BNC_NB", "NNet", "RFor", "Logistic")
PLOT <- ggplot(dd) + geom_line(aes(x = p, y = value, group = classifier, color = classifier)) + 
  facet_grid(rows = vars(stat), cols = vars(k),  scales = "free") + theme_bw() + ylab("") + 
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave(filename = "res_simulation.pdf", PLOT, width = 5, height = 4)
