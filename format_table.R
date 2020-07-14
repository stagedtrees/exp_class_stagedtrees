library(xtable)

##read TABLE.rds
TABLE <- readRDS("TABLE.rds")
## compute averages
AVG <- apply(TABLE, c(1,2,3), mean, na.rm = TRUE)



Xaccuracy <- xtable(AVG["accuracy",,])

nice_accuracy <- toLatex(Xaccuracy)


writeLines(nice_accuracy, "AVG_accuracy.tex")
