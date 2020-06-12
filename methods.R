library(bnlearn)

library(nnet)
library(randomForest)
library(rpart)
library(MASS)
library(rrlda)
library(sda)
library(sparseLDA)
library(mda)
library(klaR)
library(HDclassif)
library(class)
library(kohonen)
library(adabag)
library(caTools)
library(ipred)
library(caret)
library(glmnet)
library(spls)
library(pls)

library(e1071)
library(gam)
library(polspline)
library(tree)
library(ada)
library(pROC)
library(InformationValue)
#library(readxl)

### all classifers are a function of train and test (plu additional args)
### and return predictions


source("st_methods.R")
source("bn_methods.R")
source("bnc_methods.R")

classifiers <- c(
  "st_full", "st_indep",  "st_fbhc", ## st_methods (stagedtrees) "st_hc_indep",
  "st_bj_kl", "st_bj_tv", "st_bj_cd", 
  "st_naive",### st_methods  (stagedtrees)
  "bn_tabu", ## bn_methods (bnlearn)
  "bnc_nb", "bnc_tan_cl", "bnc_tan_hc", "bnc_fssj", "bnc_bsej",  ## bnc_methods (bnclassify) 
  "bnc_3db", # "bnc_4db",  ## bnc_methods (bnclassify)
  "simple" ## simple
)


simple <- function(train, test, ...){
  m <- names(which.max(table(train$answer)))
  r <- rep(m, nrow(test))
  factor(r, levels = levels(train$answer))
}

