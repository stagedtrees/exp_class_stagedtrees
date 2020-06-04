#############################  PACKAGES  #############################  

# graphical models packages
install.packages("bnlearn")
install.packages("devtools")
devtools::install_github("gherardovarando/stagedtrees", ref = "master")

# Paper "Do we need hundreds of classifiers to solve real world classification problems?" R packages.
install.packages("nnet")
install.packages("randomForest")
install.packages("rpart")
install.packages("MASS")
install.packages("rrlda")
install.packages("sda")
install.packages("sparseLDA")
install.packages("mda")
install.packages("klaR")
install.packages("HDclassif")
install.packages("class")
install.packages("kohonen")
install.packages("adabag")
install.packages("caTools")
install.packages("ipred")
install.packages("caret")
install.packages("glmnet")
install.packages("spls")
install.packages("pls")

# other packages
install.packages("e1071")
install.packages("gam")
install.packages("polspline")
install.packages("tree")
install.packages("ada")
install.packages("pROC")
install.packages("InformationValue")


#######   load the needed libraries
library(bnlearn)
library(stagedtrees)

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

#############################  DATASETS  #############################  


abalone <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/abalone.data", header = F)
abalone <- na.exclude(abalone)

# da eseguire su tutte le variabili continue tranne la risposta. Quella la facciamo "a mano" col codice sotto.
data <- abalone
for(k in 1:(NCOL(data) - 1)) {
  if(is.numeric(data[, k])) {
    m <- round(mean(data[, k]), 4)
    for(i in 1:NROW(data)) {
      data[i, k] <- ifelse(data[i, k] >= m, 1, 0)
    }
    data[, k] <- factor(data[, k], labels = c(paste(c("< ", as.character(m)), collapse = ""),
                                              paste(c(">= ", as.character(m)), collapse = "")))
  }
}
abalone <- data

# variabile risposta
summary(abalone$V9)
quant <- quantile(abalone$V9, probs = c(0.33, 0.66))
for(i in 1:NROW(abalone)) {
      abalone$V9[i] <- ifelse(abalone$V9[i] < quant[1], 1, abalone$V9[i])
      abalone$V9[i] <- ifelse(abalone$V9[i] >= quant[1] & abalone$V9[i] <= quant[2], 2, abalone$V9[i])
      abalone$V9[i] <- ifelse(abalone$V9[i] > quant[2], 3, abalone$V9[i])
}
abalone$V9 <- factor(abalone$V9, labels = c(paste(c("< ", as.character(quant[1])), collapse = ""),
                                            paste(c(as.character(quant[1]), " - ", as.character(quant[2])), collapse = ""),
                                              paste(c("> ", as.character(quant[2])), collapse = "")))
str(abalone)
colnames(abalone) <- c("answer", colnames(abalone)[-1])




# adult <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/adult.data", header = F)

breast_cancer_wisconsin <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/breast-cancer-wisconsin.data", header = F)[, -1]
breast_cancer_wisconsin <- breast_cancer_wisconsin[breast_cancer_wisconsin$V7 != "?", ]
breast_cancer_wisconsin$V7 <- factor(breast_cancer_wisconsin$V7, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
breast_cancer_wisconsin$V7 <- as.numeric(breast_cancer_wisconsin$V7)

data <- breast_cancer_wisconsin
for(k in 1:(NCOL(data) - 1)) {
  if(is.numeric(data[, k])) {
    m <- 5
    for(i in 1:NROW(data)) {
      data[i, k] <- ifelse(data[i, k] > m, 1, 0)
    }
    data[, k] <- factor(data[, k], labels = c("<= 5", "> 5"))
  }
}
breast_cancer_wisconsin <- data
rm(data)

breast_cancer_wisconsin$V11 <- factor(breast_cancer_wisconsin$V11, labels = c("benign", "malignant"))
breast_cancer_wisconsin <- breast_cancer_wisconsin[, c(10, 1:9)]
str(breast_cancer_wisconsin)
colnames(breast_cancer_wisconsin) <- c("answer", colnames(breast_cancer_wisconsin)[-1])



# direi troppo grande.
breast_cancer <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/breast-cancer.data", header = F, na.strings = "?")
breast_cancer <- data.frame(na.exclude(breast_cancer))
breast_cancer$V7 <- factor(breast_cancer$V7)
colnames(breast_cancer) <- c("answer", colnames(breast_cancer)[-1])



car <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/car.data", header = F)
car <- car[, c(7, 1:6)]
colnames(car) <- c("answer", colnames(car)[-1])




# diagnosis <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/diagnosis.data", header = F)
# echocardiogram <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/echocardiogram.data", header = F)




library(readxl)
energy <- as.data.frame(read_excel("Desktop/Latex/Paper Classification/datasets/ENB2012_data.xlsx"))
for(i in 1:NROW(energy)) {
  energy$X1[i] <- ifelse(energy$X1[i] >= 0.80, 3, energy$X1[i])
  energy$X1[i] <- ifelse(energy$X1[i] > 0.70 & energy$X1[i] <= 0.80, 2, energy$X1[i])
  energy$X1[i] <- ifelse(energy$X1[i] <= 0.70, 1, energy$X1[i])
}
energy$X1 <- as.factor(energy$X1)

for(i in 1:NROW(energy)) {
  energy$X2[i] <- ifelse(energy$X2[i] <= 600, 1, energy$X2[i])
  energy$X2[i] <- ifelse(energy$X2[i] > 600 & energy$X2[i] < 700, 2, energy$X2[i])
  energy$X2[i] <- ifelse(energy$X2[i] >= 700, 3, energy$X2[i])
  
}
energy$X2 <- as.factor(energy$X2)

energy$X3 <- factor(ifelse(energy$X3 >= 300, 1, 0))

energy$X4 <- factor(ifelse(energy$X4 >= 200, 1, 0))

energy$X5 <- factor(energy$X5, labels = c(0, 1))

energy$X6 <- factor(ifelse(energy$X6 >= 4, 1, 0))

energy$X7 <- factor(ifelse(energy$X7 >= 0.2, 1, 0))

energy$X8 <- factor(ifelse(energy$X8 <= 1, 1, ifelse(energy$X8 <= 3, 2, 3)))


energy$Y1 <- factor(ifelse(energy$Y1 >= median(energy$Y1), 1, 0), labels = c(paste(c("< ", as.character(median(energy$Y1))), collapse = ""),
                                            paste(c(">= ", as.character(median(energy$Y1))), collapse = "")))

energy$Y2 <- factor(ifelse(energy$Y2 >= median(energy$Y2), 1, 0), labels = c(paste(c("< ", as.character(median(energy$Y2))), collapse = ""),
                                                                             paste(c(">= ", as.character(median(energy$Y2))), collapse = "")))

# creo 2 dataset: energy1 (y1) e energy2 (y2).
energy1 <- energy[, c(9, 1:8)]
energy2 <- energy[, c(10, 1:8)]
colnames(energy1) <- c("answer", colnames(energy1)[-1])
colnames(energy2) <- c("answer", colnames(energy2)[-1])
rm(energy)



fertility <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/fertility_Diagnosis.txt", header = F)
for(i in 1:NCOL(fertility)) {
  print(table(fertility[, i]))
}
fertility$V1 <- factor(ifelse(fertility$V1 == -1, "winter", ifelse(fertility$V1 == -0.33, "spring", 
                       ifelse(fertility$V1 == 0.33, "summer", "fall"))), levels = c("winter", "spring", "summer", "fall"))


# 18 sta a 0.50, 36 a 1. divido in 3 intervalli, (18, 23), (24, 29) e (30, 36) --> (0.50, 0.64), (0.65, 0.81), (0.82, 1).
fertility$V2 <- factor(ifelse(fertility$V2 >= 0.82, 3, ifelse(fertility$V2 <= 0.64, 1, 2)), 
                                                              labels = c("1", "2", "3"))
for(i in c(3, 4, 5, 6, 8)) fertility[, i] <- as.factor(fertility[, i])

fertility$V7 <- factor(ifelse(fertility$V7 == 1, 3, ifelse(fertility$V7 == 0.8, 2, 1)), 
                       labels = c("1", "2", "3"))

# 1 sta a 0.06, 16 a 1. divido in 3 intervalli, (1, 5), (6, 10) e (11, 16) --> (0.06, 0.31), (0.32, 0.62), (0.63, 1).
fertility$V9 <- factor(ifelse(fertility$V9 > 0.62, 3, ifelse(fertility$V9 <= 0.31, 1, 2)), 
                       labels = c("1", "2", "3"))
fertility <- fertility[, c(10, 1:9)]
colnames(fertility) <- c("answer", colnames(fertility)[-1])




house_votes <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/house-votes-84.data", header = F)
for(i in 2:NCOL(house_votes)) house_votes[, i] <- as.numeric(house_votes[, i])

for(i in 1:NROW(house_votes)) {
  for(j in 2:NCOL(house_votes)) {
    if(house_votes[i, j] == 1) house_votes[i, j] <- NA
  }
}
house_votes <- data.frame(na.exclude(house_votes))
for(i in 2:NCOL(house_votes)) house_votes[, i] <- factor(house_votes[, i], labels = c("n", "y"))

colnames(house_votes) <- c("answer", colnames(house_votes)[-1])







indian_liver <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/Indian Liver Patient Dataset (ILPD).csv", header = F)
indian_liver <- data.frame(na.exclude(indian_liver))

indian_liver$V1 <- factor(ifelse(indian_liver$V1 <= 33, "young", 
                                 ifelse(indian_liver$V1 > 58, "old", "adult")), levels = c("young", "adult", "old"))

indian_liver$V3 <- factor(ifelse(indian_liver$V3 <= 1, "1", 
                                 ifelse(indian_liver$V3 > 3, "3", "2")), levels = c("1", "2", "3"))

indian_liver$V4 <- factor(ifelse(indian_liver$V4 <= 0.2, "1", 
                                 ifelse(indian_liver$V4 > 1.5, "3", "2")), levels = c("1", "2", "3"))

indian_liver$V5 <- factor(ifelse(indian_liver$V5 >= 200, 1, 0))
indian_liver$V6 <- factor(ifelse(indian_liver$V6 >= 40, 1, 0))
indian_liver$V7 <- factor(ifelse(indian_liver$V7 >= 40, 1, 0))

indian_liver$V8 <- factor(ifelse(indian_liver$V8 <= 5.8, "1", 
                                 ifelse(indian_liver$V8 > 7.2, "3", "2")), levels = c("1", "2", "3"))

indian_liver$V9 <- factor(ifelse(indian_liver$V9 >= 3.1, 1, 0))

indian_liver$V10 <- factor(ifelse(indian_liver$V10 <= 0.70, "1", 
                                 ifelse(indian_liver$V10 >= 1.1, "3", "2")), levels = c("1", "2", "3"))
indian_liver$V11 <- factor(indian_liver$V11, labels = c("liver", "not_liver"))
indian_liver <- indian_liver[, c(11, 1:10)]

colnames(indian_liver) <- c("answer", colnames(indian_liver)[-1])






iris <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/iris.data", header = F)
colnames(iris) <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "class")
iris$sepal_length <- factor(ifelse(iris$sepal_length <= 5.1, "short", 
                         ifelse(iris$sepal_length >= 6.4, "long", "medium")), levels = c("short", "medium", "long"))
iris$sepal_width <- factor(ifelse(iris$sepal_width <= 2.8, "short", 
                                   ifelse(iris$sepal_width >= 3.3, "long", "medium")), levels = c("short", "medium", "long"))
iris$petal_length <- factor(ifelse(iris$petal_length <= 1.6, "short", 
                                  ifelse(iris$petal_length >= 5.1, "long", "medium")), levels = c("short", "medium", "long"))
iris$petal_width <- factor(ifelse(iris$petal_width <= 0.3, "short", 
                                  ifelse(iris$petal_width >= 1.8, "long", "medium")), levels = c("short", "medium", "long"))
iris <- iris[, c(5, 1:4)]
colnames(iris) <- c("answer", colnames(iris)[-1])








magic04 <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/magic04.data", header = F)
for(k in 1:(NCOL(magic04) - 1)) {
    quant <- quantile(magic04[, k], probs = c(0.33, 0.66))
      magic04[, k] <- factor(ifelse(magic04[, k] <= quant[1], "low", 
                                     ifelse(magic04[, k] >= quant[2], "high", "medium")), levels = c("low", "medium", "high"))
}
magic04 <- magic04[, c(11, 1:10)]
colnames(magic04) <- c("answer", colnames(magic04)[-1])





# i train sono un sotto-campione del test. usiamo direttamente il test che è completo.

# monks1_train <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/monks-1.train", sep = " ", header = F)[, -1]
monks1_test <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/monks-1.test", sep = " ", header = F)[, -1]
# monks2_train <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/monks-2.train", sep = " ", header = F)[, -1]
monks2_test <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/monks-2.test", sep = " ", header = F)[, -1]
# monks3_train <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/monks-3.train", sep = " ", header = F)[, -1]
monks3_test <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/monks-3.test", sep = " ", header = F)[, -1]

monks1 <- monks1_test[, -NCOL(monks1_test)]
monks2 <- monks2_test[, -NCOL(monks2_test)]
monks3 <- monks3_test[, -NCOL(monks3_test)]
rm(monks1_test); rm(monks2_test); rm(monks3_test)


monks1$V2 <- factor(monks1$V2, levels = c("0", "1"))
monks1$V3 <- factor(monks1$V3)
monks1$V4 <- factor(monks1$V4)
monks1$V5 <- factor(monks1$V5)
monks1$V6 <- factor(monks1$V6)
monks1$V7 <- factor(monks1$V7)
monks1$V8 <- factor(monks1$V8)


monks2$V2 <- factor(monks2$V2, levels = c("0", "1"))
monks2$V3 <- factor(monks2$V3)
monks2$V4 <- factor(monks2$V4)
monks2$V5 <- factor(monks2$V5)
monks2$V6 <- factor(monks2$V6)
monks2$V7 <- factor(monks2$V7)
monks2$V8 <- factor(monks2$V8)


monks3$V2 <- factor(monks3$V2, levels = c("0", "1"))
monks3$V3 <- factor(monks3$V3)
monks3$V4 <- factor(monks3$V4)
monks3$V5 <- factor(monks3$V5)
monks3$V6 <- factor(monks3$V6)
monks3$V7 <- factor(monks3$V7)
monks3$V8 <- factor(monks3$V8)


colnames(monks1) <- c("answer", colnames(monks1)[-1])
colnames(monks2) <- c("answer", colnames(monks2)[-1])
colnames(monks3) <- c("answer", colnames(monks3)[-1])






nursery <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/nursery.txt", header = T)
nursery <- nursery[, c(9, 1:8)]
colnames(nursery) <- c("answer", colnames(nursery)[-1])




# page_block <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/page-blocks.data", sep = "\t", header = F)
post_operative <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/post-operative.data", header = F,
                           na.strings = "?")
post_operative <- data.frame(na.exclude(post_operative))

# c'erano 2 5 e 1 7, messi insieme a 10 per non creare classi con 1 o 2 osservazioni.
post_operative$V8[post_operative$V8 == 5 | post_operative$V8 == 7] <- 10 

post_operative$V8 <- factor(post_operative$V8, levels = c("10", "15"))

post_operative <- post_operative[, c(9, 1:8)]

post_operative$V9[as.numeric(post_operative$V9) == 2] <- "A"
post_operative$V9 <- factor(post_operative$V9)

colnames(post_operative) <- c("answer", colnames(post_operative)[-1])






# non viene caricato bene. tutte variabili continue. non usiamolo
# seed <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/seeds_dataset.txt", header = F, sep = "\t")


tae <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/tae.data", header = F)
tae$V1 <- factor(tae$V1, labels = c("english", "not english"))
tae$V2 <- factor(tae$V2)
tae$V3 <- factor(tae$V3)
tae$V4 <- factor(tae$V4, labels = c("summer", "regular"))
tae$V5 <- factor(ifelse(tae$V5 <= 20, "small", 
                                   ifelse(tae$V5 >= 30, "big", "medium")), levels = c("small", "medium", "big"))
tae$V6 <- factor(tae$V6, labels = c("low", "medium", "high"))
tae <- tae[, c(6, 1:5)]
colnames(tae) <- c("answer", colnames(tae)[-1])



tic_tac_toe <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/tic-tac-toe.data", header = F)
tic_tac_toe <- tic_tac_toe[, c(10, 1:9)]
colnames(tic_tac_toe) <- c("answer", colnames(tic_tac_toe)[-1])







# Asym 
Asym <- read.csv("/Users/federico/Dropbox/Software Paper/final/Asym.csv")
for(i in 1:NCOL(Asym)) Asym[, i] <- factor(Asym[, i], labels = c("0", "1"))
Asym <- Asym[, c(4, 1:3)]
colnames(Asym) <- c("answer", colnames(Asym)[-1])

# chds 
chds <- read.csv("/Users/federico/Dropbox/Software Paper/final/chds.csv")
chds <- chds[, c(4, 1:3)]
colnames(chds) <- c("answer", colnames(chds)[-1])

# selfy 
selfy <- read.csv("/Users/federico/Dropbox/Software Paper/final/selfy.csv")
selfy <- selfy[, c(4, 1:3)]
colnames(selfy) <- c("answer", colnames(selfy)[-1])

# FallEld 
FallEld <- read.csv("/Users/federico/Dropbox/Software Paper/final/FallEld.csv")
FallEld <- FallEld[, c(4, 1:3)]
colnames(FallEld) <- c("answer", colnames(FallEld)[-1])





#######   datasets downloadable directly in R   ####### 

# chestSim500 
data("chestSim500", package = "gRbase")
chestSim500 <- chestSim500[, c(8, 1:7)]
colnames(chestSim500) <- c("answer", colnames(chestSim500)[-1])

# PhDArticles dataset with variable Articles in last column
data("PhDArticles", package = "stagedtrees")
colnames(PhDArticles) <- c("answer", colnames(PhDArticles)[-1])

# Pokemon 
data("Pokemon", package = "stagedtrees")
Pokemon <- Pokemon[, c(5, 1:4)]
colnames(Pokemon) <- c("answer", colnames(Pokemon)[-1])

# puffin dataset with variable class in last column
data("puffin", package = "MBCbook")
puffin$class <- as.factor(puffin$class)
colnames(puffin) <- c("answer", colnames(puffin)[-1])

# reinis 
data(reinis, package = "gRbase")
reinis <- as.data.frame(reinis)
reinis <- reinis[rep(row.names(reinis), reinis$Freq), 1:6]
reinis <- reinis[, c(6, 1:5)]
colnames(reinis) <- c("answer", colnames(reinis)[-1])

# Titanic
data(Titanic, package = "datasets")
Titanic <- as.data.frame(Titanic)
Titanic <- Titanic[rep(row.names(Titanic), Titanic$Freq), 1:4]
Titanic <- Titanic[, c(4, 1:3)]
colnames(Titanic) <- c("answer", colnames(Titanic)[-1])

rm(i); rm(j); rm(k); rm(m); rm(quant)





#########  FUNCTION TO ANALYZE MODELS  ######### 

load("~/Dropbox/Gherardo:Manuele/Classification Paper/classification_paper_code.RData")

all_datasets <- list(abalone, Asym, breast_cancer, breast_cancer_wisconsin, car,chds, 
                     chestSim500, energy1, energy2, FallEld, fertility, house_votes, indian_liver, iris,                   
                     magic04, monks1, monks2, monks3, nursery, PhDArticles, Pokemon, post_operative,
                     puffin, reinis, selfy, tae, tic_tac_toe, Titanic)
attr(all_datasets, "names") <- ls()[-2]
# categories_answer <- sapply(all_datasets, function(x) length(levels(x[, 1])))
binary_datasets <- all_datasets[-c(1, 5, 14, 19, 20, 22, 25, 26)]  # 20 binary datasets



#############################  MODELS  #############################  



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



set.seed(123) 
for(d in c(1, 3, 4, 5)) { # length(binary_datasets)
    for(i in 1:2)  # 10
    {
      # print(d)
      # print(i)
      id_train <- sample(1:NROW(binary_datasets[[d]]), NROW(results[[d]][[4]]), replace = FALSE)
      id_test <- sample(setdiff(1:NROW(binary_datasets[[d]]), id_train), NROW(results[[d]][[5]]), replace = FALSE)
      id_val <- setdiff(1:NROW(binary_datasets[[d]]), c(id_train, id_test))
      train <- binary_datasets[[d]][id_train, ]
      test <- binary_datasets[[d]][id_test, ]
      validation <- binary_datasets[[d]][id_val, ]
     
      
      ####### stagedtrees models  ####### 
      
      # train models
      time_full <- system.time(m_full <- full(train, lambda = 0.5))[1] # FULL
      time_indep <- system.time(m_indep <- indep(train, lambda = 0.5))[1] # INDEP
      m_full <- join_zero_counts(m_full, name = "NA")
      m_indep <- join_zero_counts(m_indep, name = "NA")
      
      time_m1 <- system.time(m1 <- hc.sevt(m_indep))[1] # HC - INDEP 
      
      time_m2 <- system.time(m2 <- fbhc.sevt(m_full, max_iter = Inf))[1] # FAST BHC
      
      time_m3 <- system.time(m3 <- bj.sevt(m_full, distance = kl, thr = 0.2))[1] # KL - 0.20
      
      time_m4 <- system.time(m4 <- bj.sevt(m_full, distance = tv, thr = 0.2))[1] # TV - 0.20
      
      time_m5 <- system.time(m5 <- bj.sevt(m_full, distance = cd, thr = 0.2))[1] # CD - 0.20
      
      time_m6 <- system.time(m6 <- naive.sevt(m_full, distance = kl))[1] # NAIVE TREE
      
      
      # test models 
      prob_m_full <- predict(m_full, newdata = test, class = "answer", prob = TRUE) # only for the best cutoff. Then we use the validation
      m_full_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m_full[, 2], optimiseFor = "Both")
      prob_m_full <- predict(m_full, newdata = validation, class = "answer", prob = TRUE) # sovrascrivo quello del test, non mi serve più
      pred_m_full <- factor(ifelse(prob_m_full[, 2] >= m_full_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                   levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      
      prob_m_indep <- predict(m_indep, newdata = test, class = "answer", prob = TRUE)
      m_indep_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m_indep[, 2], optimiseFor = "Both")
      prob_m_indep <- predict(m_indep, newdata = validation, class = "answer", prob = TRUE)
      pred_m_indep <- factor(ifelse(prob_m_indep[, 2] >= m_indep_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                    levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      
      prob_m1 <- predict(m1, newdata = test, class = "answer", prob = TRUE)
      m1_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m1[, 2], optimiseFor = "Both")
      prob_m1 <- predict(m1, newdata = validation, class = "answer", prob = TRUE)
      pred_m1 <- factor(ifelse(prob_m1[, 2] >= m1_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                               levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      
      prob_m2 <- predict(m2, newdata = test, class = "answer", prob = TRUE)
      m2_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m2[, 2], optimiseFor = "Both")
      prob_m2 <- predict(m2, newdata = validation, class = "answer", prob = TRUE)
      pred_m2 <- factor(ifelse(prob_m2[, 2] >= m2_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                               levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      
      prob_m3 <- predict(m3, newdata = test, class = "answer", prob = TRUE)
      m3_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m3[, 2], optimiseFor = "Both")
      prob_m3 <- predict(m3, newdata = validation, class = "answer", prob = TRUE)
      pred_m3 <- factor(ifelse(prob_m3[, 2] >= m3_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                               levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      
      prob_m4 <- predict(m4, newdata = test, class = "answer", prob = TRUE)
      m4_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m4[, 2], optimiseFor = "Both")
      prob_m4 <- predict(m4, newdata = validation, class = "answer", prob = TRUE)
      pred_m4 <- factor(ifelse(prob_m4[, 2] >= m4_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                               levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      
      prob_m5 <- predict(m5, newdata = test, class = "answer", prob = TRUE)
      m5_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m5[, 2], optimiseFor = "Both")
      prob_m5 <- predict(m5, newdata = validation, class = "answer", prob = TRUE)
      pred_m5 <- factor(ifelse(prob_m5[, 2] >= m5_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                               levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      
      prob_m6 <- predict(m6, newdata = test, class = "answer", prob = TRUE)
      m6_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m6[, 2], optimiseFor = "Both")
      prob_m6 <- predict(m6, newdata = validation, class = "answer", prob = TRUE)
      pred_m6 <- factor(ifelse(prob_m6[, 2] >= m6_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                               levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      
      
      acc_full <- sum(diag(table(pred_m_full, validation$answer))) / NROW(validation)
      acc_indep <- sum(diag(table(pred_m_indep, validation$answer))) / NROW(validation)
      acc_m1 <- sum(diag(table(pred_m1, validation$answer))) / NROW(validation)
      acc_m2 <- sum(diag(table(pred_m2, validation$answer))) / NROW(validation)
      acc_m3 <- sum(diag(table(pred_m3, validation$answer))) / NROW(validation)
      acc_m4 <- sum(diag(table(pred_m4, validation$answer))) / NROW(validation)
      acc_m5 <- sum(diag(table(pred_m5, validation$answer))) / NROW(validation)
      acc_m6 <- sum(diag(table(pred_m6, validation$answer))) / NROW(validation)
      
      
      ####### literature algorithms  ####### 
      
      # bnlearn hc
      time_m11 <- system.time(m11 <- bnlearn::hc(train))[1]
      m11 <- bn.fit(m11, train)
      prob_m11 <- t(attr(predict(m11, node = "answer", data = test, prob = TRUE), "prob"))
      attr(prob_m11, "dimnames") <- NULL
      m11_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m11[, 2], optimiseFor = "Both")
      prob_m11 <- t(attr(predict(m11, node = "answer", data = validation, prob = TRUE), "prob"))
      attr(prob_m11, "dimnames") <- NULL
      pred_m11 <- factor(ifelse(prob_m11[, 2] >= m11_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m11 <- sum(diag(table(pred_m11, validation$answer))) / NROW(validation)

      # bnlearn tabu
      time_m12 <- system.time(m12 <- tabu(train))[1]
      m12 <- bn.fit(m12, train)
      prob_m12 <- t(attr(predict(m12, node = "answer", data = test, prob = TRUE), "prob"))
      attr(prob_m12, "dimnames") <- NULL
      m12_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m12[, 2], optimiseFor = "Both")
      prob_m12 <- t(attr(predict(m12, node = "answer", data = validation, prob = TRUE), "prob"))
      attr(prob_m12, "dimnames") <- NULL
      pred_m12 <- factor(ifelse(prob_m12[, 2] >= m12_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m12 <- sum(diag(table(pred_m12, validation$answer))) / NROW(validation)

      # naive bayes
      time_m13 <- system.time(m13 <- naiveBayes(x = train[, -1], y = train$answer))[1]
      prob_m13 <- predict(m13, newdata = test, type = "raw")
      m13_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m13[, 2], optimiseFor = "Both")
      prob_m13 <- predict(m13, newdata = validation, type = "raw")
      pred_m13 <- factor(ifelse(prob_m13[, 2] >= m13_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m13 <- sum(diag(table(pred_m13, validation$answer))) / NROW(validation)

      # logistic model
      time_m14 <- system.time(m14 <- multinom(answer ~ ., data = train))[1]
      prob_m14 <- predict(m14, newdata = test, type = "probs")
      m14_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m14, optimiseFor = "Both")
      prob_m14 <- predict(m14, newdata = validation, type = "probs")
      pred_m14 <- factor(ifelse(prob_m14 >= m14_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m14 <- sum(diag(table(pred_m14, validation$answer))) / NROW(validation)

      # neural network
      time_m15 <- system.time(m15 <- nnet(answer ~ ., data = train, size = 10, 
                                          decay = 0.001, maxit = 5000, linout = FALSE, MaxNWts = 5000))[1]
      prob_m15 <- predict(m15, newdata = test, type = "raw")
      m15_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m15, optimiseFor = "Both")
      prob_m15 <- predict(m15, newdata = validation, type = "raw")
      pred_m15 <- factor(ifelse(prob_m15 >= m15_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m15 <- sum(diag(table(pred_m15, validation$answer))) / NROW(validation)
      
      time_m16 <- system.time(m16 <- nnet(answer ~ ., data = train, size = 20, 
                                          decay = 0.01, maxit = 5000, linout = FALSE, MaxNWts = 5000))[1]
      prob_m16 <- predict(m16, newdata = test, type = "raw")
      m16_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m16, optimiseFor = "Both")
      prob_m16 <- predict(m16, newdata = validation, type = "raw")
      pred_m16 <- factor(ifelse(prob_m16 >= m16_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m16 <- sum(diag(table(pred_m16, validation$answer))) / NROW(validation)

      
      # classification tree
      time_m17 <- system.time(m17 <- rpart(answer ~ ., data = train, method = "class", cp = 0.0))[1]
      prob_m17 <- predict(m17, newdata = test, type = "prob")
      m17_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m17[, 2], optimiseFor = "Both")
      prob_m17 <- predict(m17, newdata = validation, type = "prob")
      pred_m17 <- factor(ifelse(prob_m17[, 2] >= m17_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m17 <- sum(diag(table(pred_m17, validation$answer))) / NROW(validation)
      
      time_m18 <- system.time(m18 <- rpart(answer ~ ., data = train, method = "class", cp = 0.05))[1]
      prob_m18 <- predict(m18, newdata = test, type = "prob")
      m18_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m18[, 2], optimiseFor = "Both")
      prob_m18 <- predict(m18, newdata = validation, type = "prob")
      pred_m18 <- factor(ifelse(prob_m18[, 2] >= m18_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m18 <- sum(diag(table(pred_m18, validation$answer))) / NROW(validation)
      
      
      # random forest
      time_m19 <- system.time(m19  <- randomForest(answer ~ ., data = train, nodesize = 1, ntree = 100, 
                                                   mtry = round(sqrt(NCOL(train)))))[1]
      prob_m19 <- predict(m19, newdata = test, type = "prob")
      m19_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m19[, 2], optimiseFor = "Both")
      prob_m19 <- predict(m19, newdata = validation, type = "prob")
      pred_m19 <- factor(ifelse(prob_m19[, 2] >= m19_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m19 <- sum(diag(table(pred_m19, validation$answer))) / NROW(validation)
      
      
      time_m20 <- system.time(m20  <- randomForest(answer ~ ., data = train, nodesize = 1, ntree = 300, 
                                                   mtry = round(sqrt(NCOL(train)))))[1]
      prob_m20 <- predict(m20, newdata = test, type = "prob")
      m20_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m20[, 2], optimiseFor = "Both")
      prob_m20 <- predict(m20, newdata = validation, type = "prob")
      pred_m20 <- factor(ifelse(prob_m20[, 2] >= m20_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m20 <- sum(diag(table(pred_m20, validation$answer))) / NROW(validation)
      
      ## Paper "Do we need hundreds of algorithms ...." algorithms
      x <- train
      for(l in 1:NCOL(train)) x[, l] <- as.numeric(x[, l])
      y <- test
      for(l in 1:NCOL(test)) y[, l] <- as.numeric(y[, l])
      z <- validation
      for(l in 1:NCOL(validation)) z[, l] <- as.numeric(z[, l])
      
      
      time_m21 <- system.time(m21 <- lda(answer ~ ., data = train))[1]
      prob_m21 <- predict(m21, newdata = test)$posterior
      m21_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m21[, 2], optimiseFor = "Both")
      prob_m21 <- predict(m21, newdata = validation)$posterior
      pred_m21 <- factor(ifelse(prob_m21[, 2] >= m21_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m21 <- sum(diag(table(pred_m21, validation$answer))) / NROW(validation)

      time_m22 <- system.time(m22 <- fda(answer ~ ., data = train))[1]
      prob_m22 <- predict(m22, newdata = test, type = "posterior")
      m22_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m22[, 2], optimiseFor = "Both")
      prob_m22 <- predict(m22, newdata = validation, type = "posterior")
      pred_m22 <- factor(ifelse(prob_m22[, 2] >= m22_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m22 <- sum(diag(table(pred_m22, validation$answer))) / NROW(validation)

      time_m23 <- system.time(m23 <- mda(answer ~ ., data = train))[1]
      prob_m23 <- predict(m23, newdata = test, type = "posterior")
      m23_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m23[, 2], optimiseFor = "Both")
      prob_m23 <- predict(m23, newdata = validation, type = "posterior")
      pred_m23 <- factor(ifelse(prob_m23[, 2] >= m23_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m23 <- sum(diag(table(pred_m23, validation$answer))) / NROW(validation)

      time_m24 <- system.time(m24 <- rda(answer ~ ., data = train))[1]
      prob_m24 <- predict(m24, newdata = test)$posterior
      m24_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m24[, 2], optimiseFor = "Both")
      prob_m24 <- predict(m24, newdata = validation)$posterior
      pred_m24 <- factor(ifelse(prob_m24[, 2] >= m24_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m24 <- sum(diag(table(pred_m24, validation$answer))) / NROW(validation)

      time_m25 <- system.time(m25 <- hdda(cls = x[, 1], model = "ABQD", data = x[, -1]))[1]
      prob_m25 <- predict(m25, data = y[, -1])$posterior
      m25_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m25[, 2], optimiseFor = "Both")
      prob_m25 <- predict(m25, data = z[, -1])$posterior
      pred_m25 <- factor(ifelse(prob_m25[, 2] >= m25_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m25 <- sum(diag(table(pred_m25, validation$answer))) / NROW(validation)

      time_m26 <- system.time(m26 <- klaR::NaiveBayes(answer ~ ., data = train))[1]
      prob_m26 <- predict(m26, newdata = test)$posterior
      m26_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m26[, 2], optimiseFor = "Both")
      prob_m26 <- predict(m26, newdata = validation)$posterior
      pred_m26 <- factor(ifelse(prob_m26[, 2] >= m26_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m26 <- sum(diag(table(pred_m26, validation$answer))) / NROW(validation)

      time_m27 <- system.time(m27 <- boosting(answer ~ ., data = train, mfinal = 20))[1]
      prob_m27 <- predict(m27, newdata = test)$prob
      m27_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m27[, 2], optimiseFor = "Both")
      prob_m27 <- predict(m27, newdata = validation)$prob
      pred_m27 <- factor(ifelse(prob_m27[, 2] >= m27_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m27 <- sum(diag(table(pred_m27, validation$answer))) / NROW(validation)

      time_m28 <- system.time(m28 <- LogitBoost(xlearn = x[, -1], ylearn = train$answer))[1]
      prob_m28 <- predict(m28, xtest = y[, -1], type = "raw")
      m28_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m28[, 2], optimiseFor = "Both")
      prob_m28 <- predict(m28, xtest = z[, -1], type = "raw")
      pred_m28 <- factor(ifelse(prob_m28[, 2] >= m28_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer)) 
      acc_m28 <- sum(diag(table(pred_m28, validation$answer))) / NROW(validation)

      time_m29 <- system.time(m29 <- bagging(answer ~ ., data = train))[1]
      prob_m29 <- predict(m29, newdata = test, type = "prob")
      m29_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m29[, 2], optimiseFor = "Both")
      prob_m29 <- predict(m29, newdata = validation, type = "prob")
      pred_m29 <- factor(ifelse(prob_m29[, 2] >= m29_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m29 <- sum(diag(table(pred_m29, validation$answer))) / NROW(validation)

      
      
      time_m30 <- system.time(m30 <- glm(answer ~ ., data = train, family = binomial))[1]
      prob_m30 <- predict(m30, newdata = test, type = "response")
      m30_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m30, optimiseFor = "Both")
      prob_m30 <- predict(m30, newdata = validation, type = "response")
      pred_m30 <- factor(ifelse(prob_m30 >= m30_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m30 <- sum(diag(table(pred_m30, validation$answer))) / NROW(validation)

      time_m31 <- system.time(m31 <- knn(x[, -1], y[, -1], cl = x$answer, k = 10, prob = T))[1]
      prob_m31 <- attr(m31, "prob")
      m31_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m31, optimiseFor = "Both")
      m31 <- knn(x[, -1], z[, -1], cl = x$answer, k = 10, prob = T)
      prob_m31 <- attr(m31, "prob")
      pred_m31 <- factor(ifelse(prob_m31 >= m31_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m31 <- sum(diag(table(pred_m31, validation$answer))) / NROW(validation)

      # NON POSSIAMO AVERE LE PROBABILITA'. TRATTA TUTTO COME VARIABILI CONTINUE.
      time_m32 <- system.time(m32 <- spls(x = x[, -1], y = x$answer, K = 2, eta = 0.5))[1]
      pred_m32 <- predict(m32, newx = z[, -1]) # poi da categorizzare, predice valori continui
      pred_m32 <- factor(ifelse(pred_m32 >= 1.5, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m32 <- sum(diag(table(pred_m32, validation$answer))) / NROW(validation)
      
      # NON POSSIAMO AVERE LE PROBABILITA'. TRATTA TUTTO COME VARIABILI CONTINUE.
      time_m33 <- system.time(m33 <- plsr(answer ~ ., data = x))[1]
      pred_m33 <- predict(m33, newdata = z, type = "response") # poi da categorizzare, predice valori continui
      pred_m33 <- factor(ifelse(pred_m33[, , 2] >= 1.5, levels(binary_datasets[[d]]$answer)[2], levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m33 <- sum(diag(table(pred_m33, validation$answer))) / NROW(validation)
      
      # NON POSSIAMO AVERE LE PROBABILITA'. TRATTA TUTTO COME VARIABILI CONTINUE.
      time_m34 <- system.time(m34 <- mars(x = x[, -1], y = x[, 1]))[1]
      pred_m34 <- predict(m34, newdata = z[, -1])
      pred_m34 <- factor(ifelse(pred_m34 >= 1.5, levels(binary_datasets[[d]]$answer)[2], levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m34 <- sum(diag(table(pred_m34, validation$answer))) / NROW(validation)
      
      # NON POSSIAMO AVERE LE PROBABILITA'. TRATTA TUTTO COME VARIABILI CONTINUE.
      time_m35 <- system.time(m35 <- lm(as.numeric(answer) ~ ., data = train))[1]
      pred_m35 <- predict(m35, newdata = validation)
      pred_m35 <- factor(ifelse(pred_m35 >= 1.5, levels(binary_datasets[[d]]$answer)[2], levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m35 <- sum(diag(table(pred_m35, validation$answer))) / NROW(validation)
      
      time_m36 <- system.time(m36 <- svm(answer ~ ., data = train, probability = TRUE))[1]
      prob_m36 <- attr(predict(m36, newdata = test, probability = T), "probabilities")
      prob_m36 <- prob_m36[, attr(prob_m36, "dimnames")[[2]] == levels(binary_datasets[[d]]$answer)[2]]
      m36_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m36, optimiseFor = "Both")
      prob_m36 <- attr(predict(m36, newdata = validation, probability = T), "probabilities")
      prob_m36 <- prob_m36[, attr(prob_m36, "dimnames")[[2]] == levels(binary_datasets[[d]]$answer)[2]]
      pred_m36 <- factor(ifelse(prob_m36 >= m36_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m36 <- sum(diag(table(pred_m36, validation$answer))) / NROW(validation)
      
      
      time_m37 <- system.time(m37 <- gam(answer ~ ., data = train, family = binomial))[1]
      prob_m37 <- predict(m37, newdata = test, type = "response")
      m37_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m37, optimiseFor = "Both")
      prob_m37 <- predict(m37, newdata = validation, type = "response")
      pred_m37 <- factor(ifelse(prob_m37 >= m37_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m37 <- sum(diag(table(pred_m37, validation$answer))) / NROW(validation)

      # NON POSSIAMO AVERE LE PROBABILITA'. TRATTA TUTTO COME VARIABILI CONTINUE.
      time_m38 <- system.time(m38 <- polymars(responses = train$answer, predictors = train[, -1]))[1]
      pred_m38 <- predict(m38, x = validation[, -1])
      pred_m38 <- factor(ifelse(pred_m38 >= 1.5, levels(binary_datasets[[d]]$answer)[2], levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m38 <- sum(diag(table(pred_m38, validation$answer))) / NROW(validation)
      
      time_m39 <- system.time(m39 <- ada(answer ~ ., data = train, iter = 100))[1]
      prob_m39 <- predict(m39, newdata = test, type = "prob")
      m39_cutoff <- optimalCutoff(as.numeric(test$answer) - 1, prob_m39[, 2], optimiseFor = "Both")
      prob_m39 <- predict(m39, newdata = validation, type = "prob")
      pred_m39 <- factor(ifelse(prob_m39[, 2] >= m39_cutoff, levels(binary_datasets[[d]]$answer)[2], 
                                levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
      acc_m39 <- sum(diag(table(pred_m39, validation$answer))) / NROW(validation)
      
      
      ### ROC CURVE
      roc_full <- roc(response = validation$answer, predictor = prob_m_full[, 2], auc = TRUE, plot = FALSE)
      roc_indep <- roc(response = validation$answer, predictor = prob_m_indep[, 2], auc = TRUE, plot = FALSE)
      roc_m1 <- roc(response = validation$answer, predictor = prob_m1[, 2], auc = TRUE, plot = FALSE)
      roc_m2 <- roc(response = validation$answer, predictor = prob_m2[, 2], auc = TRUE, plot = FALSE)
      roc_m3 <- roc(response = validation$answer, predictor = prob_m3[, 2], auc = TRUE, plot = FALSE)
      roc_m4 <- roc(response = validation$answer, predictor = prob_m4[, 2], auc = TRUE, plot = FALSE)
      roc_m5 <- roc(response = validation$answer, predictor = prob_m5[, 2], auc = TRUE, plot = FALSE)
      roc_m6 <- roc(response = validation$answer, predictor = prob_m6[, 2], auc = TRUE, plot = FALSE)
      roc_m11 <- roc(response = validation$answer, predictor = prob_m11[, 2], auc = TRUE, plot = FALSE)
      roc_m12 <- roc(response = validation$answer, predictor = prob_m12[, 2], auc = TRUE, plot = FALSE)
      roc_m13 <- roc(response = validation$answer, predictor = prob_m13[, 2], auc = TRUE, plot = FALSE)
      roc_m14 <- roc(response = validation$answer, predictor = prob_m14, auc = TRUE, plot = FALSE)
      roc_m15 <- roc(response = validation$answer, predictor = as.vector(prob_m15), auc = TRUE, plot = FALSE)
      roc_m16 <- roc(response = validation$answer, predictor = as.vector(prob_m16), auc = TRUE, plot = FALSE)
      roc_m17 <- roc(response = validation$answer, predictor = prob_m17[, 2], auc = TRUE, plot = FALSE)
      roc_m18 <- roc(response = validation$answer, predictor = prob_m18[, 2], auc = TRUE, plot = FALSE)
      roc_m19 <- roc(response = validation$answer, predictor = prob_m19[, 2], auc = TRUE, plot = FALSE)
      roc_m20 <- roc(response = validation$answer, predictor = prob_m20[, 2], auc = TRUE, plot = FALSE)
      roc_m21 <- roc(response = validation$answer, predictor = prob_m21[, 2], auc = TRUE, plot = FALSE)
      roc_m22 <- roc(response = validation$answer, predictor = prob_m22[, 2], auc = TRUE, plot = FALSE)
      roc_m23 <- roc(response = validation$answer, predictor = prob_m23[, 2], auc = TRUE, plot = FALSE)
      roc_m24 <- roc(response = validation$answer, predictor = prob_m24[, 2], auc = TRUE, plot = FALSE)
      roc_m25 <- roc(response = validation$answer, predictor = prob_m25[, 2], auc = TRUE, plot = FALSE)
      roc_m26 <- roc(response = validation$answer, predictor = prob_m26[, 2], auc = TRUE, plot = FALSE)
      roc_m27 <- roc(response = validation$answer, predictor = prob_m27[, 2], auc = TRUE, plot = FALSE)
      roc_m28 <- roc(response = validation$answer, predictor = prob_m28[, 2], auc = TRUE, plot = FALSE)
      roc_m29 <- roc(response = validation$answer, predictor = prob_m29[, 2], auc = TRUE, plot = FALSE)
      roc_m30 <- roc(response = validation$answer, predictor = prob_m30, auc = TRUE, plot = FALSE)
      roc_m31 <- roc(response = validation$answer, predictor = prob_m31, auc = TRUE, plot = FALSE)
      roc_m36 <- roc(response = validation$answer, predictor = prob_m36, auc = TRUE, plot = FALSE)
      roc_m37 <- roc(response = validation$answer, predictor = prob_m37, auc = TRUE, plot = FALSE)
      roc_m39 <- roc(response = validation$answer, predictor = prob_m39[, 2], auc = TRUE, plot = FALSE)
      
      ### CONFUSION MATRIX
      confusion_full <- table(pred_m_full, validation$answer)
      confusion_indep <- table(pred_m_indep, validation$answer)
      confusion_m1 <- table(pred_m1, validation$answer)
      confusion_m2 <- table(pred_m2, validation$answer)
      confusion_m3 <- table(pred_m3, validation$answer)
      confusion_m4 <- table(pred_m4, validation$answer)
      confusion_m5 <- table(pred_m5, validation$answer)
      confusion_m6 <- table(pred_m6, validation$answer)
      confusion_m11 <- table(pred_m11, validation$answer)
      confusion_m12 <- table(pred_m12, validation$answer)
      confusion_m13 <- table(pred_m13, validation$answer)
      confusion_m14 <- table(pred_m14, validation$answer)
      confusion_m15 <- table(pred_m15, validation$answer)
      confusion_m16 <- table(pred_m16, validation$answer)
      confusion_m17 <- table(pred_m17, validation$answer)
      confusion_m18 <- table(pred_m18, validation$answer)
      confusion_m19 <- table(pred_m19, validation$answer)
      confusion_m20 <- table(pred_m20, validation$answer)
      confusion_m21 <- table(pred_m21, validation$answer)
      confusion_m22 <- table(pred_m22, validation$answer)
      confusion_m23 <- table(pred_m23, validation$answer)
      confusion_m24 <- table(pred_m24, validation$answer)
      confusion_m25 <- table(pred_m25, validation$answer)
      confusion_m26 <- table(pred_m26, validation$answer)
      confusion_m27 <- table(pred_m27, validation$answer)
      confusion_m28 <- table(pred_m28, validation$answer)
      confusion_m29 <- table(pred_m29, validation$answer)
      confusion_m30 <- table(pred_m30, validation$answer)
      confusion_m31 <- table(pred_m31, validation$answer)
      confusion_m32 <- table(pred_m32, validation$answer)
      confusion_m33 <- table(pred_m33, validation$answer)
      confusion_m34 <- table(pred_m34, validation$answer)
      confusion_m35 <- table(pred_m35, validation$answer)
      confusion_m36 <- table(pred_m36, validation$answer)
      confusion_m37 <- table(pred_m37, validation$answer)
      confusion_m38 <- table(pred_m38, validation$answer)
      confusion_m39 <- table(pred_m39, validation$answer)
      
      ### ACCURACY
      accuracy <- c(acc_full, acc_indep, acc_m1, acc_m2, acc_m3, acc_m4, acc_m5, acc_m6,
                         acc_m11, acc_m12, acc_m13, acc_m14, acc_m15, acc_m16, acc_m17, acc_m18,
                         acc_m19, acc_m20, acc_m21, acc_m22, acc_m23, acc_m24, acc_m25, acc_m26,
                         acc_m27, acc_m28, acc_m29, acc_m30, acc_m31, acc_m32, acc_m33, acc_m34,
                         acc_m35, acc_m36, acc_m37, acc_m38, acc_m39)
      
      ### ERROR
      error <- numeric(37)
      for(e in 1:length(error)) error[e] <- 1 - accuracy[e]

      
      ### AUC
      auc_models <- c(roc_full$auc, roc_indep$auc, roc_m1$auc, roc_m2$auc, roc_m3$auc, roc_m4$auc, roc_m5$auc, roc_m6$auc,
                      roc_m11$auc, roc_m12$auc, roc_m13$auc, roc_m14$auc, roc_m15$auc, roc_m16$auc, roc_m17$auc, roc_m18$auc,
                      roc_m19$auc, roc_m20$auc, roc_m21$auc, roc_m22$auc, roc_m23$auc, roc_m24$auc, roc_m25$auc, roc_m26$auc,
                      roc_m27$auc, roc_m28$auc, roc_m29$auc, roc_m30$auc, roc_m31$auc, NA, NA, NA, NA, roc_m36$auc, roc_m37$auc, NA, roc_m39$auc)
      
      ### THRESHOLD MODELS
      
      thr <- c(m_full_cutoff, m_indep_cutoff, m1_cutoff, m2_cutoff, m3_cutoff, m4_cutoff, m5_cutoff, m6_cutoff, m11_cutoff,
               m12_cutoff, m13_cutoff, m14_cutoff, m15_cutoff, m16_cutoff, m17_cutoff, m18_cutoff, m19_cutoff, m20_cutoff, m21_cutoff,
               m22_cutoff, m23_cutoff, m24_cutoff, m25_cutoff, m26_cutoff, m27_cutoff, m28_cutoff, m29_cutoff, m30_cutoff, m31_cutoff,
               NA, NA, NA, NA, m36_cutoff, m37_cutoff, NA, m39_cutoff)
      
      
      #### saving results for dataset d and bootstrap iteration i
      
      results[[d]]$computational_time[, i] <- c(time_full, time_indep, time_m1, time_m2, time_m3, time_m4, time_m5,
                                                time_m6, time_m11, time_m12, time_m13, time_m14, time_m15, time_m16,
                                                time_m17, time_m18, time_m19, time_m20, time_m21, time_m22, time_m23,
                                                time_m24, time_m25, time_m26, time_m27, time_m28, time_m29, time_m30,
                                                time_m31, time_m32, time_m33, time_m34, time_m35, time_m36, time_m37,
                                                time_m38, time_m39)
      
      
      results[[d]]$summary[[i]][1, 1:4] <- as.vector(t(confusion_full))
      results[[d]]$summary[[i]][2, 1:4] <- as.vector(t(confusion_indep))
      results[[d]]$summary[[i]][3, 1:4] <- as.vector(t(confusion_m1))
      results[[d]]$summary[[i]][4, 1:4] <- as.vector(t(confusion_m2))
      results[[d]]$summary[[i]][5, 1:4] <- as.vector(t(confusion_m3))
      results[[d]]$summary[[i]][6, 1:4] <- as.vector(t(confusion_m4))
      results[[d]]$summary[[i]][7, 1:4] <- as.vector(t(confusion_m5))
      results[[d]]$summary[[i]][8, 1:4] <- as.vector(t(confusion_m6))
      results[[d]]$summary[[i]][9, 1:4] <- as.vector(t(confusion_m11))
      results[[d]]$summary[[i]][10, 1:4] <- as.vector(t(confusion_m12))
      results[[d]]$summary[[i]][11, 1:4] <- as.vector(t(confusion_m13))
      results[[d]]$summary[[i]][12, 1:4] <- as.vector(t(confusion_m14))
      results[[d]]$summary[[i]][13, 1:4] <- as.vector(t(confusion_m15))
      results[[d]]$summary[[i]][14, 1:4] <- as.vector(t(confusion_m16))
      results[[d]]$summary[[i]][15, 1:4] <- as.vector(t(confusion_m17))
      results[[d]]$summary[[i]][16, 1:4] <- as.vector(t(confusion_m18))
      results[[d]]$summary[[i]][17, 1:4] <- as.vector(t(confusion_m19))
      results[[d]]$summary[[i]][18, 1:4] <- as.vector(t(confusion_m20))
      results[[d]]$summary[[i]][19, 1:4] <- as.vector(t(confusion_m21))
      results[[d]]$summary[[i]][20, 1:4] <- as.vector(t(confusion_m22))
      results[[d]]$summary[[i]][21, 1:4] <- as.vector(t(confusion_m23))
      results[[d]]$summary[[i]][22, 1:4] <- as.vector(t(confusion_m24))
      results[[d]]$summary[[i]][23, 1:4] <- as.vector(t(confusion_m25))
      results[[d]]$summary[[i]][24, 1:4] <- as.vector(t(confusion_m26))
      results[[d]]$summary[[i]][25, 1:4] <- as.vector(t(confusion_m27))
      results[[d]]$summary[[i]][26, 1:4] <- as.vector(t(confusion_m28))
      results[[d]]$summary[[i]][27, 1:4] <- as.vector(t(confusion_m29))
      results[[d]]$summary[[i]][28, 1:4] <- as.vector(t(confusion_m30))
      results[[d]]$summary[[i]][29, 1:4] <- as.vector(t(confusion_m31))
      results[[d]]$summary[[i]][30, 1:4] <- as.vector(t(confusion_m32))
      results[[d]]$summary[[i]][31, 1:4] <- as.vector(t(confusion_m33))
      results[[d]]$summary[[i]][32, 1:4] <- as.vector(t(confusion_m34))
      results[[d]]$summary[[i]][33, 1:4] <- as.vector(t(confusion_m35))
      results[[d]]$summary[[i]][34, 1:4] <- as.vector(t(confusion_m36))
      results[[d]]$summary[[i]][35, 1:4] <- as.vector(t(confusion_m37))
      results[[d]]$summary[[i]][36, 1:4] <- as.vector(t(confusion_m38))
      results[[d]]$summary[[i]][37, 1:4] <- as.vector(t(confusion_m39))
      
      results[[d]]$summary[[i]][, 5] <- accuracy
      results[[d]]$summary[[i]][, 6] <- error
      results[[d]]$summary[[i]][, 7] <- apply(results[[d]]$summary[[i]][, 1:4], 1, function(x) x[2] / (x[2] + x[4]))
      results[[d]]$summary[[i]][, 8] <- apply(results[[d]]$summary[[i]][, 1:4], 1, function(x) x[3] / (x[1] + x[3]))
      results[[d]]$summary[[i]][, 9] <- apply(results[[d]]$summary[[i]][, 1:4], 1, function(x) x[4] / (x[2] + x[4]))
      results[[d]]$summary[[i]][, 10] <- apply(results[[d]]$summary[[i]][, 1:4], 1, function(x) x[1] / (x[1] + x[3]))
      results[[d]]$summary[[i]][, 11] <- apply(results[[d]]$summary[[i]][, 1:4], 1, function(x) x[4] / (x[3] + x[4]))
      results[[d]]$summary[[i]][, 11][is.na(results[[d]]$summary[[i]][, 11])] <- 0    
      results[[d]]$summary[[i]][, 12] <- apply(results[[d]]$summary[[i]][, c(9, 11)], 1, function(x) 2 * ((x[1] * x[2]) / (x[1] + x[2])))
      results[[d]]$summary[[i]][, 12][is.na(results[[d]]$summary[[i]][, 12])] <- 0    
      results[[d]]$summary[[i]][, 13] <- auc_models
      results[[d]]$summary[[i]][, 14] <- thr
      
      
      
      
      results[[d]]$roc_curve$specificity[[i]][1, 1:length(roc_full$specificities)] <- roc_full$specificities
      results[[d]]$roc_curve$specificity[[i]][2, 1:length(roc_indep$specificities)] <- roc_indep$specificities
      results[[d]]$roc_curve$specificity[[i]][3, 1:length(roc_m1$specificities)] <- roc_m1$specificities
      results[[d]]$roc_curve$specificity[[i]][4, 1:length(roc_m2$specificities)] <- roc_m2$specificities
      results[[d]]$roc_curve$specificity[[i]][5, 1:length(roc_m3$specificities)] <- roc_m3$specificities
      results[[d]]$roc_curve$specificity[[i]][6, 1:length(roc_m4$specificities)] <- roc_m4$specificities
      results[[d]]$roc_curve$specificity[[i]][7, 1:length(roc_m5$specificities)] <- roc_m5$specificities
      results[[d]]$roc_curve$specificity[[i]][8, 1:length(roc_m6$specificities)] <- roc_m6$specificities
      results[[d]]$roc_curve$specificity[[i]][9, 1:length(roc_m11$specificities)] <- roc_m11$specificities
      results[[d]]$roc_curve$specificity[[i]][10, 1:length(roc_m12$specificities)] <- roc_m12$specificities
      results[[d]]$roc_curve$specificity[[i]][11, 1:length(roc_m13$specificities)] <- roc_m13$specificities
      results[[d]]$roc_curve$specificity[[i]][12, 1:length(roc_m14$specificities)] <- roc_m14$specificities
      results[[d]]$roc_curve$specificity[[i]][13, 1:length(roc_m15$specificities)] <- roc_m15$specificities
      results[[d]]$roc_curve$specificity[[i]][14, 1:length(roc_m16$specificities)] <- roc_m16$specificities
      results[[d]]$roc_curve$specificity[[i]][15, 1:length(roc_m17$specificities)] <- roc_m17$specificities
      results[[d]]$roc_curve$specificity[[i]][16, 1:length(roc_m18$specificities)] <- roc_m18$specificities
      results[[d]]$roc_curve$specificity[[i]][17, 1:length(roc_m19$specificities)] <- roc_m19$specificities
      results[[d]]$roc_curve$specificity[[i]][18, 1:length(roc_m20$specificities)] <- roc_m20$specificities
      results[[d]]$roc_curve$specificity[[i]][19, 1:length(roc_m21$specificities)] <- roc_m21$specificities
      results[[d]]$roc_curve$specificity[[i]][20, 1:length(roc_m22$specificities)] <- roc_m22$specificities
      results[[d]]$roc_curve$specificity[[i]][21, 1:length(roc_m23$specificities)] <- roc_m23$specificities
      results[[d]]$roc_curve$specificity[[i]][22, 1:length(roc_m24$specificities)] <- roc_m24$specificities
      results[[d]]$roc_curve$specificity[[i]][23, 1:length(roc_m25$specificities)] <- roc_m25$specificities
      results[[d]]$roc_curve$specificity[[i]][24, 1:length(roc_m26$specificities)] <- roc_m26$specificities
      results[[d]]$roc_curve$specificity[[i]][25, 1:length(roc_m27$specificities)] <- roc_m27$specificities
      results[[d]]$roc_curve$specificity[[i]][26, 1:length(roc_m28$specificities)] <- roc_m28$specificities
      results[[d]]$roc_curve$specificity[[i]][27, 1:length(roc_m29$specificities)] <- roc_m29$specificities
      results[[d]]$roc_curve$specificity[[i]][28, 1:length(roc_m30$specificities)] <- roc_m30$specificities
      results[[d]]$roc_curve$specificity[[i]][29, 1:length(roc_m31$specificities)] <- roc_m31$specificities
      results[[d]]$roc_curve$specificity[[i]][30, 1:length(roc_m36$specificities)] <- roc_m36$specificities
      results[[d]]$roc_curve$specificity[[i]][31, 1:length(roc_m37$specificities)] <- roc_m37$specificities
      results[[d]]$roc_curve$specificity[[i]][32, 1:length(roc_m39$specificities)] <- roc_m39$specificities
      
      
      results[[d]]$roc_curve$sensitivity[[i]][1, 1:length(roc_full$sensitivities)] <- roc_full$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][2, 1:length(roc_indep$sensitivities)] <- roc_indep$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][3, 1:length(roc_m1$sensitivities)] <- roc_m1$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][4, 1:length(roc_m2$sensitivities)] <- roc_m2$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][5, 1:length(roc_m3$sensitivities)] <- roc_m3$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][6, 1:length(roc_m4$sensitivities)] <- roc_m4$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][7, 1:length(roc_m5$sensitivities)] <- roc_m5$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][8, 1:length(roc_m6$sensitivities)] <- roc_m6$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][9, 1:length(roc_m11$sensitivities)] <- roc_m11$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][10, 1:length(roc_m12$sensitivities)] <- roc_m12$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][11, 1:length(roc_m13$sensitivities)] <- roc_m13$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][12, 1:length(roc_m14$sensitivities)] <- roc_m14$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][13, 1:length(roc_m15$sensitivities)] <- roc_m15$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][14, 1:length(roc_m16$sensitivities)] <- roc_m16$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][15, 1:length(roc_m17$sensitivities)] <- roc_m17$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][16, 1:length(roc_m18$sensitivities)] <- roc_m18$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][17, 1:length(roc_m19$sensitivities)] <- roc_m19$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][18, 1:length(roc_m20$sensitivities)] <- roc_m20$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][19, 1:length(roc_m21$sensitivities)] <- roc_m21$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][20, 1:length(roc_m22$sensitivities)] <- roc_m22$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][21, 1:length(roc_m23$sensitivities)] <- roc_m23$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][22, 1:length(roc_m24$sensitivities)] <- roc_m24$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][23, 1:length(roc_m25$sensitivities)] <- roc_m25$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][24, 1:length(roc_m26$sensitivities)] <- roc_m26$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][25, 1:length(roc_m27$sensitivities)] <- roc_m27$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][26, 1:length(roc_m28$sensitivities)] <- roc_m28$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][27, 1:length(roc_m29$sensitivities)] <- roc_m29$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][28, 1:length(roc_m30$sensitivities)] <- roc_m30$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][29, 1:length(roc_m31$sensitivities)] <- roc_m31$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][30, 1:length(roc_m36$sensitivities)] <- roc_m36$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][31, 1:length(roc_m37$sensitivities)] <- roc_m37$sensitivities
      results[[d]]$roc_curve$sensitivity[[i]][32, 1:length(roc_m39$sensitivities)] <- roc_m39$sensitivities

   
      results[[d]][[4]][, i] <- id_train
      results[[d]][[5]][, i] <- id_test
      results[[d]][[6]][, i] <- id_val
    } 
}




## ROC CURVE
colors <- rainbow(31)
plot(1 - results$Titanic$roc_curve$specificity$sample_1[1, ], results$Titanic$roc_curve$sensitivity$sample_1[1, ], lwd = 0.5, type = "l")
plot(roc_indep, lwd = 0.5, col = colors[1], add = TRUE)
plot(roc_m1, lwd = 0.5, col = colors[2], add = TRUE)
plot(roc_m2, lwd = 0.5, col = colors[3], add = TRUE)
plot(roc_m3, lwd = 0.5, col = colors[4], add = TRUE)
plot(roc_m4, lwd = 0.5, col = colors[5], add = TRUE)
plot(roc_m5, lwd = 0.5, col = colors[6], add = TRUE)
plot(roc_m6, lwd = 0.5, col = colors[7], add = TRUE)
plot(roc_m11, lwd = 0.5, col = colors[8], add = TRUE)
plot(roc_m12, lwd = 0.5, col = colors[9], add = TRUE)
plot(roc_m13, lwd = 0.5, col = colors[10], add = TRUE)
plot(roc_m14, lwd = 0.5, col = colors[11], add = TRUE)
plot(roc_m15, lwd = 0.5, col = colors[12], add = TRUE)
plot(roc_m16, lwd = 0.5, col = colors[13], add = TRUE)
plot(roc_m17, lwd = 0.5, col = colors[14], add = TRUE)
plot(roc_m18, lwd = 0.5, col = colors[15], add = TRUE)
plot(roc_m19, lwd = 0.5, col = colors[16], add = TRUE)
plot(roc_m20, lwd = 0.5, col = colors[17], add = TRUE)
plot(roc_m21, lwd = 0.5, col = colors[18], add = TRUE)
plot(roc_m22, lwd = 0.5, col = colors[19], add = TRUE)
plot(roc_m23, lwd = 0.5, col = colors[20], add = TRUE)
plot(roc_m24, lwd = 0.5, col = colors[21], add = TRUE)
plot(roc_m25, lwd = 0.5, col = colors[22], add = TRUE)
plot(roc_m26, lwd = 0.5, col = colors[23], add = TRUE)
plot(roc_m27, lwd = 0.5, col = colors[24], add = TRUE)
plot(roc_m28, lwd = 0.5, col = colors[25], add = TRUE)
plot(roc_m29, lwd = 0.5, col = colors[26], add = TRUE)
plot(roc_m30, lwd = 0.5, col = colors[27], add = TRUE)
plot(roc_m31, lwd = 0.5, col = colors[28], add = TRUE)
plot(roc_m36, lwd = 0.5, col = colors[29], add = TRUE)
plot(roc_m37, lwd = 0.5, col = colors[30], add = TRUE)
plot(roc_m39, lwd = 0.5, col = colors[31], add = TRUE)



## AUC
auc_full <- roc(response = test$answer, predictor = prob_m_full[, 2], auc = TRUE, plot = F)$auc
auc_indep <- roc(response = test$answer, predictor = prob_m_indep[, 2], auc = TRUE, plot = F)$auc
auc_m1 <- roc(response = test$answer, predictor = prob_m1[, 2], auc = TRUE, plot = F)$auc
auc_m2 <- roc(response = test$answer, predictor = prob_m2[, 2], auc = TRUE, plot = F)$auc
auc_m3 <- roc(response = test$answer, predictor = prob_m3[, 2], auc = TRUE, plot = F)$auc
auc_m4 <- roc(response = test$answer, predictor = prob_m4[, 2], auc = TRUE, plot = F)$auc
auc_m5 <- roc(response = test$answer, predictor = prob_m5[, 2], auc = TRUE, plot = F)$auc
auc_m6 <- roc(response = test$answer, predictor = prob_m6[, 2], auc = TRUE, plot = F)$auc
auc_m11 <- roc(response = test$answer, predictor = prob_m11[, 2], auc = TRUE, plot = F)$auc
auc_m12 <- roc(response = test$answer, predictor = prob_m12[, 2], auc = TRUE, plot = F)$auc
auc_m13 <- roc(response = test$answer, predictor = prob_m13[, 2], auc = TRUE, plot = F)$auc
auc_m14 <- roc(response = test$answer, predictor = prob_m14, auc = TRUE, plot = F)$auc
auc_m15 <- roc(response = test$answer, predictor = prob_m15, auc = TRUE, plot = F)$auc
auc_m16 <- roc(response = test$answer, predictor = prob_m16, auc = TRUE, plot = F)$auc
auc_m17 <- roc(response = test$answer, predictor = prob_m17[, 2], auc = TRUE, plot = F)$auc
auc_m18 <- roc(response = test$answer, predictor = prob_m18[, 2], auc = TRUE, plot = F)$auc
auc_m19 <- roc(response = test$answer, predictor = prob_m19[, 2], auc = TRUE, plot = F)$auc
auc_m20 <- roc(response = test$answer, predictor = prob_m20[, 2], auc = TRUE, plot = F)$auc
auc_m21 <- roc(response = test$answer, predictor = prob_m21[, 2], auc = TRUE, plot = F)$auc
auc_m22 <- roc(response = test$answer, predictor = prob_m22[, 2], auc = TRUE, plot = F)$auc
auc_m23 <- roc(response = test$answer, predictor = prob_m23[, 2], auc = TRUE, plot = F)$auc
auc_m24 <- roc(response = test$answer, predictor = prob_m24[, 2], auc = TRUE, plot = F)$auc
auc_m25 <- roc(response = test$answer, predictor = prob_m25[, 2], auc = TRUE, plot = F)$auc
auc_m26 <- roc(response = test$answer, predictor = prob_m26[, 2], auc = TRUE, plot = F)$auc
auc_m27 <- roc(response = test$answer, predictor = prob_m27[, 2], auc = TRUE, plot = F)$auc
auc_m28 <- roc(response = test$answer, predictor = prob_m28[, 2], auc = TRUE, plot = F)$auc
auc_m29 <- roc(response = test$answer, predictor = prob_m29[, 2], auc = TRUE, plot = F)$auc
auc_m30 <- roc(response = test$answer, predictor = prob_m30, auc = TRUE, plot = F)$auc
auc_m31 <- roc(response = test$answer, predictor = prob_m31, auc = TRUE, plot = F)$auc
auc_m36 <- roc(response = test$answer, predictor = prob_m36, auc = TRUE, plot = F)$auc
auc_m37 <- roc(response = test$answer, predictor = prob_m37, auc = TRUE, plot = F)$auc
auc_m39 <- roc(response = test$answer, predictor = prob_m39[, 2], auc = TRUE, plot = F)$auc





##  MATRICE DI CONFUSIONE
confusion_full <- tabella.sommario(previsti = pred_m_full, osservati = test$answer)
confusion_indep <- tabella.sommario(previsti = pred_m_indep, osservati = test$answer)
confusion_m1 <- tabella.sommario(previsti = pred_m1, osservati = test$answer)
confusion_m2 <- tabella.sommario(previsti = pred_m2, osservati = test$answer)
confusion_m3 <- tabella.sommario(previsti = pred_m3, osservati = test$answer)
confusion_m4 <- tabella.sommario(previsti = pred_m4, osservati = test$answer)
confusion_m5 <- tabella.sommario(previsti = pred_m5, osservati = test$answer)
confusion_m6 <- tabella.sommario(previsti = pred_m6, osservati = test$answer)
confusion_m11 <- tabella.sommario(previsti = pred_m11, osservati = test$answer)
confusion_m12 <- tabella.sommario(previsti = pred_m12, osservati = test$answer)
confusion_m13 <- tabella.sommario(previsti = pred_m13, osservati = test$answer)
confusion_m14 <- tabella.sommario(previsti = pred_m14, osservati = test$answer)
confusion_m15 <- tabella.sommario(previsti = pred_m15, osservati = test$answer)
confusion_m16 <- tabella.sommario(previsti = pred_m16, osservati = test$answer)
confusion_m17 <- tabella.sommario(previsti = pred_m17, osservati = test$answer)
confusion_m18 <- tabella.sommario(previsti = pred_m18, osservati = test$answer)
confusion_m19 <- tabella.sommario(previsti = pred_m19, osservati = test$answer)
confusion_m20 <- tabella.sommario(previsti = pred_m20, osservati = test$answer)
confusion_m21 <- tabella.sommario(previsti = pred_m21, osservati = test$answer)
confusion_m22 <- tabella.sommario(previsti = pred_m22, osservati = test$answer)
confusion_m23 <- tabella.sommario(previsti = pred_m23, osservati = test$answer)
confusion_m24 <- tabella.sommario(previsti = pred_m24, osservati = test$answer)
confusion_m25 <- tabella.sommario(previsti = pred_m25, osservati = test$answer)
confusion_m26 <- tabella.sommario(previsti = pred_m26, osservati = test$answer)
confusion_m27 <- tabella.sommario(previsti = pred_m27, osservati = test$answer)
confusion_m28 <- tabella.sommario(previsti = pred_m28, osservati = test$answer)
confusion_m29 <- tabella.sommario(previsti = pred_m29, osservati = test$answer)
confusion_m30 <- tabella.sommario(previsti = pred_m30, osservati = test$answer)
confusion_m31 <- tabella.sommario(previsti = pred_m31, osservati = test$answer)
confusion_m32 <- tabella.sommario(previsti = pred_m32, osservati = test$answer)
confusion_m33 <- tabella.sommario(previsti = pred_m33, osservati = test$answer)
confusion_m34 <- tabella.sommario(previsti = pred_m34, osservati = test$answer)
confusion_m35 <- tabella.sommario(previsti = pred_m35, osservati = test$answer)
confusion_m36 <- tabella.sommario(previsti = pred_m36, osservati = test$answer)
confusion_m37 <- tabella.sommario(previsti = pred_m37, osservati = test$answer)
confusion_m38 <- tabella.sommario(previsti = pred_m38, osservati = test$answer)
confusion_m39 <- tabella.sommario(previsti = pred_m39, osservati = test$answer)





## FALSI POSITIVI - FALSI NEGATIVI - ERRORE GLOBALE
fp_fn_err <- matrix(NA, nrow = 37, ncol = 3)
fp_fn_err[1, ] <- c(fp_fn(previsti = pred_m_full, osservati = test$answer)$fp, 
               fp_fn(previsti = pred_m_full, osservati = test$answer)$fn, 
               fp_fn(previsti = pred_m_full, osservati = test$answer)$error)
fp_fn_err[2, ] <- c(fp_fn(previsti = pred_m_indep, osservati = test$answer)$fp, 
               fp_fn(previsti = pred_m_indep, osservati = test$answer)$fn, 
               fp_fn(previsti = pred_m_indep, osservati = test$answer)$error)
fp_fn_err[3, ] <- c(fp_fn(previsti = pred_m1, osservati = test$answer)$fp, 
               fp_fn(previsti = pred_m1, osservati = test$answer)$fn, 
               fp_fn(previsti = pred_m1, osservati = test$answer)$error)
fp_fn_err[4, ] <- c(fp_fn(previsti = pred_m2, osservati = test$answer)$fp, 
               fp_fn(previsti = pred_m2, osservati = test$answer)$fn, 
               fp_fn(previsti = pred_m2, osservati = test$answer)$error)
fp_fn_err[5, ] <- c(fp_fn(previsti = pred_m3, osservati = test$answer)$fp, 
               fp_fn(previsti = pred_m3, osservati = test$answer)$fn, 
               fp_fn(previsti = pred_m3, osservati = test$answer)$error)
fp_fn_err[6, ] <- c(fp_fn(previsti = pred_m4, osservati = test$answer)$fp, 
               fp_fn(previsti = pred_m4, osservati = test$answer)$fn, 
               fp_fn(previsti = pred_m4, osservati = test$answer)$error)
fp_fn_err[7, ] <- c(fp_fn(previsti = pred_m5, osservati = test$answer)$fp, 
               fp_fn(previsti = pred_m5, osservati = test$answer)$fn, 
               fp_fn(previsti = pred_m5, osservati = test$answer)$error)
fp_fn_err[8, ] <- c(fp_fn(previsti = pred_m6, osservati = test$answer)$fp, 
               fp_fn(previsti = pred_m6, osservati = test$answer)$fn, 
               fp_fn(previsti = pred_m6, osservati = test$answer)$error)
fp_fn_err[9, ] <- c(fp_fn(previsti = pred_m11, osservati = test$answer)$fp, 
                    fp_fn(previsti = pred_m11, osservati = test$answer)$fn, 
                    fp_fn(previsti = pred_m11, osservati = test$answer)$error)
fp_fn_err[10, ] <- c(fp_fn(previsti = pred_m12, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m12, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m12, osservati = test$answer)$error)
fp_fn_err[11, ] <- c(fp_fn(previsti = pred_m13, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m13, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m13, osservati = test$answer)$error)
fp_fn_err[12, ] <- c(fp_fn(previsti = pred_m14, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m14, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m14, osservati = test$answer)$error)
fp_fn_err[13, ] <- c(fp_fn(previsti = pred_m15, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m15, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m15, osservati = test$answer)$error)
fp_fn_err[14, ] <- c(fp_fn(previsti = pred_m16, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m16, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m16, osservati = test$answer)$error)
fp_fn_err[15, ] <- c(fp_fn(previsti = pred_m17, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m17, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m17, osservati = test$answer)$error)
fp_fn_err[16, ] <- c(fp_fn(previsti = pred_m18, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m18, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m18, osservati = test$answer)$error)
fp_fn_err[17, ] <- c(fp_fn(previsti = pred_m19, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m19, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m19, osservati = test$answer)$error)
fp_fn_err[18, ] <- c(fp_fn(previsti = pred_m20, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m20, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m20, osservati = test$answer)$error)
fp_fn_err[19, ] <- c(fp_fn(previsti = pred_m21, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m21, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m21, osservati = test$answer)$error)
fp_fn_err[20, ] <- c(fp_fn(previsti = pred_m22, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m22, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m22, osservati = test$answer)$error)
fp_fn_err[21, ] <- c(fp_fn(previsti = pred_m23, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m23, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m23, osservati = test$answer)$error)
fp_fn_err[22, ] <- c(fp_fn(previsti = pred_m24, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m24, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m24, osservati = test$answer)$error)
fp_fn_err[23, ] <- c(fp_fn(previsti = pred_m25, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m25, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m25, osservati = test$answer)$error)
fp_fn_err[24, ] <- c(fp_fn(previsti = pred_m26, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m26, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m26, osservati = test$answer)$error)
fp_fn_err[25, ] <- c(fp_fn(previsti = pred_m27, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m27, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m27, osservati = test$answer)$error)
fp_fn_err[26, ] <- c(fp_fn(previsti = pred_m28, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m28, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m28, osservati = test$answer)$error)
fp_fn_err[27, ] <- c(fp_fn(previsti = pred_m29, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m29, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m29, osservati = test$answer)$error)
fp_fn_err[28, ] <- c(fp_fn(previsti = pred_m30, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m30, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m30, osservati = test$answer)$error)
fp_fn_err[29, ] <- c(fp_fn(previsti = pred_m31, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m31, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m31, osservati = test$answer)$error)
fp_fn_err[30, ] <- c(fp_fn(previsti = pred_m32, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m32, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m32, osservati = test$answer)$error)
fp_fn_err[31, ] <- c(fp_fn(previsti = pred_m33, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m33, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m33, osservati = test$answer)$error)
fp_fn_err[32, ] <- c(fp_fn(previsti = pred_m34, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m34, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m34, osservati = test$answer)$error)
fp_fn_err[33, ] <- c(fp_fn(previsti = pred_m35, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m35, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m35, osservati = test$answer)$error)
fp_fn_err[34, ] <- c(fp_fn(previsti = pred_m36, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m36, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m36, osservati = test$answer)$error)
fp_fn_err[35, ] <- c(fp_fn(previsti = pred_m37, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m37, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m37, osservati = test$answer)$error)
fp_fn_err[36, ] <- c(fp_fn(previsti = pred_m38, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m38, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m38, osservati = test$answer)$error)
fp_fn_err[37, ] <- c(fp_fn(previsti = pred_m39, osservati = test$answer)$fp, 
                     fp_fn(previsti = pred_m39, osservati = test$answer)$fn, 
                     fp_fn(previsti = pred_m39, osservati = test$answer)$error)

plot(1:37, fp_fn_err[, 1], ylim = c(0, 1))
points(1:37, fp_fn_err[, 2], col = 2)
points(1:37, fp_fn_err[, 3], col = 3)




## SENSITIVITA' (RECALL) - SPECIFICITA' 
# sensitività: % tra tutti i realmente positivi che vengono classificati correttamente.
# specificità: % tra tutti i realmente negativi che vengono classificati correttamente.

## When passing a table, more than two levels can
## be used
# sensitivity(irisTabs, "versicolor")
# specificity(irisTabs, c("setosa", "virginica"))

sens_spec <- matrix(NA, nrow = 37, ncol = 2)
sens_spec[1, ] <- c(caret::sensitivity(pred_m_full, test$answer), caret::specificity(pred_m_full, test$answer))
sens_spec[2, ] <- c(caret::sensitivity(pred_m_indep, test$answer), caret::specificity(pred_m_indep, test$answer))
sens_spec[3, ] <- c(caret::sensitivity(pred_m1, test$answer), caret::specificity(pred_m1, test$answer))
sens_spec[4, ] <- c(caret::sensitivity(pred_m2, test$answer), caret::specificity(pred_m2, test$answer))
sens_spec[5, ] <- c(caret::sensitivity(pred_m3, test$answer), caret::specificity(pred_m3, test$answer))
sens_spec[6, ] <- c(caret::sensitivity(pred_m4, test$answer), caret::specificity(pred_m4, test$answer))
sens_spec[7, ] <- c(caret::sensitivity(pred_m5, test$answer), caret::specificity(pred_m5, test$answer))
sens_spec[8, ] <- c(caret::sensitivity(pred_m6, test$answer), caret::specificity(pred_m6, test$answer))
sens_spec[9, ] <- c(caret::sensitivity(pred_m11, test$answer), caret::specificity(pred_m11, test$answer))
sens_spec[10, ] <- c(caret::sensitivity(pred_m12, test$answer), caret::specificity(pred_m12, test$answer))
sens_spec[11, ] <- c(caret::sensitivity(pred_m13, test$answer), caret::specificity(pred_m13, test$answer))
sens_spec[12, ] <- c(caret::sensitivity(pred_m14, test$answer), caret::specificity(pred_m14, test$answer))
sens_spec[13, ] <- c(caret::sensitivity(pred_m15, test$answer), caret::specificity(pred_m15, test$answer))
sens_spec[14, ] <- c(caret::sensitivity(pred_m16, test$answer), caret::specificity(pred_m16, test$answer))
sens_spec[15, ] <- c(caret::sensitivity(pred_m17, test$answer), caret::specificity(pred_m17, test$answer))
sens_spec[16, ] <- c(caret::sensitivity(pred_m18, test$answer), caret::specificity(pred_m18, test$answer))
sens_spec[17, ] <- c(caret::sensitivity(pred_m19, test$answer), caret::specificity(pred_m19, test$answer))
sens_spec[18, ] <- c(caret::sensitivity(pred_m20, test$answer), caret::specificity(pred_m20, test$answer))
sens_spec[19, ] <- c(caret::sensitivity(pred_m21, test$answer), caret::specificity(pred_m21, test$answer))
sens_spec[20, ] <- c(caret::sensitivity(pred_m22, test$answer), caret::specificity(pred_m22, test$answer))
sens_spec[21, ] <- c(caret::sensitivity(pred_m23, test$answer), caret::specificity(pred_m23, test$answer))
sens_spec[22, ] <- c(caret::sensitivity(pred_m24, test$answer), caret::specificity(pred_m24, test$answer))
sens_spec[23, ] <- c(caret::sensitivity(pred_m25, test$answer), caret::specificity(pred_m25, test$answer))
sens_spec[24, ] <- c(caret::sensitivity(pred_m26, test$answer), caret::specificity(pred_m26, test$answer))
sens_spec[25, ] <- c(caret::sensitivity(pred_m27, test$answer), caret::specificity(pred_m27, test$answer))
sens_spec[26, ] <- c(caret::sensitivity(pred_m28, test$answer), caret::specificity(pred_m28, test$answer))
sens_spec[27, ] <- c(caret::sensitivity(pred_m29, test$answer), caret::specificity(pred_m29, test$answer))
sens_spec[28, ] <- c(caret::sensitivity(pred_m30, test$answer), caret::specificity(pred_m30, test$answer))
sens_spec[29, ] <- c(caret::sensitivity(pred_m31, test$answer), caret::specificity(pred_m31, test$answer))
sens_spec[30, ] <- c(caret::sensitivity(pred_m32, test$answer), caret::specificity(pred_m32, test$answer))
sens_spec[31, ] <- c(caret::sensitivity(pred_m33, test$answer), caret::specificity(pred_m33, test$answer))
sens_spec[32, ] <- c(caret::sensitivity(pred_m34, test$answer), caret::specificity(pred_m34, test$answer))
sens_spec[33, ] <- c(caret::sensitivity(pred_m35, test$answer), caret::specificity(pred_m35, test$answer))
sens_spec[34, ] <- c(caret::sensitivity(pred_m36, test$answer), caret::specificity(pred_m36, test$answer))
sens_spec[35, ] <- c(caret::sensitivity(pred_m37, test$answer), caret::specificity(pred_m37, test$answer))
sens_spec[36, ] <- c(caret::sensitivity(pred_m38, test$answer), caret::specificity(pred_m38, test$answer))
sens_spec[37, ] <- c(caret::sensitivity(pred_m39, test$answer), caret::specificity(pred_m39, test$answer))


##  PRECISION 
precision <- numeric(37)
precision[1] <- confusion_full[1, 1] / rowSums(confusion_full)[1]
precision[2] <- confusion_indep[1, 1] / rowSums(confusion_indep)[1]
precision[3] <- confusion_m1[1, 1] / rowSums(confusion_m1)[1]
precision[4] <- confusion_m2[1, 1] / rowSums(confusion_m2)[1]
precision[5] <- confusion_m3[1, 1] / rowSums(confusion_m3)[1]
precision[6] <- confusion_m4[1, 1] / rowSums(confusion_m4)[1]
precision[7] <- confusion_m5[1, 1] / rowSums(confusion_m5)[1]
precision[8] <- confusion_m6[1, 1] / rowSums(confusion_m6)[1]
precision[9] <- confusion_m11[1, 1] / rowSums(confusion_m11)[1]
precision[10] <- confusion_m12[1, 1] / rowSums(confusion_m12)[1]
precision[11] <- confusion_m13[1, 1] / rowSums(confusion_m13)[1]
precision[12] <- confusion_m14[1, 1] / rowSums(confusion_m14)[1]
precision[13] <- confusion_m15[1, 1] / rowSums(confusion_m15)[1]
precision[14] <- confusion_m16[1, 1] / rowSums(confusion_m16)[1]
precision[15] <- confusion_m17[1, 1] / rowSums(confusion_m17)[1]
precision[16] <- confusion_m18[1, 1] / rowSums(confusion_m18)[1]
precision[17] <- confusion_m19[1, 1] / rowSums(confusion_m19)[1]
precision[18] <- confusion_m20[1, 1] / rowSums(confusion_m20)[1]
precision[19] <- confusion_m21[1, 1] / rowSums(confusion_m21)[1]
precision[20] <- confusion_m22[1, 1] / rowSums(confusion_m22)[1]
precision[21] <- confusion_m23[1, 1] / rowSums(confusion_m23)[1]
precision[22] <- confusion_m24[1, 1] / rowSums(confusion_m24)[1]
precision[23] <- confusion_m25[1, 1] / rowSums(confusion_m25)[1]
precision[24] <- confusion_m26[1, 1] / rowSums(confusion_m26)[1]
precision[25] <- confusion_m27[1, 1] / rowSums(confusion_m27)[1]
precision[26] <- confusion_m28[1, 1] / rowSums(confusion_m28)[1]
precision[27] <- confusion_m29[1, 1] / rowSums(confusion_m29)[1]
precision[28] <- confusion_m30[1, 1] / rowSums(confusion_m30)[1]
precision[29] <- confusion_m31[1, 1] / rowSums(confusion_m31)[1]
precision[30] <- confusion_m32[1, 1] / rowSums(confusion_m32)[1]
precision[31] <- confusion_m33[1, 1] / rowSums(confusion_m33)[1]
precision[32] <- confusion_m34[1, 1] / rowSums(confusion_m34)[1]
precision[33] <- confusion_m35[1, 1] / rowSums(confusion_m35)[1]
precision[34] <- confusion_m36[1, 1] / rowSums(confusion_m36)[1]
precision[35] <- confusion_m37[1, 1] / rowSums(confusion_m37)[1]
precision[36] <- confusion_m38[1, 1] / rowSums(confusion_m38)[1]
precision[37] <- confusion_m39[1, 1] / rowSums(confusion_m39)[1]
precision[is.na(precision)] <- 0  # quando dividiamo per 0 (0 previsti positivi)


## F1 - SCORE 
f1_score <- numeric(37)
f1_score <- apply(cbind(sens_spec, precision), 1, function(x) 2 * ((x[1] * x[3]) / (x[1] + x[3])))
f1_score[is.na(f1_score)] <- 0  # quando si ha 0 recall e precision si divide per 0.







## BEST SPLIT
sp <- seq(0.001, 1, length.out = 1000)
qq <- list()
for(i in 1:length(sp)) qq[[i]] <- table(prob_m5[, 2] > sp[i], test$answer)




d <- (roc_m19[[2]] - roc_m19[[1]]) / sqrt(2)
order(prob_m19[, 2])[(length(prob_m19[, 2]) - which.max(d))]    
prob_m19[98, 2] 

table(prob_m20[, 2] > optimalCutoff(as.numeric(test$answer) - 1, 
                                   prob_m20[, 2], optimiseFor = "Both"), test$answer)



### mean of bootstrap iterations for stagedtrees algorithms
mean_boot_results <- Reduce('+', boot_result)
mean_boot_results <- mean_boot_results / length(boot_result)
row.names(mean_boot_results) <- c("FULL", "INDEP", "HC - FULL", "HC - INDEP", "BHC", "FAST BHC", "RANDOM BHC", "KL - 0.01", "KL - 0.05", "KL - 0.20", 
                                  "TV - 0.01", "TV - 0.05", "TV - 0.20", "LP - 0.01", "LP - 0.05", "LP - 0.20", "RY - 0.01", "RY - 0.05", "RY - 0.20", 
                                  "HL - 0.01", "HL - 0.05", "HL - 0.20", "BH - 0.01", "BH - 0.05", "BH - 0.20", "CD - 0.01", "CD - 0.05", "CD - 0.20",
                                  "NAIVE TREE", "BN - STAGED TREE")
mean_boot_results


## mean of bootstrap accuracies for literature algorithms
mean_accuracy <- apply(accuracy, 2, mean)
mean_accuracy <- as.data.frame(x = mean_accuracy)
row.names(mean_accuracy) <- c("BNLEARN HC", "BNLEARN TABU", "NAIVE BAYES", "LOGISTIC", "NEURAL NETWORK", "CLASSIFICATION TREE", "RANDOM FOREST")
mean_accuracy






