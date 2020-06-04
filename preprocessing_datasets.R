#############################  DATASETS  #############################  


# load the .R file with the code to install and load all the needed libraries
source("/Users/federico/Dropbox/Gherardo:Manuele/Classification Paper/install_load_packages.R")



abalone <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/abalone.data", header = F)
abalone <- na.exclude(abalone)
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
rm(data)
# answer variable 
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




# maybe too large
breast_cancer <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/breast-cancer.data", header = F, na.strings = "?")
breast_cancer <- data.frame(na.exclude(breast_cancer))
breast_cancer$V7 <- factor(breast_cancer$V7)
colnames(breast_cancer) <- c("answer", colnames(breast_cancer)[-1])





car <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/car.data", header = F)
car <- car[, c(7, 1:6)]
colnames(car) <- c("answer", colnames(car)[-1])





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
# 2 different datasets: energy1 with y1 and energy2 with y2.
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





# i .train sono un sotto-campione del test. usiamo direttamente il .test che è completo.
monks1_test <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/monks-1.test", sep = " ", header = F)[, -1]
monks2_test <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/monks-2.test", sep = " ", header = F)[, -1]
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





post_operative <- read.csv("/Users/federico/Desktop/Latex/Paper Classification/datasets/post-operative.data", header = F,
                           na.strings = "?")
post_operative <- data.frame(na.exclude(post_operative))
# c'erano due 5 e un 7, messi insieme a 10 per non creare classi con 1 o 2 osservazioni.
post_operative$V8[post_operative$V8 == 5 | post_operative$V8 == 7] <- 10 
post_operative$V8 <- factor(post_operative$V8, levels = c("10", "15"))
post_operative <- post_operative[, c(9, 1:8)]
post_operative$V9[as.numeric(post_operative$V9) == 2] <- "A"
post_operative$V9 <- factor(post_operative$V9)
colnames(post_operative) <- c("answer", colnames(post_operative)[-1])





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




Asym <- read.csv("/Users/federico/Dropbox/Software Paper/final/Asym.csv")
for(i in 1:NCOL(Asym)) Asym[, i] <- factor(Asym[, i], labels = c("0", "1"))
Asym <- Asym[, c(4, 1:3)]
colnames(Asym) <- c("answer", colnames(Asym)[-1])




chds <- read.csv("/Users/federico/Dropbox/Software Paper/final/chds.csv")
chds <- chds[, c(4, 1:3)]
colnames(chds) <- c("answer", colnames(chds)[-1])




selfy <- read.csv("/Users/federico/Dropbox/Software Paper/final/selfy.csv")
selfy <- selfy[, c(4, 1:3)]
colnames(selfy) <- c("answer", colnames(selfy)[-1])




FallEld <- read.csv("/Users/federico/Dropbox/Software Paper/final/FallEld.csv")
FallEld <- FallEld[, c(4, 1:3)]
colnames(FallEld) <- c("answer", colnames(FallEld)[-1])





#######   datasets downloadable directly in R   ####### 

data("chestSim500", package = "gRbase")
chestSim500 <- chestSim500[, c(8, 1:7)]
colnames(chestSim500) <- c("answer", colnames(chestSim500)[-1])




data("PhDArticles", package = "stagedtrees")
colnames(PhDArticles) <- c("answer", colnames(PhDArticles)[-1])




data("Pokemon", package = "stagedtrees")
Pokemon <- Pokemon[, c(5, 1:4)]
colnames(Pokemon) <- c("answer", colnames(Pokemon)[-1])




data("puffin", package = "MBCbook")
puffin$class <- as.factor(puffin$class)
colnames(puffin) <- c("answer", colnames(puffin)[-1])




data(reinis, package = "gRbase")
reinis <- as.data.frame(reinis)
reinis <- reinis[rep(row.names(reinis), reinis$Freq), 1:6]
reinis <- reinis[, c(6, 1:5)]
colnames(reinis) <- c("answer", colnames(reinis)[-1])




data(Titanic, package = "datasets")
Titanic <- as.data.frame(Titanic)
Titanic <- Titanic[rep(row.names(Titanic), Titanic$Freq), 1:4]
Titanic <- Titanic[, c(4, 1:3)]
colnames(Titanic) <- c("answer", colnames(Titanic)[-1])

rm(i); rm(j); rm(k); rm(m); rm(quant)

save.image("/Users/federico/Dropbox/Gherardo:Manuele/Classification Paper/datasets.RData")


