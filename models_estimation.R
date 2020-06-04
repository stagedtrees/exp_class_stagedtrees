#############################  MODEL ESTIMATION THROUGH BOOTSTRAP ITERATIONS  #############################  

source("/Users/federico/Dropbox/Gherardo:Manuele/Classification Paper/install_load_packages.R")
source("/Users/federico/Dropbox/Gherardo:Manuele/Classification Paper/initialize_results.R")



set.seed(123) 
for(d in c(1:9, 11:20)) { 
  for(i in 1:10)  
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
    
    # NON POSSIAMO AVERE LE PROBABILITA'. TRATTA TUTTO COME VARIABILI CONTINUE. NON AVREMO QUINDI LA CURVA ROC
    time_m32 <- system.time(m32 <- spls(x = x[, -1], y = x$answer, K = 2, eta = 0.5))[1]
    pred_m32 <- predict(m32, newx = z[, -1]) # poi da categorizzare, predice valori continui
    pred_m32 <- factor(ifelse(pred_m32 >= 1.5, levels(binary_datasets[[d]]$answer)[2], 
                              levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
    acc_m32 <- sum(diag(table(pred_m32, validation$answer))) / NROW(validation)
    
    # NON POSSIAMO AVERE LE PROBABILITA'. TRATTA TUTTO COME VARIABILI CONTINUE. NON AVREMO QUINDI LA CURVA ROC
    time_m33 <- system.time(m33 <- plsr(answer ~ ., data = x))[1]
    pred_m33 <- predict(m33, newdata = z, type = "response") # poi da categorizzare, predice valori continui
    pred_m33 <- factor(ifelse(pred_m33[, , 2] >= 1.5, levels(binary_datasets[[d]]$answer)[2], levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
    acc_m33 <- sum(diag(table(pred_m33, validation$answer))) / NROW(validation)
    
    # NON POSSIAMO AVERE LE PROBABILITA'. TRATTA TUTTO COME VARIABILI CONTINUE. NON AVREMO QUINDI LA CURVA ROC
    time_m34 <- system.time(m34 <- mars(x = x[, -1], y = x[, 1]))[1]
    pred_m34 <- predict(m34, newdata = z[, -1])
    pred_m34 <- factor(ifelse(pred_m34 >= 1.5, levels(binary_datasets[[d]]$answer)[2], levels(binary_datasets[[d]]$answer)[1]), levels = levels(binary_datasets[[d]]$answer))
    acc_m34 <- sum(diag(table(pred_m34, validation$answer))) / NROW(validation)
    
    # NON POSSIAMO AVERE LE PROBABILITA'. TRATTA TUTTO COME VARIABILI CONTINUE. NON AVREMO QUINDI LA CURVA ROC
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
    
    # NON POSSIAMO AVERE LE PROBABILITA'. TRATTA TUTTO COME VARIABILI CONTINUE. NON AVREMO QUINDI LA CURVA ROC
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

saveRDS(results, file = "results.rds")