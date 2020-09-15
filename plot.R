library(ggplot2)
library(data.table)

datasets <- factor(read.table("binary_fast_datasets_names.tsv")[, 1])

nreps <- 10

### select which methods to plot
classifiers <- c(
  "st_full", "st_full_mi", "st_full_cmi", "st_full_ch",
  "st_indep", "st_indep_mi", "st_indep_cmi", "st_indep_ch",
  "st_hc_indep_5", "st_hc_indep_mi_5", "st_hc_indep_cmi_5", "st_hc_indep_ch_5",
  "st_hc_indep_7", "st_hc_indep_mi_7", "st_hc_indep_cmi_7", "st_hc_indep_ch_7",
  "st_hc_full_5", "st_hc_full_mi_5", "st_hc_full_cmi_5", "st_hc_full_ch_5",
  # "st_hc_full_7", "st_hc_full_mi_7", "st_hc_full_cmi_7", "st_hc_full_ch_7",
  "st_fbhc", "st_fbhc_mi", "st_fbhc_cmi", "st_fbhc_ch",
  "st_bhc_5", "st_bhc_mi_5", "st_bhc_cmi_5", "st_bhc_ch_5",
  "st_bhc_7", "st_bhc_mi_7", "st_bhc_cmi_7", "st_bhc_ch_7",
  "st_bj_kl", "st_bj_kl_mi", "st_bj_kl_cmi", "st_bj_kl_ch",
  "st_bj_tv", "st_bj_tv_mi", "st_bj_tv_cmi", "st_bj_tv_ch",
  "st_bj_cd", "st_bj_cd_mi", "st_bj_cd_cmi", "st_bj_cd_ch",
  "st_naive", "st_naive_mi", "st_naive_cmi", "st_naive_ch",
  "bn_tabu",
  "bn_hc",
  "bnc_nb",
  "bnc_tan_cl",
  "bnc_tan_hc",
  "bnc_fssj",
  "bnc_bsej",
  "bnc_3db",
  "nnet_1",
  "nnet_2",
  "rf_1",
  "rf_2",
  "glm_binomial",
  "logistic_basic",
  "naive_bayes_1",
  "naive_bayes_2",
  "cl_tree_1",
  "cl_tree_2",
  "regularized_da",
  "boosting_basic",
  "bagging_basic",
  "svm_basic",
  "gam_basic",
  "adaboost_basic",
  "simple"
)


##read ROC_CURVE.rds
ROC_CURVE <- readRDS("ROC_CURVE.rds")

constantwise_function <- function(x, x.val, y.val) {
  x.val <- sort(as.numeric(na.exclude(1 - x.val)))
  y.val <- sort(as.numeric(na.exclude(y.val)))
  out <- 0
  
  for(i in 1:(length(x.val) - 1)) {
    if(x > x.val[i] & x <= x.val[i+1]) {
      out <- ((y.val[i] - y.val[i+1]) / (x.val[i] - x.val[i+1])) * x + y.val[i] - 
        ((y.val[i] - y.val[i+1]) / (x.val[i] - x.val[i+1])) * x.val[i]
    }
  }
  return(out)
}

constantwise_function <- Vectorize(constantwise_function, "x")

interpolation <- function(x.val, y.val) {
  y <- NULL
  count <- 0
  for(i in c(seq(0, 0.3, length.out = 70), seq(0.3, 0.8, length.out = 24), 
             seq(0.8, 1, length.out = 24))) {
    count <- count + 1
    y[count] <- constantwise_function(i, x.val, y.val)
  }
  return(c(0, y, 1))
}

interpolated_sensitivity <- array(
  data = rep(NA, 120),
  dim = c(
    length(datasets),
    length(classifiers),
    nreps,
    120 
  ),
  dimnames = list(
    data = datasets,
    classifier = classifiers,
    rep = 1:nreps,
    values = 1:120
  )
)

for(i in 1:length(datasets)) {
  for(j in 1:length(classifiers)) {
    for(k in 1:nreps) {
      interpolated_sensitivity[i, j, k, ] <- interpolation(ROC_CURVE[1, i, j, k, ], ROC_CURVE[2, i, j, k, ])
    }
  }
}

# they are the same for all roc curve, so now it is reasonable to compute the mean between bootstrap iterations.
fixed_specificities <- c(0, seq(0, 0.3, length.out = 70), seq(0.3, 0.8, length.out = 24), 
                         seq(0.8, 1, length.out = 24), 1)


AVG_ROC_CURVE <- apply(interpolated_sensitivity, c(1,2,4), mean)
saveRDS(AVG_ROC_CURVE, "AVG_ROC_CURVE.rds")



pdf("roc_curves1.pdf")
par(mfrow = c(2, 2))
colors <- rainbow(length(classifiers) - 1)
for(j in 1:4) {
  plot(fixed_specificities, AVG_ROC_CURVE[j, 1, ], lwd = 0.5,
       type = "l", xlab = "1 - Specificity", ylab = "Sensitivity", 
       main = attr(AVG_ROC_CURVE, "dimnames")$data[j], xlim = c(0, 1), ylim = c(0, 1))
  for(i in 2:length(classifiers)) {
    points(fixed_specificities, AVG_ROC_CURVE[j, i, ], lwd = 0.5, col = colors[i-1], type = "l")
  }
}
dev.off()


pdf("roc_curves2.pdf")
par(mfrow = c(2, 2))
colors <- rainbow(length(classifiers) - 1)
for(j in 5:8) {
  plot(fixed_specificities, AVG_ROC_CURVE[j, 1, ], lwd = 0.5,
       type = "l", xlab = "1 - Specificity", ylab = "Sensitivity", 
       main = attr(AVG_ROC_CURVE, "dimnames")$data[j], xlim = c(0, 1), ylim = c(0, 1))
  for(i in 2:length(classifiers)) {
    points(fixed_specificities, AVG_ROC_CURVE[j, i, ], lwd = 0.5, col = colors[i-1], type = "l")
  }
}
dev.off()


pdf("roc_curves3.pdf")
par(mfrow = c(2, 2))
colors <- rainbow(length(classifiers) - 1)
for(j in 9:12) {
  plot(fixed_specificities, AVG_ROC_CURVE[j, 1, ], lwd = 0.5,
       type = "l", xlab = "1 - Specificity", ylab = "Sensitivity", 
       main = attr(AVG_ROC_CURVE, "dimnames")$data[j], xlim = c(0, 1), ylim = c(0, 1))
  for(i in 2:length(classifiers)) {
    points(fixed_specificities, AVG_ROC_CURVE[j, i, ], lwd = 0.5, col = colors[i-1], type = "l")
  }
}
dev.off()


pdf("roc_curves4.pdf")
par(mfrow = c(2, 2))
colors <- rainbow(length(classifiers) - 1)
for(j in 13:15) {
  plot(fixed_specificities, AVG_ROC_CURVE[j, 1, ], lwd = 0.5,
       type = "l", xlab = "1 - Specificity", ylab = "Sensitivity", 
       main = attr(AVG_ROC_CURVE, "dimnames")$data[j], xlim = c(0, 1), ylim = c(0, 1))
  for(i in 2:length(classifiers)) {
    points(fixed_specificities, AVG_ROC_CURVE[j, i, ], lwd = 0.5, col = colors[i-1], type = "l")
  }
}
dev.off()


################################################################################################################
################################################################################################################
################################################################################################################


##read TABLE.rds
TABLE <- readRDS("TABLE.rds")
## compute averages
AVG <- apply(TABLE, c(1,2,3), mean, na.rm = TRUE)

## transform to data.table
data <- as.data.table(AVG)



## plot accuracies
ggplot(data = data[stat %in% c("accuracy", "balanced_accuracy") & classifier %in% classifiers], aes(y = data, x = value, 
                        group = classifier, color = classifier)) + 
  geom_jitter(height = 0.2, width = 0, alpha = 0.5) + facet_grid(cols = vars(stat), scales = "free") + 
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30),
    legend.box.spacing = unit(0.5, "lines"),
    legend.box.margin = ggplot2::margin(
      t = 0,
      r = 5,
      b = 0,
      l = 0,
      unit = "pt"
    ),
    plot.margin = ggplot2::margin(
      t = 0,
      r = 5,
      b = 0,
      l = 0,
      unit = "pt"
    )
  ) + xlab("") +  
  ggsave("plot_accuracy.pdf", width = 7, height = 6, units = "in")



## plot time
ggplot(data = data[stat == "time" & classifier %in% classifiers], aes(y = data, x = value, 
                                                                      group = classifier, color = classifier)) + 
  geom_jitter(height = 0.2, alpha = 0.5) + 
  theme_bw() +
  scale_x_log10()+
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30),
    legend.box.spacing = unit(0.5, "lines"),
    legend.box.margin = ggplot2::margin(
      t = 0,
      r = 15,
      b = 0,
      l = 0,
      unit = "pt"
    ),
    plot.margin = ggplot2::margin(
      t = 0,
      r = 15,
      b = 0,
      l = 0,
      unit = "pt"
    )
  ) + guides(color=guide_legend(nrow=3,byrow=TRUE)) +  
  xlab("seconds") + 
  ggsave("plot_time.pdf", width = 5, height = 6, units = "in")


## plot spec - sens - fp - fn
ggplot(data = data[stat %in% c("sens", "fn", "spec", "fp") & classifier %in% classifiers], aes(y = data, x = value, 
                                                                                                          group = classifier, color = classifier)) + 
  geom_jitter(height = 0.2, width = 0, alpha = 0.5) + facet_grid(cols = vars(stat), scales = "free") + 
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30),
    legend.box.spacing = unit(0.5, "lines"),
    legend.box.margin = ggplot2::margin(
      t = 0,
      r = 5,
      b = 0,
      l = 0,
      unit = "pt"
    ),
    plot.margin = ggplot2::margin(
      t = 0,
      r = 5,
      b = 0,
      l = 0,
      unit = "pt"
    )
  ) + xlab("") +  
  ggsave("plot_confusion_matrix.pdf", width = 7, height = 6, units = "in")




## plot auc - cutoff - precision - f1
ggplot(data = data[stat %in% c("auc", "cutoff", "precision", "f1") & classifier %in% classifiers], aes(y = data, x = value, 
                                                                                               group = classifier, color = classifier)) + 
  geom_jitter(height = 0.2, width = 0, alpha = 0.5) + facet_grid(cols = vars(stat), scales = "free") + 
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30),
    legend.box.spacing = unit(0.5, "lines"),
    legend.box.margin = ggplot2::margin(
      t = 0,
      r = 5,
      b = 0,
      l = 0,
      unit = "pt"
    ),
    plot.margin = ggplot2::margin(
      t = 0,
      r = 5,
      b = 0,
      l = 0,
      unit = "pt"
    )
  ) + xlab("") +  
  ggsave("plot_auc_cutoff_precision_f1.pdf", width = 7, height = 6, units = "in")
