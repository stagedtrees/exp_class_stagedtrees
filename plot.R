library(ggplot2)
library(data.table)

datasets <- factor(read.table("binary_fast_datasets_names.tsv")[, 1])

nreps <- 10

### select which methods to plot
classifiers <- c("st_full_cmi", "st_indep_cmi", "st_hc_indep_cmi_5", "st_hc_indep_cmi_7", 
                 "st_hc_full_cmi_5", "st_fbhc_cmi", "st_bhc_cmi_5", "st_bhc_cmi_7", 
                 "st_bj_kl_01_cmi", "st_bj_kl_05_cmi", "st_bj_kl_20_cmi",
                 "st_naive_cmi", "st_kmeans_cmi", "bn_tabu", "bn_hc","bnc_3db", "bnc_nb", "bnc_tan_hc",
                 "nnet_1", "rf_1", "logistic_basic", "cl_tree_1", "regularized_da", "naive_bayes_1",
                 "boosting_basic", "bagging_basic", "svm_basic", "gam_basic", "adaboost_basic")

## read .rds files
AVG <- readRDS("AVG.rds")
data <- as.data.table(AVG)
AVG_ROC_CURVE <- readRDS("AVG_ROC_CURVE.rds")

# they are the same for all roc curve, so now it is reasonable to compute the mean between bootstrap iterations.
fixed_specificities <- c(0, seq(0, 0.3, length.out = 70), seq(0.3, 0.8, length.out = 24), 
                         seq(0.8, 1, length.out = 24), 1)

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
for(j in 13:16) {
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




## plot auc - precision - f1
ggplot(data = data[stat %in% c("auc", "precision", "f1") & classifier %in% classifiers], aes(y = data, x = value, 
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
  ggsave("plot_auc_precision_f1.pdf", width = 7, height = 6, units = "in")
