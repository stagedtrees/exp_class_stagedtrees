library(ggplot2)
library(data.table)

datasets <- factor(read.table("binary_datasets_names_final.tsv")[, 1])

nreps <- 10

### select which methods to plot
classifiers <- c("st_full_cmi", 
                 "st_indep_cmi", 
                 "st_hc_indep_cmi_7", 
                 "st_hc_full_cmi_5", 
                 "st_bhc_cmi_7", 
                 "st_bj_kl_01_cmi", 
                 "st_naive_cmi", 
                 "st_kmeans_cmi", 
                 "bnc_3db", 
                 "bnc_nb", 
                 "bnc_tan_hc",
                 "nnet_1", "rf_1", 
                 "logistic_basic", 
                 "cl_tree_1" 
                 )

good  <- c(
	         "st_full", 
                 "st_indep", 
                 "st_hc_indep", 
                 "st_hc_full", 
                 "st_bhc", 
                 "st_bj_01", 
                 "st_naive_hc", 
                 "st_naive_km", 
                 "bnc_3db", 
                 "bnc_nb", 
                 "bnc_tan",
                 "nnet", "rf", 
                 "logistic", 
                 "ctree"#, 
                 )


## read .rds files
AVG <- readRDS("AVG.rds")
AVG <- AVG[,,classifiers] 
dimnames(AVG)[3] <- list(classifier = good) 
data <- as.data.table(AVG)

## plot accuracies
PLT <- ggplot(data = data[stat %in% c("auc", "balanced_accuracy")], 
	      aes(y = data, x = value, shape = classifier, 
                        group = classifier, color = classifier)) + 
  geom_jitter(height = 0.2, width = 0, alpha = 0.5) + facet_grid(cols = vars(stat), scales = "free") + 
  scale_colour_discrete(name = "Classifier", 
		     labels = good) + 
  scale_shape_manual(name = "Classifier", 
              labels = good,
	      values = c(rep(15, 9), rep(19, 7))) + 
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
  ) + xlab("") 
ggsave("plot_accuracy.pdf", PLT,  width = 7, height = 6, units = "in")



## plot time
PLT <- ggplot(data = data[stat == "time"], aes(y = data, x = value, 
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
  xlab("seconds")

ggsave("plot_time.pdf", PLT, width = 5, height = 6, units = "in")


## plot spec - sens - fp - fn
PLT <- ggplot(data = data[stat %in% c("sens", "fn", "spec", "fp")], aes(y = data, x = value, 
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
  ) + xlab("")

ggsave("plot_confusion_matrix.pdf", PLT, width = 7, height = 6, units = "in")




## plot auc - precision - f1
PLT <- ggplot(data = data[stat %in% c("auc", "precision", "f1")], aes(y = data, x = value, 
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
  ) + xlab("")
 ggsave("plot_auc_precision_f1.pdf", PLT, width = 7, height = 6, units = "in")




library(xtable)

print(xtable(t(AVG['accuracy',, good]), label = "tab:acc", digits = 4,
	     caption = "Accuracy for all the considered classifiers over the datasets in the experiments."),
      floating = FALSE, 
      file = "accuracy_table.tex", booktabs = TRUE, scalebox = 0.5)
print(xtable(t(AVG['balanced_accuracy',, good]), label = "tab:bacc", digits = 4,
caption = "Balanced accuracy for all the considered classifiers over the datasets in the experiments."),
      floating = FALSE, 
      file = "balanced_accuracy_table.tex", booktabs = TRUE, scalebox = 0.5)
print(xtable(t(AVG['auc',, good]), label = "tab:auc", digits = 4,
caption = "Area under the ROC curve for all the considered classifiers over the datasets in the experiments."),
      floating = FALSE,
      file = "auc_table.tex", booktabs = TRUE, scalebox = 0.5)
