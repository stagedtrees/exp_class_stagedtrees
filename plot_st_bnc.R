library(ggplot2)
library(data.table)

## read .rds files
TABLE <- readRDS("TABLE.rds")
dataALL <- as.data.table(TABLE)
AVG <- apply(TABLE, c(1,2,3), median, na.rm = TRUE)
data <- as.data.table(AVG)

classifiers <- c("st_bhc_3db",
                 "st_bhc_5db",
                 "st_bhc_tan_cl",
                 "st_bhc_tan_hc",
                 "st_kmeans_cmi",
                 "st_hclust_cmi",
                 "bnc_3db", 
                 "bnc_5db", 
                 "bnc_tan_hc",
                 "bnc_tan_cl",
                 "rf_1")

colors <- colorspace::qualitative_hcl(n = 11, palette = "dark3")
names(colors) <- classifiers
colors["rf_1"] <- "gray54"

data$data[data$data == "breast_cancer_wisconsin"] <- "wisconsin"

## plot accuracies st vs bnc (kdb)
ggplot(data = data[stat %in% c("accuracy", "time") &
                   classifier %in% c("st_bhc_3db", "st_bhc_5db", "bnc_3db", "bnc_5db")], 
       aes(x = data, y = value, 
           color = classifier, fill = classifier)) + 
  geom_point(position = position_dodge(width = 0.5)) +
  facet_grid(cols = vars(data), rows = vars(stat), scales = "free") + 
  scale_color_manual(values = colors) + 
  scale_fill_manual(values = colors) + 
  scale_y_log10() +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 8, angle = 90),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
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
  ) + xlab("") + guides(color=guide_legend(nrow = 1,byrow=TRUE)) 
ggsave("plot_accuracy_bnc_vs_refined_kdb.pdf",  width = 7, height = 4, units = "in")


# plot f1 st vs bnc (kdb)
ggplot(data = data[stat %in% c("balanced_accuracy", "f1", "precision") &
                     classifier %in% c("st_bhc_3db", "st_bhc_5db", "bnc_3db", "bnc_5db")], 
       aes(x = data, y = value, 
           color = classifier, fill = classifier)) + 
  geom_point(position = position_dodge(width = 0.5)) +
  facet_grid(cols = vars(data), rows = vars(stat), scales = "free") + 
  scale_color_manual(values = colors) + 
  scale_fill_manual(values = colors) + 
  scale_y_log10() +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 8, angle = 90),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
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
  ) + xlab("") + guides(color=guide_legend(nrow = 1,byrow=TRUE)) 
ggsave("plot_f1_bnc_vs_refined_kdb.pdf",  width = 7, height = 4, units = "in")

ggplot(data = data[stat %in% c("accuracy", "time") &
                     classifier %in%c("st_bhc_tan_cl", "st_bhc_tan_hc", "bnc_tan_cl", "bnc_tan_hc")], 
       aes(x = data, y = value, 
           color = classifier, fill = classifier)) + 
  geom_point(position = position_dodge(width = 0.5)) +
  facet_grid(cols = vars(data), rows = vars(stat), scales = "free") + 
  scale_color_manual(values = colors) + 
  scale_fill_manual(values = colors) + 
  scale_y_log10() +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 8, angle = 90),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x =  element_blank(),
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
  ) + xlab("") + guides(color=guide_legend(nrow = 1,byrow=TRUE)) 
ggsave("plot_accuracy_bnc_vs_refined_tan.pdf",  width = 7, height = 4, units = "in")


ggplot(data = data[stat %in%  c("balanced_accuracy", "f1", "precision") &
                     classifier %in%c("st_bhc_tan_cl", "st_bhc_tan_hc", "bnc_tan_cl", "bnc_tan_hc")], 
       aes(x = data, y = value, 
           color = classifier, fill = classifier)) + 
  geom_point(position = position_dodge(width = 0.5)) +
  facet_grid(cols = vars(data), rows = vars(stat), scales = "free") + 
  scale_color_manual(values = colors) + 
  scale_fill_manual(values = colors) + 
  scale_y_log10() +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 8, angle = 90),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x =  element_blank(),
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
  ) + xlab("") + guides(color=guide_legend(nrow = 1,byrow=TRUE)) 
ggsave("plot_f1_bnc_vs_refined_tan.pdf",  width = 7, height = 4, units = "in")


ggplot(data = data[stat %in% c("accuracy", "time") &
                     classifier %in%c("st_bhc_tan_cl", "st_bhc_tan_hc",
                                      "st_bhc_3db", "st_bhc_5db", "st_kmeans_cmi", "rf_1")], 
       aes(x = data, y = value, 
           color = classifier, fill = classifier)) + 
  geom_point(position = position_dodge(width = 0.5)) +
  facet_grid(cols = vars(data), rows = vars(stat), scales = "free") + 
  scale_color_manual(values = colors) + 
  scale_fill_manual(values = colors) + 
  scale_y_log10() +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 8, angle = 90),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x =  element_blank(),
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
  ) + xlab("") + guides(color=guide_legend(nrow = 1,byrow=TRUE)) 
ggsave("plot_accuracy_st.pdf",  width = 7, height = 4, units = "in")

## plot time
ggplot(data = data[stat == "time"], aes(x = value, y = data, 
                                               group = classifier, color = classifier)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  facet_grid(rows = vars(data), scales = "free") + 
  theme_bw() +
  scale_x_log10()+
  theme(
    strip.text.y = element_blank(),
    legend.position = "right",
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
  ) + guides(color=guide_legend(ncol = 1,byrow=TRUE)) +  
  xlab("seconds")
ggsave("plot_time.pdf",  width = 7, height = 5, units = "in")





library(xtable)

print(xtable(t(AVG['accuracy',,]), label = "tab:acc", digits = 4,
             caption = "Accuracy for all the considered classifiers over the datasets in the experiments."),
      floating = FALSE, 
      file = "accuracy_table.tex", booktabs = TRUE, scalebox = 0.5)
print(xtable(t(AVG['balanced_accuracy',,]), label = "tab:bacc", digits = 4,
             caption = "Balanced accuracy for all the considered classifiers over the datasets in the experiments."),
      floating = FALSE, 
      file = "balanced_accuracy_table.tex", booktabs = TRUE, scalebox = 0.5)


