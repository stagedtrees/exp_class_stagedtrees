library(ggplot2)
library(data.table)

### select which methods to plot
classifiers <- c(
## st_methods (stagedtrees)
#   "st_full",
  "st_fbhc", 
  "st_fbhc_ch", 
  "st_fbhc_cmi", 
  "st_fbhc_mi", 
  "st_bj_kl", 
  "st_naive_ch",
  "st_naive_mi",
  "st_naive_cmi",
  "st_bhc_mi",
  "st_bhc_cmi",
  "st_hc_indep_mi",
## bn_methods (bnlearn)
#  "bn_tabu", 
## bnc_methods (bnclassify)
  "bnc_nb", 
  "bnc_tan_cl", 
#  "bnc_tan_hc", "bnc_fssj", "bnc_bsej",   
  "bnc_3db",   
   ## nnet_methods (nnet)
  "nnet_1",
  "nnet_2",
   ## glm_methods (glm)
#  "glm_binomial",
   ## rf_methods (randomForest) 
#  "rf_basic",
   ## simple 
   "simple" ## simple
)

##read TABLE.rds
TABLE <- readRDS("TABLE.rds")
## compute averages
AVG <- apply(TABLE, c(1,2,3), mean, na.rm = TRUE)

## transform to data.table
data <- as.data.table(AVG)

## plot
ggplot(data = data[stat != "time" & classifier %in% classifiers], aes(y = data, x = value, 
                        group = classifier, color = classifier)) + 
  geom_jitter(height = 0.2, width = 0, alpha = 0.5) + facet_grid(cols = vars(stat), scales = "free") + 
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30),
    legend.box.spacing = unit(0.5, "lines"),
    legend.box.margin = margin(
      t = 0,
      r = 5,
      b = 0,
      l = 0,
      unit = "pt"
    ),
    plot.margin = margin(
      t = 0,
      r = 5,
      b = 0,
      l = 0,
      unit = "pt"
    )
  ) + xlab("") +  
  ggsave("plot.pdf", width = 7, height = 6, units = "in")



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
    legend.box.margin = margin(
      t = 0,
      r = 15,
      b = 0,
      l = 0,
      unit = "pt"
    ),
    plot.margin = margin(
      t = 0,
      r = 15,
      b = 0,
      l = 0,
      unit = "pt"
    )
  ) + guides(color=guide_legend(nrow=3,byrow=TRUE)) +  
  xlab("seconds") + 
  ggsave("plot_time.pdf", width = 5, height = 6, units = "in")
