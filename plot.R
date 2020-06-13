library(ggplot2)
library(data.table)

### here select which methods to plot
classifiers <- c(
  "st_fbhc", ## st_methods (stagedtrees) "st_hc_indep",
  "st_bj_kl", 
  "st_naive",### st_methods  (stagedtrees)
  #"bn_tabu", ## bn_methods (bnlearn)
  "bnc_nb", 
  "bnc_tan_cl", 
  #"bnc_tan_hc", "bnc_fssj", "bnc_bsej",  ## bnc_methods (bnclassify) 
  "bnc_3db", # "bnc_4db",  ## bnc_methods (bnclassify)
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
  geom_jitter(height = 0.2, width = 0) + facet_grid(cols = vars(stat), scales = "free") + 
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
  ) +  
  ggsave("plot.pdf", width = 7, height = 6, units = "in")



## plot time
ggplot(data = data[stat == "time" & classifier %in% classifiers], aes(y = data, x = value, 
                                                                      group = classifier, color = classifier)) + 
  geom_jitter(height = 0.2, width = 0) + 
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
  ) +  
  ggsave("plot_time.pdf", width = 4, height = 6, units = "in")
