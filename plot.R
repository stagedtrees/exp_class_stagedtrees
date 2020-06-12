library(ggplot2)
library(data.table)

classifiers <- c(
  "st_fbhc", ## st_methods (stagedtrees) "st_hc_indep",
  "st_bj_kl", 
  "st_naive",### st_methods  (stagedtrees)
  #"bn_tabu", ## bn_methods (bnlearn)
  "bnc_nb", "bnc_tan_cl", "bnc_tan_hc", "bnc_fssj", "bnc_bsej",  ## bnc_methods (bnclassify) 
  "bnc_3db", # "bnc_4db",  ## bnc_methods (bnclassify)
  "simple" ## simple
)
TABLE <- readRDS("TABLE.rds")
AVG <- apply(TABLE, c(1,2,3), mean, na.rm = TRUE)


data <- as.data.table(AVG)
ggplot(data = data[classifier %in% classifiers], aes(y = data, x = value, 
                        group = classifier, color = classifier)) + 
  geom_jitter(height = 0.2, width = 0) + facet_grid(cols = vars(stat), scales = "free") + 
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
  )+ 
  ggsave("plot.pdf", width = 6, height = 6, units = "in")
