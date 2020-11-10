args <- commandArgs(trailingOnly = TRUE)
dataset <- "monks1"
if (length(args) > 0){
    dataset <- args[1]
}
library(ggplot2)
library(data.table)


TABLE <- readRDS(paste0("TABLE_ORDER_", dataset, ".rds"))

## compute averages
AVG <- apply(TABLE, c(1,2,3), mean, na.rm = TRUE)
## transform to data.table
data <- as.data.table(AVG)

classifiers_cmi <- sub("order", "cmi" ,unique(data$classifier))
classifiers_ch <-  sub("order", "ch" ,unique(data$classifier))
classifiers <- c(classifiers_cmi, classifiers_ch)
AVG_OR <- readRDS("AVG.rds")
data_cmich <- as.data.table(AVG_OR)[data == dataset & 
				    stat == "accuracy" & 
				    classifier %in% classifiers]

ggplot(data = data[stat %in% c("accuracy")], 
       aes(y = sub("_order","",classifier), x = value, 
	   group = sub("_order","",classifier))) + 
  geom_violin() + 
#  geom_boxplot() + 
  geom_point(aes(y = sub("_cmi|_ch", "", classifier), 
                 x = value, color = sapply(strsplit(classifier, "_"), tail, n = 1), 
                 shape = sapply(strsplit(classifier, "_"), tail, n = 1)),
             size = 3,
             data = data_cmich, show.legend =  TRUE) + 
  theme_bw() +  
  #theme(axis.ticks.y = element_blank(), 
	#axis.text.y = element_blank()) + 
  ylab("") + 
  xlab("accuracy") + 
  labs(color = "ordering", shape = "ordering") + 
  ggsave(paste0("plot_accuracy_order_",dataset,"_",".pdf"), 
	 width = 7, height = 6, units = "in")
