library(ggplot2)
library(data.table)

dataset <- "monks1"

TABLE <- readRDS(paste0("TABLE_ORDER_", dataset, ".rds"))

## compute averages
AVG <- apply(TABLE, c(1,2,3), mean, na.rm = TRUE)
## transform to data.table
data <- as.data.table(AVG)


ggplot(data = data[stat %in% c("accuracy")], 
       aes(value, group = classifier, color = classifier)) + 
  geom_density() + 
  theme_bw() +  
  ggsave(paste0("plot_accuracy_order_",dataset,"_",".pdf"), 
	 width = 7, height = 6, units = "in")
