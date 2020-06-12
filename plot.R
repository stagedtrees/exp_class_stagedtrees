library(ggplot2)
library(data.table)


TABLE <- readRDS("TABLE.rds")
AVG <- apply(TABLE, c(1,2,3), mean, na.rm = TRUE)

data <- as.data.table(AVG)
ggplot(data = data, aes(y = data, x = value, 
                        group = classifier, color = classifier)) + 
  geom_point() + facet_grid(rows = vars(stat))
