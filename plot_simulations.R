library(ggplot2)
library(reshape2)

classifiers <- c("st_bhc_3db",
                 "st_bhc_5db",
                 "st_bhc_tan_cl",
                 "st_bhc_tan_hc",
                 "st_kmeans_cmi",
                 "st_hclust_cmi",
                 "bnc_3db", 
                 "bnc_5db", 
                 "bnc_tan_hc",
                 "bnc_tan_cl")

colors <- colorspace::qualitative_hcl(n = 11, palette = "dark3")
names(colors) <- classifiers

TABLE <- readRDS("TABLE_SIMULATION.rds")
AVG <- apply(TABLE, 1:4, mean, na.rm = TRUE)
SD <- apply(TABLE, 1:4, sd, na.rm = TRUE)
#AVG['time',,,] <- log(AVG['time',,,], base = 10)
dd <- melt(AVG,value.name = "value", na.rm = TRUE)
Q1 <- apply(TABLE, 1:4, quantile, probs = 0.05, na.rm = TRUE)
Q2 <- apply(TABLE, 1:4, quantile, probs = 0.95, na.rm = TRUE)
dd$q1 <- melt(Q1,value.name = "q1", na.rm = TRUE)$q1
dd$q2 <- melt(Q2,value.name = "q2", na.rm = TRUE)$q2
dd$gen = "random_sevt"

TABLE <- readRDS("TABLE_SIMULATION_LINEAR.rds")
AVG <- apply(TABLE, 1:4, mean, na.rm = TRUE)
ddl <- melt(AVG,value.name = "value", na.rm = TRUE)
Q1 <- apply(TABLE, 1:4, quantile, probs = 0.05, na.rm = TRUE)
Q2 <- apply(TABLE, 1:4, quantile, probs = 0.95, na.rm = TRUE)
ddl$q1 <- melt(Q1,value.name = "q1", na.rm = TRUE)$q1
ddl$q2 <- melt(Q2,value.name = "q2", na.rm = TRUE)$q2
ddl$gen = "linear"

TABLE <- readRDS("TABLE_SIMULATION_XOR.rds")
AVG <- apply(TABLE, 1:4, mean, na.rm = TRUE)
ddx <- melt(AVG,value.name = "value", na.rm = TRUE)
Q1 <- apply(TABLE, 1:4, quantile, probs = 0.05, na.rm = TRUE)
Q2 <- apply(TABLE, 1:4, quantile, probs = 0.95, na.rm = TRUE)
ddx$q1 <- melt(Q1,value.name = "q1", na.rm = TRUE)$q1
ddx$q2 <- melt(Q2,value.name = "q2", na.rm = TRUE)$q2
ddx$gen = "xor"

alld <- rbind(dd, ddl, ddx)
alld$gen <- factor(alld$gen, levels = c("random_sevt", "linear", "xor"))
#levels(alld$stat)[1] <- "log-time"
alld$stat <- factor(alld$stat, levels = c("accuracy", "f1", "time"))
ggplot(alld[alld$classifier %in% classifiers,]) + geom_line(aes(x = p, y = value, group = classifier, color = classifier)) + 
  #geom_ribbon(aes(x = p, ymin = q1, ymax = q2, 
  #                group = classifier, fill = classifier), alpha = 0.2, color = NA) +
  facet_grid(rows = vars(stat), cols = vars(gen),  scales = "free") + theme_bw() + ylab("") + 
  #scale_color_brewer(type = "qual") + 
  scale_color_manual(values = colors) + 
  #scale_fill_manual(values = colors) + 
  #scale_y_log10() +
  #scale_color_viridis_d()+
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave(filename = "res_simulation.pdf", width = 7, height = 4)
