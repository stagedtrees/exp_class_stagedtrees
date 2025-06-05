datasets <- factor(read.table("binary_datasets_names.tsv")[, 1])
source("methods.R")
library("cli")

set.seed(2020)
results_5db <- list()
for (d in datasets){
  message(d)
  timestamp()
  data <- readRDS(paste0("datasets/", d, ".rds"))
  kdbk <- min(5, ncol(data)-1)
  model_bnc <- bnclassify::kdb(class = "answer", dataset = data, 
                           k = 5, smooth = 1, kdbk = kdbk)
  model_bnc <- bnclassify::lp(model_bnc, dataset = data, smooth = 1)
  
  model_st <- as_sevt(bnlearn::as.bn.fit(bnclassify::as_grain(model_bnc)))
  model_st <- stages_bhc(sevt_fit(model_st, data, lambda = 1))
  
  npar_bnc <- bnclassify::nparams(model_bnc)
  npar_st <- attr(logLik(model_st), "df")
  results_5db[[d]] <- c(npar_st / npar_bnc) 
}

set.seed(2020)
results_tan_cl <- list()
for (d in datasets){
  message(d)
  timestamp()
  data <- readRDS(paste0("datasets/", d, ".rds"))
  model_bnc <- bnclassify::tan_cl(class = "answer", dataset = data)
  model_bnc <- bnclassify::lp(model_bnc, dataset = data, smooth = 1)
  
  model_st <- as_sevt(bnlearn::as.bn.fit(bnclassify::as_grain(model_bnc)))
  model_st <- stages_bhc(sevt_fit(model_st, data, lambda = 1))
  
  npar_bnc <- bnclassify::nparams(model_bnc)
  npar_st <- attr(logLik(model_st), "df")
  results_tan_cl[[d]] <- c(npar_st / npar_bnc) 
}

library("ggplot2")

DD <- reshape2::melt(list("5db" = results_5db, "tan_cl" = results_tan_cl))


ggplot(DD) + geom_point(aes(x = L2, y = 1 - value)) + 
  facet_grid(rows = vars(L1), cols = vars(L2), scales = "free") + theme_bw() +
  theme(
    strip.text.x = element_text(size = 8, angle = 90),
    legend.position = "bottom",
    legend.title = element_blank(),
    #axis.title.y = element_blank(),
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
  ) + xlab("") + ylab("1 - (#params st) / (#params bnc)") 
ggsave("asymmetry_ratio.pdf",   width = 7, height = 4, units = "in")
