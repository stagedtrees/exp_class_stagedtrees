rm(list = ls())
all_names <- read.table("datasets_names.tsv")[,1]

nreps <- 20
k <- 2

splits_path <- "splits/"
dir.create(splits_path, showWarnings = FALSE)
for (nam in all_names){
  path <- paste0(splits_path, nam, "/")
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  data <- readRDS(paste0("datasets/", nam, ".rds" ))
  ## replicability dataset-wise
  set.seed(2020)
  for (r in 1:nreps){
    N <- nrow(data)
    id_test <- sample(1:N, N / k, replace = FALSE)
    saveRDS(id_test, file = paste0(path, r, "_id_test.rds"))
  }
}

