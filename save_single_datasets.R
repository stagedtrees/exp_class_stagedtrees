rm(list = ls())
load("datasets.RData")
all_names <- ls()
saveRDS(all_names, "datasets_names.rds")
saveRDS(all_names[-c(1, 5, 14, 19, 20, 22, 25, 26)], "binary_datasets_names.rds")
saveRDS(all_names[-c(1, 5, 14, 19, 20, 22, 25, 26)][c(1,3:9, 11, 13:20)], "binary_fast_datasets_names.rds")
dir.create("datasets/", showWarnings = FALSE)
for (n in all_names){
  saveRDS(get(n), file = paste0("datasets/", n, ".rds"))
}
