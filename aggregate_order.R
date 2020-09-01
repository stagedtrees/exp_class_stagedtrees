dataset <- "monks1"
classifiers <- c(
		 "st_naive_order",
		 "st_fbhc_order"
		 )

source("statistics.R")
nreps <- 10


res_path <- paste0("results/", dataset, "/")
data <- readRDS(paste0("datasets/", dataset, ".rds"))
split_path <- paste0("splits/", dataset, "/")
n_orders <- factorial(ncol(data)-1)
TABLE <- array(
	       data = NA,
	       dim = c(
		       length(statistics) - 1,
		       length(classifiers),
                       n_orders,
		       nreps
		       ),
	       dimnames = list(
			       stat = statistics[-length(statistics)],
			       classifier = classifiers,
			       order = 1:n_orders,
			       rep = 1:nreps
			       )
	       )


for (r in 1:nreps) {  
	id_test <- readRDS(paste0(split_path, r, "_id_test.rds"))
	true <- data$answer[id_test]
	for (c_name in classifiers) {
		for (k in 1:n_orders){
			filename <- paste0(res_path, c_name, "_",k, "_", r, ".rds" )
			if (file.exists(filename)){
				res <- readRDS(filename)
				for (stat in statistics[-length(statistics)]){
					stat_fun <- get(stat)
					TABLE[stat, c_name, k, r] <- stat_fun(res, true)
				}
			}
		}
	}
}

saveRDS(TABLE, paste0("TABLE_ORDER_",dataset,  ".rds"))

