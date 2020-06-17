

## how to 

* `Rscript run_classifiers.R` runs all the defined classifiers (in `methods.R`) over
   all datasets (by default now only on the fast binary datasets). 
   You can pass optional arguments, in that case the format is: 
   ```
   Rscript run_classifier.R data class1 class2 class3 ... 
   ```
   where `data` can be the name of one of the datasets (e.g. `Asym`) or the 
   name of a rds file containing a list of datasets 
   (e.g. `binary_fast_datasets_names.rds`). The arguments `class1 class2 ... `  
   are identifiers of classifiers: the name of a method (e.g. `bnc_nb` for the 
   naive bayes implemented in bnclassify) or the name of a family of methods such as
   `bnc_` in that case all classifiers in the `bnc_` family will be executed (the 
    final `_` is important!)   

    so for example the following command will execute all methods in the `st_` family 
    (stagedtrees) plus the `simple` classifier over the `Titanic` dataset: 

    ```
    Rscript run_classifiers.R Titanic st_ simple 
    ```
     

*  Running `aggregate.R` the available results will be evaluated according
to the measures defined in `statistics.R` and saved in a multidimensional array 
`TABLE.rds`

* The script `plot.R` takes the aggregated table `TABLE.rds` and produces plots. 


### methods 

#### st_methods (stagedtrees) 

*  `st_full` 
*  `st_indep`  
*  `st_hc_indep`
*  `st_fbhc`
*  `st_bhc` 
*  `st_bj_kl`
*  `st_bj_tv`
*  `st_bj_cd` 
*  `st_naive`

#### bn_methods (bnlearn)

*  `bn_tabu` 

#### bnc_methods (bnclassify)

*  `bnc_nb`
*  `bnc_tan_cl` 
*  `bnc_tan_hc`
*  `bnc_fssj`
*  `bnc_bsej`
*  `bnc_3db` 

#### nnet_methods (nnet)

*  `nnet_basic`

#### glm_methods (glm)

*  `glm_binomial`

#### simple

* `simple`

