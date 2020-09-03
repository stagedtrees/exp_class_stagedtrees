

## how to 

* `Rscript run_classifiers.R` runs all the defined classifiers (in `methods.R`) over
   all datasets (by default now only on the fast binary datasets). 
   You can pass optional arguments, in that case the format is: 
   ```
   Rscript run_classifier.R DATA CLASS1 CLASS2 CLASS3 ... 
   ```
   where `DATA` can be the name of one of the datasets (e.g. `Asym`) or the 
   name of a tsv file containing a list of datasets 
   (e.g. `binary_fast_datasets_names.tsv`). The arguments `CLASS1 CLASS2 ... `  
   are identifiers of classifiers: the name of a method (e.g. `bnc_nb` for the 
   naive bayes implemented in bnclassify) or the name of a family of methods such as
   `bnc_` in that case all classifiers in the `bnc_` family will be executed (the 
    final `_` is important!)   

    examples : 

    * run all methods in the `st_` family 
    (stagedtrees) and the `simple` classifier over the `Titanic` dataset: 

    ```
    Rscript run_classifiers.R Titanic st_ simple 
    ```

    * run the `simple` classifier over all the datasets:
    ```
    Rscript run_classifiers.R datasets_names.tsv simple 
    ``` 
     

*  Running `aggregate.R` the available results will be evaluated according
to the measures defined in `statistics.R` and saved in a multidimensional array 
`TABLE.rds`. Values for plotting roc curves are saved into the 
multidimensional array `ROC_CURVES.rds`.

* The script `plot.R` takes the aggregated tables `TABLE.rds` and `ROC_CURVES.rds` and produces plots. 

See the [available methods in METHODS.md](METHODS.md) and the available datasets in 
[datasets_names.tsv](datasets_names.tsv).  


### order experiments

* `Rscript run_order.R DATA st_naive_order st_fbhc_order` 

* `Rscript aggregate_order.R DATA` 

* `Rscript plot_order.R DATA` 

where `DATA` is the name of one of the datasets.
