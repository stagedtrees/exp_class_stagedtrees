## what it is

This repository contains the code to run experiments on the benchmarking and evaluations 
of staged event tree classifiers. 

The code was used in the following publications:

<details> 

<summary> 
Leonelli, M. and  Varando, G.. (2024). 
Context-Specific Refinements of Bayesian Network Classifiers.
<i>Proceedings of The 12th International Conference on Probabilistic Graphical Models</i>
</summary>

```
  
@InProceedings{leonelli24context,
  title = 	 {Context-Specific Refinements of Bayesian Network Classifiers},
  author =       {Leonelli, Manuele and Varando, Gherardo},
  booktitle = 	 {Proceedings of The 12th International Conference on Probabilistic Graphical Models},
  pages = 	 {182--198},
  year = 	 {2024},
  editor = 	 {Kwisthout, Johan and Renooij, Silja},
  volume = 	 {246},
  series = 	 {Proceedings of Machine Learning Research},
  month = 	 {11--13 Sep},
  publisher =    {PMLR},
  pdf = 	 {https://raw.githubusercontent.com/mlresearch/v246/main/assets/leonelli24a/leonelli24a.pdf},
  url = 	 {https://proceedings.mlr.press/v246/leonelli24a.html}
}
```  

</details>

<details> 

<summary> Carli F., Leonelli M., Varando G. (2023) 
A new class of generative classifiers based on staged tree models
_Knowledge-Based Systems_

</summary>

```
@article{carli2023new,
title = {A new class of generative classifiers based on staged tree models},
journal = {Knowledge-Based Systems},
volume = {268},
pages = {110488},
year = {2023},
issn = {0950-7051},
doi = {https://doi.org/10.1016/j.knosys.2023.110488},
url = {https://www.sciencedirect.com/science/article/pii/S0950705123002381},
author = {Federico Carli and Manuele Leonelli and Gherardo Varando},
keywords = {Bayesian networks, Model selection, Staged trees, Statistical classification},
abstract = {Generative models for classification use the joint probability distribution of the class variable and the features to construct a decision rule. Among generative models, Bayesian networks and naive Bayes classifiers are the most commonly used and provide a clear graphical representation of the relationship among all variables. However, these have the disadvantage of highly restricting the type of relationships that could exist, by not allowing for context-specific independence. Here we introduce a new class of generative classifiers, called staged tree classifiers, which formally account for context-specific independence. They are constructed by a partitioning of the vertices of an event tree from which conditional independence can be formally read. The naive staged tree classifier is also defined, which extends the classic naive Bayes classifier whilst retaining the same complexity. An extensive simulation study shows that the classification accuracy of staged tree classifiers is competitive with that of state-of-the-art classifiers and an example showcases their use in practice.}
}
```

</details>


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
