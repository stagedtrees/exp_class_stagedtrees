AVG <- readRDS("AVG.rds")
AVG_NO_CUTOFF <- readRDS("AVG_NO_CUTOFF.rds")

data <- as.data.table(AVG)
data_no_cutoff <- as.data.table(AVG_NO_CUTOFF)

datasets <- factor(read.table("binary_fast_datasets_names.tsv")[, 1])

nreps <- 10

### select which methods to plot
classifiers <- c(
  "st_full", "st_full_mi", "st_full_cmi", "st_full_ch",
  "st_indep", "st_indep_mi", "st_indep_cmi", "st_indep_ch",
  "st_hc_indep_5", "st_hc_indep_mi_5", "st_hc_indep_cmi_5", "st_hc_indep_ch_5",
  "st_hc_indep_7", "st_hc_indep_mi_7", "st_hc_indep_cmi_7", "st_hc_indep_ch_7",
  "st_hc_full_5", "st_hc_full_mi_5", "st_hc_full_cmi_5", "st_hc_full_ch_5",
  # "st_hc_full_7", "st_hc_full_mi_7", "st_hc_full_cmi_7", "st_hc_full_ch_7",
  "st_fbhc", "st_fbhc_mi", "st_fbhc_cmi", "st_fbhc_ch",
  "st_bhc_5", "st_bhc_mi_5", "st_bhc_cmi_5", "st_bhc_ch_5",
  "st_bhc_7", "st_bhc_mi_7", "st_bhc_cmi_7", "st_bhc_ch_7",
  "st_bj_kl", "st_bj_kl_mi", "st_bj_kl_cmi", "st_bj_kl_ch",
  "st_bj_tv", "st_bj_tv_mi", "st_bj_tv_cmi", "st_bj_tv_ch",
  "st_bj_cd", "st_bj_cd_mi", "st_bj_cd_cmi", "st_bj_cd_ch",
  "st_naive", "st_naive_mi", "st_naive_cmi", "st_naive_ch",
  "bn_tabu",
  "bn_hc",
  "bnc_nb",
  "bnc_tan_cl",
  "bnc_tan_hc",
  "bnc_fssj",
  "bnc_bsej",
  "bnc_3db",
  "nnet_1",
  "nnet_2",
  "rf_1",
  "rf_2",
  "glm_binomial",
  "logistic_basic",
  "naive_bayes_1",
  "naive_bayes_2",
  "cl_tree_1",
  "cl_tree_2",
  "regularized_da",
  "boosting_basic",
  "bagging_basic",
  "svm_basic",
  "gam_basic",
  "adaboost_basic",
  "simple"
)

# stat deve essere tra questi 12, ad esempio c("fp", "fn") come è sotto.
attr(AVG, "dimnames")[1]
# $stat
# [1] "time"              "accuracy"          "balanced_accuracy" "f1"                "sens"             
# [6] "spec"              "fn"                "fp"                "error"             "precision"        
# [11] "auc"               "cutoff"     

# data deve essere tra questi
attr(AVG, "dimnames")[2]
# $data
# [1] "Asym"                    "breast_cancer_wisconsin" "chestSim500"             "energy1"                
# [5] "energy2"                 "FallEld"                 "fertility"               "monks1"                 
# [9] "monks2"                  "monks3"                  "Pokemon"                 "puffin"                 
# [13] "reinis"                  "tic_tac_toe"             "Titanic"                

# classifier deve essere tra questi
classifiers
# [1] "st_full"           "st_full_mi"        "st_full_cmi"       "st_full_ch"        "st_indep"         
# [6] "st_indep_mi"       "st_indep_cmi"      "st_indep_ch"       "st_hc_indep_5"     "st_hc_indep_mi_5" 
# [11] "st_hc_indep_cmi_5" "st_hc_indep_ch_5"  "st_hc_indep_7"     "st_hc_indep_mi_7"  "st_hc_indep_cmi_7"
# [16] "st_hc_indep_ch_7"  "st_hc_full_5"      "st_hc_full_mi_5"   "st_hc_full_cmi_5"  "st_hc_full_ch_5"  
# [21] "st_fbhc"           "st_fbhc_mi"        "st_fbhc_cmi"       "st_fbhc_ch"        "st_bhc_5"         
# [26] "st_bhc_mi_5"       "st_bhc_cmi_5"      "st_bhc_ch_5"       "st_bhc_7"          "st_bhc_mi_7"      
# [31] "st_bhc_cmi_7"      "st_bhc_ch_7"       "st_bj_kl"          "st_bj_kl_mi"       "st_bj_kl_cmi"     
# [36] "st_bj_kl_ch"       "st_bj_tv"          "st_bj_tv_mi"       "st_bj_tv_cmi"      "st_bj_tv_ch"      
# [41] "st_bj_cd"          "st_bj_cd_mi"       "st_bj_cd_cmi"      "st_bj_cd_ch"       "st_naive"         
# [46] "st_naive_mi"       "st_naive_cmi"      "st_naive_ch"       "bn_tabu"           "bn_hc"            
# [51] "bnc_nb"            "bnc_tan_cl"        "bnc_tan_hc"        "bnc_fssj"          "bnc_bsej"         
# [56] "bnc_3db"           "nnet_1"            "nnet_2"            "rf_1"              "rf_2"             
# [61] "glm_binomial"      "logistic_basic"    "naive_bayes_1"     "naive_bayes_2"     "cl_tree_1"        
# [66] "cl_tree_2"         "regularized_da"    "boosting_basic"    "bagging_basic"     "svm_basic"        
# [71] "gam_basic"         "adaboost_basic"    "simple" 

# bisogna specificare sotto data in ggplot(data = ......) come:
# data[stat %in% c() & classifier %in% c() & data %in% c()]

# Esempio: grafico di falsi positivi e falsi negativi, per solo dataset Asym, 
# per i soli nostri alberi (i primi 48 dell' oggetto "classifiers"):



ggplot(data = data[stat %in% c("accuracy", "auc", "precision", "f1") &
                     classifier %in% classifiers[c(40:50)] &
                     data %in% "Asym"], aes(y = data, x = value, 
                                            group = classifier, color = classifier)) + 
  geom_jitter(height = 0.2, width = 0, alpha = 0.5) + facet_grid(cols = vars(stat), scales = "free") + 
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30),
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
  ) + xlab("")




ggplot(data = data[stat %in% c("time") &
                     classifier %in% classifiers[c(29:32)] & 
                     data %in% "breast_cancer_wisconsin"], aes(y = data, x = value, 
                                              group = classifier, color = classifier)) + 
  geom_jitter(height = 0.2, width = 0, alpha = 0.5) + facet_grid(cols = vars(stat), scales = "free") + 
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30),
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
  ) + xlab("")
