# script performs Multi-Group Confirmatory Factor Analysis to test for
# measurement equivalence 

# only uses questions A2-A5. Only cases with answers to all 4 questions are used

# --------------------------------------------------------------------------------------
  
# packages
pacman::p_load(haven, tidyverse, lavaan, semTools, irr, psych)

# read data
rm(list=ls())
setwd("//A0002017.iab.baintern.de/Benutzer/SchmitzR015/Eigene Dateien/Non_prob/Prog")

NonPropData <- read_dta("Z:/Eigene Dateien/Non_prob/Data/NonPropData_CalWeighted_v09022025.dta")
SoepConsData <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData.dta")

soepConsData_selfMode <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData_selfMode.dta")
soepConsData_interviewerMode <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData_interviewerMode.dta")
NonPropData_fullcase <- read_dta("Z:/Eigene Dateien/Non_prob/Data/NonPropData_CompleteCases_CalWeighted_v26032025.dta")
NonPropData_DemogWeights <- read_dta("Z:/Eigene Dateien/Non_prob/Data/NonPropData_CompleteCases_onlyDemog_CalWeighted_v30042025.dta")

# function to extract indicators from list
fit_extract <- function(list_models) {
  coef <- c("chisq", "df", "pvalue",
            "cfi", "rmsea")
  
  
  map(list_models, function(x) fitMeasures(x)[coef]) %>%
    reduce(rbind) %>% tibble::as_tibble() %>%
    mutate(model = names(list_models),
           across(where(is.numeric),~round(.,3))) %>%
    dplyr::select(model, everything())
  
}


# -----------------------------------------------------------------------------------
  
# Datatransformation

# transform scale of Soep Data from 0-10 to 1-5
SoepConsData <- SoepConsData %>%
  filter(if_all(A2:A5, ~!is.na(.)))%>% # keep only full cases
  mutate(across(A1:A5, ~ case_when(. %in% c(0,1) ~ 1,
                                   . %in% c(2,3) ~ 2,
                                   . %in% c(4,5,6) ~ 3,
                                   . %in% c(7,8) ~ 4,
                                   . %in% c(9,10) ~ 5)),
         survey = "soep") %>%
  left_join(soepConsData_selfMode[, c("pid", "W_SOEP_self")], by = "pid") %>%
  left_join(soepConsData_interviewerMode[, c("pid", "W_SOEP_interview")], by = "pid") %>%
  rename(weight = W_SOEP) %>%
  mutate(weight_trim = weight,
         weight_demo = weight,
         weight_demo_trim = weight,
         weight_modes = coalesce(W_SOEP_interview, W_SOEP_self))
  

# adjust format of nonprob data
NonPropData_wide <- NonPropData %>%
  dplyr::select(ID, Answer, Question)%>%
  pivot_wider(names_from = Question, values_from = Answer, names_prefix = "A") %>%
  right_join(NonPropData_fullcase, by = "ID") %>%
  rename(weight = "calweight",weight_trim = "calweight_trim", weight_std = "calweight_std") %>%
  left_join(NonPropData_DemogWeights, by = "ID") %>%
  rename(weight_demo = "calweight",weight_demo_trim = "calweight_trim", weight_demo_std = "calweight_std") %>%
  mutate(survey = "civey",
         weight_modes = weight,
         mode = 7)


# merge both df
conspiracy_data_fullcase <- bind_rows(SoepConsData[,c("A2", "A3", "A4", "A5", 
                                                      "survey", "weight", "weight_trim", 
                                                      "weight_demo", "weight_demo_trim")],
                                      NonPropData_wide)

# remove rows with no answer to  A2:A5
conspiracy_data_fullcase <- conspiracy_data_fullcase[rowSums(conspiracy_data_fullcase[,1:4], na.rm = T) != 0, ]

# -------------------------------------------------------------------------------------
  
# Test scale
# calculate kappa statistic
kappa_val <- kappam.fleiss(conspiracy_data_fullcase[, 1:4])
kappa_val

# kappa of 0.222 -> not very high

# calculate cronbachs alpha
alpha_val <- alpha(conspiracy_data_fullcase[, 1:4])
alpha_val

# alpha of 0.81 -> good reliabilty! 

# -------------------------------------------------------------------------------------
# CFA --------------------------------------------------------------------------------

model <- "conspiracy_belief =~ A2 + A3 + A4 + A5"

# configural equivalence
fit_config_fullcase <- cfa(model, 
                           data = conspiracy_data_fullcase,
                           group = "survey",
                           sampling.weights = "weight",
                           estimator = "WLSMV", 
                           ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_fullcase <- cfa(model, 
                           data = conspiracy_data_fullcase,
                           group = "survey",
                           group.equal = c("loadings"),
                           sampling.weights = "weight",
                           estimator = "WLSMV", 
                           ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_fullcase <- cfa(model, 
                           data = conspiracy_data_fullcase,
                           group = "survey",
                           group.equal = c("loadings", "thresholds"), 
                           sampling.weights = "weight",
                           estimator = "WLSMV", 
                           ordered = c("A2", "A3", "A4", "A5"))

# mean equivalence
fit_mean_fullcase <- cfa(model, 
                         data = conspiracy_data_fullcase,
                         group = "survey",
                         group.equal = c("loadings", "thresholds", "means"), 
                         sampling.weights = "weight",
                         estimator = "WLSMV", 
                         ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_fullcase <- cfa(model, 
                             data = conspiracy_data_fullcase,
                             group = "survey",
                             group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                             sampling.weights = "weight",
                             estimator = "WLSMV", 
                             ordered = c("A2", "A3", "A4", "A5"))

# save fit indices
fit_extract(list(configural_equivalence = fit_config_fullcase,
                 metric_equivalence = fit_metric_fullcase,
                 scalar_equivalence = fit_scalar_fullcase,
                 mean_equivalence = fit_mean_fullcase,
                 residual_equivalence = fit_residual_fullcase)) %>%
  write_csv("./output/fit_table_fullcase.csv")


# sensitivity checks for weighting ------------------------------------------------------
# 1. no weights 
# configural equivalence
fit_config_fullcase_uw <- cfa(model, 
                           data = conspiracy_data_fullcase,
                           group = "survey",
                           estimator = "WLSMV", 
                           ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_fullcase_uw <- cfa(model, 
                           data = conspiracy_data_fullcase,
                           group = "survey",
                           group.equal = c("loadings"),
                           estimator = "WLSMV", 
                           ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_fullcase_uw <- cfa(model, 
                           data = conspiracy_data_fullcase,
                           group = "survey",
                           group.equal = c("loadings", "thresholds"), 
                           estimator = "WLSMV", 
                           ordered = c("A2", "A3", "A4", "A5"))

# mean equivalence
fit_mean_fullcase_uw <- cfa(model, 
                         data = conspiracy_data_fullcase,
                         group = "survey",
                         group.equal = c("loadings", "thresholds", "means"), 
                         estimator = "WLSMV", 
                         ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_fullcase_uw <- cfa(model, 
                             data = conspiracy_data_fullcase,
                             group = "survey",
                             group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                             estimator = "WLSMV", 
                             ordered = c("A2", "A3", "A4", "A5"))

# save fit indices
fit_extract(list(configural_equivalence = fit_config_fullcase,
                 metric_equivalence = fit_metric_fullcase_uw,
                 scalar_equivalence = fit_scalar_fullcase_uw,
                 mean_equivalence = fit_mean_fullcase_uw,
                 residual_equivalenc  = fit_residual_fullcase_uw)) %>%
  write_csv("./output/fit_table_fullcase_unweighted.csv")


# ----------------------------------------------------------------------------
# 2. with trimmed weights
fit_config_fullcase_trim <- cfa(model, 
                              data = conspiracy_data_fullcase,
                              group = "survey",
                              sampling.weights = "weight_trim",
                              estimator = "WLSMV", 
                              ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_fullcase_trim <- cfa(model, 
                              data = conspiracy_data_fullcase,
                              group = "survey",
                              group.equal = c("loadings"),
                              sampling.weights = "weight_trim",
                              estimator = "WLSMV", 
                              ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_fullcase_trim <- cfa(model, 
                              data = conspiracy_data_fullcase,
                              group = "survey",
                              group.equal = c("loadings", "thresholds"), 
                              sampling.weights = "weight_trim",
                              estimator = "WLSMV", 
                              ordered = c("A2", "A3", "A4", "A5"))

# mean equivalence
fit_mean_fullcase_trim <- cfa(model, 
                            data = conspiracy_data_fullcase,
                            group = "survey",
                            group.equal = c("loadings", "thresholds", "means"), 
                            estimator = "WLSMV", 
                            sampling.weights = "weight_trim",
                            ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_fullcase_trim <- cfa(model, 
                                data = conspiracy_data_fullcase,
                                group = "survey",
                                group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                                estimator = "WLSMV", 
                                sampling.weights = "weight_trim",
                                ordered = c("A2", "A3", "A4", "A5"))

# we still have scalar equivalence, but hit the treshold of 0.01 exactly
# so we figure out, what causes that 

# Lagrange Test to identify problematic contstraints
lavTestScore(fit_scalar_fullcase_trim)

parTable(fit_scalar_fullcase_trim)

# there are issues with multiple thresholds in all of the items, 
# especially in A2 and A4

# we first try to archive partial equivalence by allowing for the tresholds of 4 
# to be freely estimated, since we see issues ith 3 of the 4 tresholds for this question

# fit partial model
fit_partial_fullcase_trim <- cfa(model, 
                                data = conspiracy_data_fullcase,
                                group = "survey",
                                group.equal = c("loadings", "thresholds"),
                                group.partial = c("A4 | t1","A4 | t2",  "A4 | t3", "A4 | t4"),  
                                sampling.weights = "weight_trim",
                                estimator = "WLSMV", 
                                ordered = c("A2", "A3", "A4", "A5"))


# with free estimation of tresholds for A4 we archive partial equivalence                              

# save fit indices
fit_extract(list(configural_equivalence = fit_config_fullcase_trim,
                 metric_equivalence = fit_metric_fullcase_trim,
                 scalar_equivalence = fit_scalar_fullcase_trim,
                 mean_equivalence = fit_mean_fullcase_trim,
                 residual_equivalence = fit_residual_fullcase_trim)) %>%
  write_csv("./output/fit_table_fullcase_trimweight.csv")
# ----------------------------------------------------------------------------
# 3. with weights only adjusted to demographics
fit_config_fullcase_demo <- cfa(model, 
                                data = conspiracy_data_fullcase,
                                group = "survey",
                                sampling.weights = "weight_demo",
                                estimator = "WLSMV", 
                                ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_fullcase_demo <- cfa(model, 
                                data = conspiracy_data_fullcase,
                                group = "survey",
                                group.equal = c("loadings"),
                                sampling.weights = "weight_demo",
                                estimator = "WLSMV", 
                                ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_fullcase_demo <- cfa(model, 
                                data = conspiracy_data_fullcase,
                                group = "survey",
                                group.equal = c("loadings", "thresholds"), 
                                sampling.weights = "weight_demo",
                                estimator = "WLSMV", 
                                ordered = c("A2", "A3", "A4", "A5"))

# mean equivalence
fit_mean_fullcase_demo <- cfa(model, 
                              data = conspiracy_data_fullcase,
                              group = "survey",
                              group.equal = c("loadings", "thresholds", "means"), 
                              sampling.weights = "weight_demo",
                              estimator = "WLSMV", 
                              ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_fullcase_demo <- cfa(model, 
                                  data = conspiracy_data_fullcase,
                                  group = "survey",
                                  group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                                  sampling.weights = "weight_demo",
                                  estimator = "WLSMV", 
                                  ordered = c("A2", "A3", "A4", "A5"))

# save fit indices
fit_extract(list(configural_equivalence = fit_config_fullcase_demo,
                 metric_equivalence = fit_metric_fullcase_demo,
                 scalar_equivalence = fit_scalar_fullcase_demo,
                 mean_equivalence = fit_mean_fullcase_demo,
                 residual_equivalence = fit_residual_fullcase_demo)) %>%
  write_csv("./output/fit_table_fullcase_demoweight.csv")


# sensitivity checks for scaling ------------------------------------------------------
# 1. Smaller middle category
SoepConsData_t1 <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData.dta")

# transform scale of Soep Data from 0-10 to 1-5
SoepConsData_t1 <- SoepConsData_t1 %>%
  filter(if_all(A2:A5, ~!is.na(.)))%>%
  mutate(across(A1:A5, ~ case_when(. %in% c(0,1) ~ 1,
                                   . %in% c(2,3,4) ~ 2,
                                   . %in% c(5) ~ 3,
                                   . %in% c(6,7,8) ~ 4,
                                   . %in% c(9,10) ~ 5)),
         survey = "soep") %>%
  rename(weight = W_SOEP)


# merge both df
conspiracy_data_t1 <- bind_rows(SoepConsData_t1[,c("A2", "A3", "A4", "A5", "survey", "weight", "mode")],
                                NonPropData_wide)

# remove rows with no answer to  A2:A5
conspiracy_data_t1 <- conspiracy_data_t1[rowSums(conspiracy_data_t1[,1:4], na.rm = T) != 0, ]

# calculate kappa statistic
kappa_val <- kappam.fleiss(conspiracy_data_t1[, 1:4])
kappa_val

# kappa of 0.227 

# calculate cronbachs alpha
alpha_val <- alpha(conspiracy_data_t1[, 1:4])
alpha_val

# alpha of 0.81


#cfa
# configural equivalence
fit_config_t1 <- cfa(model, 
                     data = conspiracy_data_t1,
                     group = "survey",
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_t1 <- cfa(model, 
                     data = conspiracy_data_t1,
                     group = "survey",
                     group.equal = c("loadings"),
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_t1 <- cfa(model, 
                     data = conspiracy_data_t1,
                     group = "survey",
                     group.equal = c("loadings",  "thresholds"),
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"))



# mean equivalence
fit_mean_t1  <- cfa(model, 
                     data = conspiracy_data_t1,
                     group = "survey",
                     group.equal = c("loadings", "thresholds", "means"),
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_t1 <- cfa(model, 
                       data = conspiracy_data_t1,
                       group = "survey",
                       group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                       sampling.weights = "weight",
                       estimator = "WLSMV", 
                       ordered = c("A2", "A3", "A4", "A5"))

# save fit indices
fit_extract(list(configural_equivalence = fit_config_t1,
                 metric_equivalence = fit_metric_t1,
                 scalar_equivalence = fit_scalar_t1,
                 mean_equivalence = fit_mean_t1,
                 residual_equivalence = fit_residual_t1)) %>%
  write_csv("./output/fit_table_fullcase_t1.csv")


# -------------------------------------------------------------------------------------
# 2.: transform scale by using cumulative distributions
SoepConsData_t2 <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData.dta")

# scale is adjusted seperatly for  each question

# function to transform prob data based on cumulative distribution
transform_to_cumdist <- function(prob_values, nonprob_values){
  
  prob <- ecdf(prob_values)
  
  quantiles_nonprob <- quantile(nonprob_values, 
                                probs = seq(0,1, length.out = length(unique(nonprob_values))),
                                na.rm = T)
  
  transformed_prob <- sapply(prob_values, function(x){
    quantile_position <- prob(x)
    approx(seq(0,1, length.out = length(quantiles_nonprob)), 
           quantiles_nonprob, 
           xout = quantile_position, 
           rule = 2)$y
  })
  
  # round to zero digits
  transformed_prob_round <- pmin(pmax(round(transformed_prob),1),5)
  
  # ensure category 1 exists
  if(!1 %in% transformed_prob_round){
    min_index <- which.min(transformed_prob)
    transformed_prob_round[min_index] <- 1
  }
  
  return(transformed_prob_round)
}

SoepConsData_t2 <-  SoepConsData_t2 %>%
  mutate(A2 = transform_to_cumdist(A2, NonPropData_wide$A2),
         A3 = transform_to_cumdist(A3, NonPropData_wide$A3),
         A4 = transform_to_cumdist(A4, NonPropData_wide$A4),
         A5 = transform_to_cumdist(A5, NonPropData_wide$A5),
         survey = "soep") %>%
  rename(weight = W_SOEP) %>%
  mutate(weight2 = weight)

sapply(SoepConsData_t2[,c("A2", "A3", "A4", "A5")], summary)
sapply(NonPropData_wide[,c("A2", "A3", "A4", "A5")], summary)

# Problem: the quantiles do not overlap exactly, that could be due to
# the limitied amount of distinct values. The first quantile is +1 for
# most questions in the transformed soep data. Median and 3rd quantile are the same

# merge both df
conspiracy_data_t2 <- bind_rows(SoepConsData_t2[,c("A2", "A3", "A4", "A5", "survey", "weight", "weight2", "mode")],
                                NonPropData_wide)

# remove rows with no answer to  A2:A5
conspiracy_data_t2 <- conspiracy_data_t2[rowSums(conspiracy_data_t2[,1:4], na.rm = T) != 0, ]



# calculate kappa statistic
kappa_val <- kappam.fleiss(conspiracy_data_t2[, 1:4])
kappa_val

# kappa of 0.144 -> lower than before

# calculate cronbachs alpha
alpha_val <- alpha(conspiracy_data_t2[, 1:4])
alpha_val

# alpha of 0.79


#cfa
# configural equivalence
fit_config_t2 <- cfa(model, 
                     data = conspiracy_data_t2,
                     group = "survey",
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_t2 <- cfa(model, 
                     data = conspiracy_data_t2,
                     group = "survey",
                     group.equal = c("loadings"),
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_t2 <- cfa(model, 
                     data = conspiracy_data_t2,
                     group = "survey",
                     group.equal = c("loadings",  "thresholds"),
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"))



# mean equivalence
fit_mean_t2  <- cfa(model, 
                    data = conspiracy_data_t2,
                    group = "survey",
                    group.equal = c("loadings", "thresholds", "means"),
                    sampling.weights = "weight",
                    estimator = "WLSMV", 
                    ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_t2 <- cfa(model, 
                       data = conspiracy_data_t2,
                       group = "survey",
                       group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                       sampling.weights = "weight",
                       estimator = "WLSMV", 
                       ordered = c("A2", "A3", "A4", "A5"))

# save fit indices
fit_extract(list(configural_equivalence = fit_config_t2,
                 metric_equivalence = fit_metric_t2,
                 scalar_equivalence = fit_scalar_t2,
                 mean_equivalence = fit_mean_t2,
                 residual_equivalence = fit_residual_t2)) %>%
  write_csv("./output/fit_table_fullcase_t2.csv")