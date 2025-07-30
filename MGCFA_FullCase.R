# script performs Multi-Group Confirmatory Factor Analysis to test for
# measurement equivalence 

# only uses questions A2-A5. Only cases with answers to all 4 questions are used

# --------------------------------------------------------------------------------------
  
# packages
pacman::p_load(haven, tidyverse, lavaan, semTools, irr, psych, purrr, flextable, tibble, knitr, kableExtra)

# read data
rm(list=ls())
setwd("//A0002017.iab.baintern.de/Benutzer/SchmitzR015/Eigene Dateien/Non_prob/Prog")

NonPropData <- read_dta("Z:/Eigene Dateien/Non_prob/Data/NonPropData_CalWeighted_v09022025.dta")
SoepConsData <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData.dta")

soepConsData_selfMode <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData_selfMode.dta")
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
  rename(weight = W_SOEP) %>%
  mutate(weight_trim = weight,
         weight_demo = weight,
         weight_demo_trim = weight,
         weight_selfadmin = W_SOEP_self)
  

# adjust format of nonprob data
NonPropData_wide <- NonPropData %>%
  dplyr::select(ID, Answer, Question)%>%
  pivot_wider(names_from = Question, values_from = Answer, names_prefix = "A") %>%
  right_join(NonPropData_fullcase, by = "ID") %>%
  rename(weight = "calweight",weight_trim = "calweight_trim", weight_std = "calweight_std") %>%
  left_join(NonPropData_DemogWeights, by = "ID") %>%
  rename(weight_demo = "calweight",weight_demo_trim = "calweight_trim", weight_demo_std = "calweight_std") %>%
  mutate(survey = "civey",
         weight_selfadmin = weight,
         mode = 7)


# merge both df
conspiracy_data_fullcase <- bind_rows(SoepConsData[,c("A2", "A3", "A4", "A5", 
                                                      "survey", "weight", "weight_trim", 
                                                      "weight_demo", "weight_demo_trim", "weight_selfadmin", "mode")],
                                      NonPropData_wide)

# number of cases and means of A2:A5
table(conspiracy_data_fullcase$survey)
table(conspiracy_data_fullcase$mode) # CAWI = mode 2


round(colMeans(conspiracy_data_fullcase[conspiracy_data_fullcase$survey == "soep" , 1:4]),2)
round(colMeans(conspiracy_data_fullcase[conspiracy_data_fullcase$survey == "civey", 1:4]),2)
round(colMeans(conspiracy_data_fullcase[conspiracy_data_fullcase$mode == 2, 1:4]),2)
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
                           parameterization = "theta",
                           group = "survey",
                           sampling.weights = "weight",
                           estimator = "WLSMV", 
                           ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_fullcase <- cfa(model, 
                           data = conspiracy_data_fullcase,
                           group = "survey",
                           parameterization = "theta",
                           group.equal = c("loadings"),
                           sampling.weights = "weight",
                           estimator = "WLSMV", 
                           ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_fullcase <- cfa(model, 
                           data = conspiracy_data_fullcase,
                           group = "survey",
                           parameterization = "theta",
                           group.equal = c("loadings", "thresholds"), 
                           sampling.weights = "weight",
                           estimator = "WLSMV", 
                           ordered = c("A2", "A3", "A4", "A5"))

# mean equivalence
fit_mean_fullcase <- cfa(model, 
                         data = conspiracy_data_fullcase,
                         group = "survey",
                         parameterization = "theta",
                         group.equal = c("loadings", "thresholds", "means"), 
                         sampling.weights = "weight",
                         estimator = "WLSMV", 
                         ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_fullcase <- cfa(model, 
                             data = conspiracy_data_fullcase,
                             group = "survey",
                             parameterization = "theta",
                             group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                             sampling.weights = "weight",
                             estimator = "WLSMV", 
                             ordered = c("A2", "A3", "A4", "A5"))

# save fit indices
tab_fullcase <- fit_extract(list(configural = fit_config_fullcase,
                 metric = fit_metric_fullcase,
                 scalar = fit_scalar_fullcase,
                 mean = fit_mean_fullcase,
                 residual = fit_residual_fullcase)) %>%
  mutate(condition = "Main model")
  #write_csv("./output/fit_table_fullcase.csv")


# sensitivity checks for weighting ------------------------------------------------------
# 1. CAWI adjusted weights (S1 in the paper)
# configural equivalence
fit_config_fullcase_cw <- cfa(model, 
                              data = conspiracy_data_fullcase[!is.na(conspiracy_data_fullcase$weight_selfadmin),],
                              group = "survey",
                              parameterization = "theta",
                              sampling.weights = "weight_selfadmin",
                              estimator = "WLSMV", 
                              ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_fullcase_cw <- cfa(model, 
                              data = conspiracy_data_fullcase[!is.na(conspiracy_data_fullcase$weight_selfadmin),],
                              group = "survey",
                              sampling.weights = "weight_selfadmin",
                              group.equal = c("loadings"),
                              parameterization = "theta",
                              estimator = "WLSMV", 
                              ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_fullcase_cw <- cfa(model, 
                              data = conspiracy_data_fullcase[!is.na(conspiracy_data_fullcase$weight_selfadmin),],
                              group = "survey",
                              sampling.weights = "weight_selfadmin",
                              group.equal = c("loadings", "thresholds"), 
                              parameterization = "theta",
                              estimator = "WLSMV", 
                              ordered = c("A2", "A3", "A4", "A5"))

# mean equivalence
fit_mean_fullcase_cw <- cfa(model, 
                            data = conspiracy_data_fullcase[!is.na(conspiracy_data_fullcase$weight_selfadmin),],
                            group = "survey",
                            sampling.weights = "weight_selfadmin",
                            group.equal = c("loadings", "thresholds", "means"), 
                            parameterization = "theta",
                            estimator = "WLSMV", 
                            ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_fullcase_cw <- cfa(model, 
                                data = conspiracy_data_fullcase[!is.na(conspiracy_data_fullcase$weight_selfadmin),],
                                group = "survey",
                                sampling.weights = "weight_selfadmin",
                                group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                                parameterization = "theta",
                                estimator = "WLSMV", 
                                ordered = c("A2", "A3", "A4", "A5"))

# save fit indices
tab_cw<- fit_extract(list(configural = fit_config_fullcase_cw,
                          metric = fit_metric_fullcase_cw,
                          scalar = fit_scalar_fullcase_cw,
                          mean = fit_mean_fullcase_cw,
                          residual = fit_residual_fullcase_cw)) %>%
  mutate(condition = "CAWI adjusted Weights")

# ----------------------------------------------------------------------------
# 2. no weights (S2 in the paper)
# configural equivalence
fit_config_fullcase_uw <- cfa(model, 
                           data = conspiracy_data_fullcase,
                           group = "survey",
                           parameterization = "theta",
                           estimator = "WLSMV", 
                           ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_fullcase_uw <- cfa(model, 
                           data = conspiracy_data_fullcase,
                           group = "survey",
                           group.equal = c("loadings"),
                           parameterization = "theta",
                           estimator = "WLSMV", 
                           ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_fullcase_uw <- cfa(model, 
                           data = conspiracy_data_fullcase,
                           group = "survey",
                           group.equal = c("loadings", "thresholds"), 
                           parameterization = "theta",
                           estimator = "WLSMV", 
                           ordered = c("A2", "A3", "A4", "A5"))

# mean equivalence
fit_mean_fullcase_uw <- cfa(model, 
                         data = conspiracy_data_fullcase,
                         group = "survey",
                         group.equal = c("loadings", "thresholds", "means"), 
                         parameterization = "theta",
                         estimator = "WLSMV", 
                         ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_fullcase_uw <- cfa(model, 
                             data = conspiracy_data_fullcase,
                             group = "survey",
                             group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                             parameterization = "theta",
                             estimator = "WLSMV", 
                             ordered = c("A2", "A3", "A4", "A5"))

# save fit indices
tab_uw<- fit_extract(list(configural = fit_config_fullcase_uw,
                                 metric = fit_metric_fullcase_uw,
                                 scalar = fit_scalar_fullcase_uw,
                                 mean = fit_mean_fullcase_uw,
                                 residual = fit_residual_fullcase_uw)) %>%
  mutate(condition = "Unweighted")

  #write_csv("./output/fit_table_fullcase_unweighted.csv")


# ----------------------------------------------------------------------------
# 3. with trimmed weights
fit_config_fullcase_trim <- cfa(model, 
                              data = conspiracy_data_fullcase,
                              group = "survey",
                              sampling.weights = "weight_trim",
                              parameterization = "theta",
                              estimator = "WLSMV", 
                              ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_fullcase_trim <- cfa(model, 
                              data = conspiracy_data_fullcase,
                              group = "survey",
                              group.equal = c("loadings"),
                              sampling.weights = "weight_trim",
                              parameterization = "theta",
                              estimator = "WLSMV", 
                              ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_fullcase_trim <- cfa(model, 
                              data = conspiracy_data_fullcase,
                              group = "survey",
                              group.equal = c("loadings", "thresholds"), 
                              sampling.weights = "weight_trim",
                              parameterization = "theta",
                              estimator = "WLSMV", 
                              ordered = c("A2", "A3", "A4", "A5"))

# mean equivalence
fit_mean_fullcase_trim <- cfa(model, 
                            data = conspiracy_data_fullcase,
                            group = "survey",
                            group.equal = c("loadings", "thresholds", "means"), 
                            parameterization = "theta",
                            estimator = "WLSMV", 
                            sampling.weights = "weight_trim",
                            ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_fullcase_trim <- cfa(model, 
                                data = conspiracy_data_fullcase,
                                group = "survey",
                                group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                                parameterization = "theta",
                                estimator = "WLSMV", 
                                sampling.weights = "weight_trim",
                                ordered = c("A2", "A3", "A4", "A5"))


# save fit indices
tab_trim <- fit_extract(list(configural = fit_config_fullcase_trim,
                          metric = fit_metric_fullcase_trim,
                          scalar = fit_scalar_fullcase_trim,
                          mean = fit_mean_fullcase_trim,
                          residual = fit_residual_fullcase_trim)) %>%
  mutate(condition = "Trimmed weights")
  #write_csv("./output/fit_table_fullcase_trimweight.csv")

# ----------------------------------------------------------------------------
# 4. with weights only adjusted to demographics  (S3 in the paper)
fit_config_fullcase_demo <- cfa(model, 
                                data = conspiracy_data_fullcase,
                                group = "survey",
                                sampling.weights = "weight_demo",
                                parameterization = "theta",
                                estimator = "WLSMV", 
                                ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_fullcase_demo <- cfa(model, 
                                data = conspiracy_data_fullcase,
                                group = "survey",
                                group.equal = c("loadings"),
                                sampling.weights = "weight_demo",
                                parameterization = "theta",
                                estimator = "WLSMV", 
                                ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_fullcase_demo <- cfa(model, 
                                data = conspiracy_data_fullcase,
                                group = "survey",
                                group.equal = c("loadings", "thresholds"), 
                                parameterization = "theta",
                                sampling.weights = "weight_demo",
                                estimator = "WLSMV", 
                                ordered = c("A2", "A3", "A4", "A5"))

# mean equivalence
fit_mean_fullcase_demo <- cfa(model, 
                              data = conspiracy_data_fullcase,
                              group = "survey",
                              group.equal = c("loadings", "thresholds", "means"),
                              parameterization = "theta", 
                              sampling.weights = "weight_demo",
                              estimator = "WLSMV", 
                              ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_fullcase_demo <- cfa(model, 
                                  data = conspiracy_data_fullcase,
                                  group = "survey",
                                  group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                                  parameterization = "theta",
                                  sampling.weights = "weight_demo",
                                  estimator = "WLSMV", 
                                  ordered = c("A2", "A3", "A4", "A5"))

# save fit indices
tab_demo<- fit_extract(list(configural = fit_config_fullcase_demo,
                          metric = fit_metric_fullcase_demo,
                          scalar = fit_scalar_fullcase_demo,
                          mean = fit_mean_fullcase_demo,
                          residual = fit_residual_fullcase_demo)) %>%
  mutate(condition = "Demographic weights")

  #write_csv("./output/fit_table_fullcase_demoweight.csv")


# sensitivity checks for scaling ------------------------------------------------------
# 1. Smaller middle category (S4 in the paper)
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
                     parameterization = "theta",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_t1 <- cfa(model, 
                     data = conspiracy_data_t1,
                     group = "survey",
                     group.equal = c("loadings"),
                     parameterization = "theta",
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_t1 <- cfa(model, 
                     data = conspiracy_data_t1,
                     group = "survey",
                     group.equal = c("loadings",  "thresholds"),
                     parameterization = "theta",
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"))



# mean equivalence
fit_mean_t1  <- cfa(model, 
                     data = conspiracy_data_t1,
                     group = "survey",
                     group.equal = c("loadings", "thresholds", "means"),
                     parameterization = "theta",
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_t1 <- cfa(model, 
                       data = conspiracy_data_t1,
                       group = "survey",
                       group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                       parameterization = "theta",
                       sampling.weights = "weight",
                       estimator = "WLSMV", 
                       ordered = c("A2", "A3", "A4", "A5"))

# save fit indices
tab_t1 <- fit_extract(list(configural = fit_config_t1,
                          metric = fit_metric_t1,
                          scalar = fit_scalar_t1,
                          mean = fit_mean_t1,
                          residual = fit_residual_t1)) %>%
  mutate(condition = "Middle category recoded")

  #write_csv("./output/fit_table_fullcase_t1.csv")


# -------------------------------------------------------------------------------------
# 2.: transform scale by using cumulative distributions (S5 in the paper)
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
                     parameterization = "theta",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric_t2 <- cfa(model, 
                     data = conspiracy_data_t2,
                     group = "survey",
                     group.equal = c("loadings"),
                     parameterization = "theta",
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar_t2 <- cfa(model, 
                     data = conspiracy_data_t2,
                     group = "survey",
                     group.equal = c("loadings",  "thresholds"),
                     parameterization = "theta",
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"))



# mean equivalence
fit_mean_t2  <- cfa(model, 
                    data = conspiracy_data_t2,
                    group = "survey",
                    group.equal = c("loadings", "thresholds", "means"),
                    parameterization = "theta",
                    sampling.weights = "weight",
                    estimator = "WLSMV", 
                    ordered = c("A2", "A3", "A4", "A5"))

# residual equivalence
fit_residual_t2 <- cfa(model, 
                       data = conspiracy_data_t2,
                       group = "survey",
                       group.equal = c("loadings", "thresholds",  "means", "residuals"), 
                       parameterization = "theta",
                       sampling.weights = "weight",
                       estimator = "WLSMV", 
                       ordered = c("A2", "A3", "A4", "A5"))

# save fit indices
tab_t2 <- fit_extract(list(configural = fit_config_t2,
                           metric = fit_metric_t2,
                           scalar = fit_scalar_t2,
                           mean = fit_mean_t2,
                           residual = fit_residual_t2)) %>%
  mutate(condition = "Percentile-based transformation")

  #write_csv("./output/fit_table_fullcase_t2.csv")



# --------------------------------------------------------------------------------------------
# collapse results
fit_table <- bind_rows(tab_fullcase,
                       tab_cw,
                       tab_uw,
                       tab_trim,
                       tab_t1,
                       tab_t2,
                       tab_demo) %>%
  relocate(condition, model)

flextable(fit_table)

# save
write_csv(fit_table, "./output/fit_table_full.csv")

# kable
fit_table %>% 
  kable(format = "html", caption = "Table 1: Model fit indices") %>%
  kable_styling("striped", full_width = F)
