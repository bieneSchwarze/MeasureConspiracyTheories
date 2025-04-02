
# script performs Multi-Group Confirmatory Factor Analysis to test for measurement equivalence 
# only uses questions A2-A5

###################################################################################################

# packages
pacman::p_load(haven, tidyverse, lavaan, semTools, irr, psych)

# read data
rm(list=ls())

NonPropData <- read_dta("Z:/Eigene Dateien/Non_prob/Data/NonPropData_CalWeighted_v09022025.dta")
SoepConsData <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData.dta")

soepConsData_selfMode <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData_selfMode.dta")
soepConsData_interviewerMode <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData_interviewerMode.dta")
NonPropData_fullcase <- read_dta("Z:/Eigene Dateien/Non_prob/Data/NonPropData_CompleteCases_CalWeighted_v26032025.dta")

# function to extract indicators from list
fit_extract <- function(list_models) {
  coef <- c("chisq.scaled", "df.scaled", "pvalue.scaled",
            "cfi.scaled", "rmsea.scaled")
  
  
  map(list_models, function(x) fitMeasures(x)[coef]) %>%
    reduce(rbind) %>% tibble::as_tibble() %>%
    mutate(model = names(list_models),
           across(where(is.numeric),~round(.,3))) %>%
    select(model, everything())
  
}


######################################################################################################

# Datatransformation

# transform scale of Soep Data from 0-10 to 1-5
SoepConsData <- SoepConsData %>%
  mutate(across(A1:A5, ~ case_when(. %in% c(0,1) ~ 1,
                                   . %in% c(2,3) ~ 2,
                                   . %in% c(4,5,6) ~ 3,
                                   . %in% c(7,8) ~ 4,
                                   . %in% c(9,10) ~ 5)),
         survey = "soep") %>%
  left_join(soepConsData_selfMode[, c("pid", "W_SOEP_self")], by = "pid") %>%
  left_join(soepConsData_interviewerMode[, c("pid", "W_SOEP_interview")], by = "pid") %>%
  rename(weight = W_SOEP) %>%
  mutate(weight2 = weight,
         weight_modes = coalesce(W_SOEP_interview, W_SOEP_self))
  

# adjust format of nonprob data
NonPropData_wide <- NonPropData %>%
  select(ID, Answer, Question, calweight)%>%
  pivot_wider(names_from = Question, values_from = Answer, names_prefix = "A") %>%
  rename(weight = calweight) %>%
  mutate(survey = "civey",
         weight2 = 1,
         weight_modes = weight,
         mode = 7)

# merge both df
conspiracy_data <- bind_rows(SoepConsData[,c("A2", "A3", "A4", "A5", "survey", "weight", "weight2", "weight_modes", "mode")],
                             NonPropData_wide)

# remove rows with no answer to  A2:A5
conspiracy_data <- conspiracy_data[rowSums(conspiracy_data[,1:4], na.rm = T) != 0, ]


# calculate kappa statistic
kappa_val <- kappam.fleiss(conspiracy_data[, 1:4])
kappa_val

# kappa of 0.222 -> not very high

# calculate cronbachs alpha
alpha_val <- alpha(conspiracy_data[, 1:4])
alpha_val

# alpha of 0.8 -> good reliabilty! 
#####################################################################################################

# CFA 

model <- "conspiracy_belief =~ A2 + A3 + A4 + A5"

# simple model
fit <- cfa(model, 
           data = conspiracy_data, 
           estimator = "WLSMV", 
           ordered = c("A2", "A3", "A4", "A5"))

mi <- modindices(fit)
mi

summary(fit, fit.measures = T, standardized = T)

# comparison of prob vs. non-prob with semTools
measEq <-  measEq.syntax(configural.model = model,
                         data = conspiracy_data,
                         group = "survey",
                         ordered = c("A2", "A3", "A4", "A5"),
                         estimator = "WLSMV")

summary(measEq)


####################################################################################

# configural equivalence
fit_config <- cfa(model, 
                  data = conspiracy_data,
                  group = "survey",
                  sampling.weights = "weight",
                  estimator = "WLSMV", # because data is ordinal
                  ordered = c("A2", "A3", "A4", "A5"), #  define var as  ordinal
                  missing = "pairwise") # also use cases with missing answers to some Question
                                        # -> standard for ordinal data (other options for missing only for ML)

summary(fit_config, fit.measures = T, standardized = T)

# CFI of 0.999 

# metric equivalence
fit_metric <- cfa(model, 
                  data = conspiracy_data,
                  group = "survey",
                  group.equal = c("loadings"),
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "pairwise")

summary(fit_metric, fit.measures = T, standardized = T)

# CFI of 0.991

# scalar equivalence
fit_scalar <- cfa(model, 
                  data = conspiracy_data,
                  group = "survey",
                  group.equal = c("loadings", "thresholds"), # threshold instead of intercept, because we have ordinal measurement
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "pairwise")

summary(fit_scalar, fit.measures = T, standardized = T)

# CFI of 0.98

# we don't have full metric equivalence, so we need to figure out, what casues
# that and try to achieve at least partial equivalence

# Lagrange Test to identify problematic contstraints
lavTestScore(fit_scalar)

parTable(fit_scalar)

# there are issues with the loadings for A4 and A5, as well as multiple thresholds
# in all of the items, especially in A2 and A4

# fit partial model
fit_partial <- cfa(model, 
                  data = conspiracy_data,
                  group = "survey",
                  group.equal = c("loadings", "thresholds"), 
                  group.partial = c(#"A4 =~ conspiracy_belief",
                                    #"A5 =~ conspiracy_belief", # Loadings for A4 & A5
                                    "A2 | t1", "A2 | t3", "A2 | t4", # Thresholds for A2
                                    #"A3 | t2", # Thresholds for A3
                                    "A4 | t2",  "A4 | t3", "A4 | t4" # Thresholds for A4
                                    #"A5 | t1"),  # Thresholds for A5
                                    ),
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "pairwise")


# save fit indices
fit_extract(list(configural_equivalence = fit_config,
                 metric_equivalence = fit_metric,
                 scalar_equivalence = fit_scalar,
                 partial_equivalence = fit_partial)) %>%
  write_csv("./output/fit_table.csv")

###########################################################################################
# sensitivity check: no weights for civey
# configural equivalence
fit_config_uw <- cfa(model, 
                  data = conspiracy_data,
                  group = "survey",
                  sampling.weights = "weight2",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "pairwise") 

# metric equivalence
fit_metric_uw <- cfa(model, 
                  data = conspiracy_data,
                  group = "survey",
                  group.equal = c("loadings"),
                  sampling.weights = "weight2",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "pairwise")

# scalar equivalence
fit_scalar_uw <- cfa(model, 
                  data = conspiracy_data,
                  group = "survey",
                  group.equal = c("loadings",  "thresholds"),
                  sampling.weights = "weight2",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "pairwise")


# we don't have full metric equivalence, so we need to figure out, what casues
# that and try to achieve at least partial equivalence

# Lagrange Test to identify problematic contstraints
lavTestScore(fit_scalar_uw)

parTable(fit_scalar_uw)

# fit partial model
fit_partial_uw <- cfa(model, 
                   data = conspiracy_data,
                   group = "survey",
                   group.equal = c("loadings", "thresholds"), 
                   group.partial = c(#"A4 =~ conspiracy_belief",# Loadings for A4 
                                     "A2 | t1", "A2 | t3", "A2 | t4", # Thresholds for A2
                                     #"A3 | t1","A3 | t2", # Thresholds for A3
                                     "A4 | t1","A4 | t2",  "A4 | t3", "A4 | t4" # Thresholds for A4
                                     #"A5 | t1"),  # Thresholds for A5
                                     ),
                                     
                   sampling.weights = "weight2",
                   estimator = "WLSMV", 
                   ordered = c("A2", "A3", "A4", "A5"), 
                   missing = "pairwise")


# save fit indices
fit_extract(list(configural_equivalence = fit_config_uw,
                 metric_equivalence = fit_metric_uw,
                 scalar_equivalence = fit_scalar_uw,
                 partial_equivalence = fit_partial_uw)) %>%
  write_csv("./output/fit_table_unweighted.csv")


##########################################################################################
# sensitivity check: full case analysis

# add weight of full cases to civey and filter full civey cases
NonPropData_fullcase <- NonPropData_fullcase %>%
  left_join(NonPropData_wide, by = "ID") %>%
  select(-weight) %>%
  rename(weight = "calweight_trim")

# merge both df
conspiracy_data_fullcase <- bind_rows(SoepConsData[,c("A2", "A3", "A4", "A5", "survey", "weight", "weight2", "weight_modes", "mode")],
                             NonPropData_wide)

# remove rows with no answer to  A2:A5
conspiracy_data_fullcase <- conspiracy_data_fullcase[rowSums(conspiracy_data_fullcase[,1:4], na.rm = T) != 0, ]


# configural equivalence
fit_config_fullcase <- cfa(model, 
                  data = conspiracy_data_fullcase,
                  group = "survey",
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "listwise") 

# metric equivalence
fit_metric_fullcase <- cfa(model, 
                  data = conspiracy_data_fullcase,
                  group = "survey",
                  group.equal = c("loadings"),
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "listwise")

# scalar equivalence
fit_scalar_fullcase <- cfa(model, 
                  data = conspiracy_data_fullcase,
                  group = "survey",
                  group.equal = c("loadings", "thresholds"), 
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "listwise")

# save fit indices
fit_extract(list(configural_equivalence = fit_config_fullcase,
                 metric_equivalence = fit_metric_fullcase,
                 scalar_equivalence = fit_scalar_fullcase)) %>%
  write_csv("./output/fit_table_fullcase.csv")


###########################################################################################
# test for Equivalence between modes in SOEP

# transform mode to factor
SoepConsData$mode <- as.factor(SoepConsData$mode)

# remove rows with no answer to  A2:A5
SoepConsData <- SoepConsData[rowSums(SoepConsData[,8:11], na.rm = T) != 0, ]


# configural equivalence
fit_config_modes <- cfa(model, 
                     data = SoepConsData,
                     group = "mode",
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"), 
                     missing = "pairwise") 

# metric equivalence
fit_metric_modes <- cfa(model, 
                     data = SoepConsData,
                     group = "mode",
                     group.equal = c("loadings"),
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"), 
                     missing = "pairwise")

# scalar equivalence
fit_scalar_modes <- cfa(model, 
                     data = SoepConsData,
                     group = "mode",
                     group.equal = c("loadings",  "thresholds"),
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"), 
                     missing = "pairwise")


# save fit indices
fit_extract(list(configural_equivalence_modes = fit_config_modes,
                 metric_equivalence_modes = fit_metric_modes,
                 scalar_equivalencen_modes = fit_scalar_modes)) %>%
  write_csv("./output/fit_table_modes.csv")


###########################################################################################
# test for Equivalence between modes in SOEP & civey

# transform mode to factor
conspiracy_data$mode <- as.factor(conspiracy_data$mode)

# configural equivalence
fit_config_modes <- cfa(model, 
                        data = conspiracy_data,
                        group = "mode",
                        sampling.weights = "weight",
                        estimator = "WLSMV", 
                        ordered = c("A2", "A3", "A4", "A5"), 
                        missing = "pairwise") 

# metric equivalence
fit_metric_modes <- cfa(model, 
                        data = conspiracy_data,
                        group = "mode",
                        group.equal = c("loadings"),
                        sampling.weights = "weight",
                        estimator = "WLSMV", 
                        ordered = c("A2", "A3", "A4", "A5"), 
                        missing = "pairwise")

# scalar equivalence
fit_scalar_modes <- cfa(model, 
                        data = conspiracy_data,
                        group = "mode",
                        group.equal = c("loadings",  "thresholds"),
                        sampling.weights = "weight",
                        estimator = "WLSMV", 
                        ordered = c("A2", "A3", "A4", "A5"), 
                        missing = "pairwise")



# save fit indices
fit_extract(list(configural_equivalence_modes = fit_config_modes,
                 metric_equivalence_modes = fit_metric_modes,
                 scalar_equivalencen_modes = fit_scalar_modes)) %>%
  write_csv("./output/fit_table_modes_with_civey.csv")


################################################################################
# since we don't have full equivalence between the soep modes and the civey mode, we 
# test seperately for interviewer administrated and self administrated modes,
# to evaluate which modes cause the issue

# self administrated modes
conspiracy_data_self <- conspiracy_data %>%
  filter(mode %in% c(2,4,6,7) & !is.na(weight_modes))

# cfa
# configural equivalence
fit_config_modes_self <- cfa(model, 
                        data = conspiracy_data_self,
                        group = "mode",
                        sampling.weights = "weight_modes",
                        estimator = "WLSMV", 
                        ordered = c("A2", "A3", "A4", "A5"), 
                        missing = "pairwise") 

# metric equivalence
fit_metric_modes_self <- cfa(model, 
                        data = conspiracy_data_self,
                        group = "mode",
                        group.equal = c("loadings"),
                        sampling.weights = "weight_modes",
                        estimator = "WLSMV", 
                        ordered = c("A2", "A3", "A4", "A5"), 
                        missing = "pairwise")

# scalar equivalence
fit_scalar_modes_self <- cfa(model, 
                        data = conspiracy_data_self,
                        group = "mode",
                        group.equal = c("loadings",  "thresholds"),
                        sampling.weights = "weight_modes",
                        estimator = "WLSMV", 
                        ordered = c("A2", "A3", "A4", "A5"), 
                        missing = "pairwise")

# we don't have full metric equivalence,

# Lagrange Test to identify problematic contstraints
lavTestScore(fit_scalar_modes_self)

parTable(fit_scalar_modes_self)

# fit partial model
fit_partial_modes_self <- cfa(model, 
                      data = conspiracy_data_self,
                      group = "survey",
                      group.equal = c("loadings", "thresholds"), 
                      group.partial = c( "A2 | t1", "A2 | t3", "A2 | t4", # Thresholds for A2
                                         "A4 | t2",  "A4 | t3", "A4 | t4"), # Thresholds for A4
                      sampling.weights = "weight_modes",
                      estimator = "WLSMV", 
                      ordered = c("A2", "A3", "A4", "A5"), 
                      missing = "pairwise")   

# save fit indices
fit_extract(list(configural_equivalence_modes = fit_config_modes_self,
                 metric_equivalence_modes = fit_metric_modes_self,
                 scalar_equivalencen_modes = fit_scalar_modes_self,
                 partial_equivalence = fit_partial_modes_self)) %>%
  write_csv("./output/fit_table_modes_selfadministrated.csv")

##########################################################################################
# interviewer administrated modes
conspiracy_data_interviewer <- conspiracy_data %>%
  filter(mode %in% c(1,3,5,7) & !is.na(weight_modes))

# cfa
# configural equivalence
fit_config_modes_interviewer <- cfa(model, 
                             data = conspiracy_data_interviewer,
                             group = "mode",
                             sampling.weights = "weight_modes",
                             estimator = "WLSMV", 
                             ordered = c("A2", "A3", "A4", "A5"), 
                             missing = "pairwise") 

# metric equivalence
fit_metric_modes_interviewer <- cfa(model, 
                             data = conspiracy_data_interviewer,
                             group = "mode",
                             group.equal = c("loadings"),
                             sampling.weights = "weight_modes",
                             estimator = "WLSMV", 
                             ordered = c("A2", "A3", "A4", "A5"), 
                             missing = "pairwise")

# scalar equivalence
fit_scalar_modes_interviewer <- cfa(model, 
                             data = conspiracy_data_interviewer,
                             group = "mode",
                             group.equal = c("loadings",  "thresholds"),
                             sampling.weights = "weight_modes",
                             estimator = "WLSMV", 
                             ordered = c("A2", "A3", "A4", "A5"), 
                             missing = "pairwise")

# we don't have full metric equivalence,

# Lagrange Test to identify problematic contstraints
lavTestScore(fit_scalar_modes_interviewer)

parTable(fit_scalar_modes_interviewer)

# fit partial model
fit_partial_modes_interviewer <- cfa(model, 
                              data = conspiracy_data_interviewer,
                              group = "survey",
                              group.equal = c("loadings", "thresholds"), 
                              group.partial = c( "A2 | t1", "A2 | t3", "A2 | t4", # Thresholds for A2
                                                 "A4 | t2",  "A4 | t3", "A4 | t4"), # Thresholds for A4
                              sampling.weights = "weight_modes",
                              estimator = "WLSMV", 
                              ordered = c("A2", "A3", "A4", "A5"), 
                              missing = "pairwise")   

# save fit indices
fit_extract(list(configural_equivalence_modes = fit_config_modes_interviewer,
                 metric_equivalence_modes = fit_metric_modes_interviewer,
                 scalar_equivalencen_modes = fit_scalar_modes_interviewer,
                 partial_equivalence = fit_partial_modes_interviewer)) %>%
  write_csv("./output/fit_table_modes_interviewer.csv")

##################################################################################
# sensitivity test for scale: Repeat with continous estimator and fiml for missings

# configural equivalence
fit_config <- cfa(model, 
                  data = conspiracy_data,
                  group = "survey",
                  sampling.weights = "weight",
                  estimator = "MLR",
                  missing = "fiml")

# metric equivalence
fit_metric <- cfa(model, 
                  data = conspiracy_data,
                  group = "survey",
                  group.equal = c("loadings"),
                  sampling.weights = "weight",
                  estimator = "MLR",
                  missing = "fiml")

# scalar equivalence
fit_scalar <- cfa(model, 
                  data = conspiracy_data,
                  group = "survey",
                  group.equal = c("loadings", "intercepts"),
                  sampling.weights = "weight",
                  estimator = "MLR",
                  missing = "fiml")

# we don't have full metric equivalence

# Lagrange Test to identify problematic contstraints
lavTestScore(fit_scalar)

parTable(fit_scalar)

# fit partial model
fit_partial <- cfa(model, 
                   data = conspiracy_data,
                   group = "survey",
                   group.equal = c("loadings", "thresholds"), 
                   group.partial = c("A2 ~ 1",
                                     "A4 ~ 1"),  #Intercept A2 & A4
                   sampling.weights = "weight",
                   estimator = "MLR",
                   missing = "fiml")


# save fit indices
fit_extract(list(configural_equivalence = fit_config,
                 metric_equivalence = fit_metric,
                 scalar_equivalence = fit_scalar,
                 partial_equivalence = fit_partial)) %>%
  write_csv("./output/fit_table_continous.csv")


#####################################################################################
####################################################################################
# sensitivity check for scale Transformation

########################################################################################

# First Option: smaller middle category
SoepConsData_t1 <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData.dta")

# transform scale of Soep Data from 0-10 to 1-5
SoepConsData_t1 <- SoepConsData_t1 %>%
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

# alpha of 0.8


#cfa
# configural equivalence
fit_config_t1 <- cfa(model, 
                  data = conspiracy_data_t1,
                  group = "survey",
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "pairwise") 

# metric equivalence
fit_metric_t1 <- cfa(model, 
                  data = conspiracy_data_t1,
                  group = "survey",
                  group.equal = c("loadings"),
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "pairwise")

# scalar equivalence
fit_scalar_t1 <- cfa(model, 
                  data = conspiracy_data_t1,
                  group = "survey",
                  group.equal = c("loadings",  "thresholds"),
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "pairwise")

# we don't have full metric equivalence,

# Lagrange Test to identify problematic contstraints
lavTestScore(fit_scalar_t1)

parTable(fit_scalar_t1)

# fit partial model
fit_partial_t1 <- cfa(model, 
                      data = conspiracy_data_t1,
                      group = "survey",
                      group.equal = c("loadings", "thresholds"), 
                      group.partial = c( "A2 | t1", "A2 | t3", "A2 | t4", # Thresholds for A2
                                         "A4 | t1","A4 | t2",  "A4 | t3", "A4 | t4"), # Thresholds for A4
                      sampling.weights = "weight",
                      estimator = "WLSMV", 
                      ordered = c("A2", "A3", "A4", "A5"), 
                      missing = "pairwise")


# save fit indices
fit_extract(list(configural_equivalence = fit_config_t1,
                 metric_equivalence = fit_metric_t1,
                 scalar_equivalencen = fit_scalar_t1,
                 partial_equivalence = fit_partial_t1)) %>%
  write_csv("./output/fit_table_transformation1.csv")

# the cfi for scalar equivalence  is lower with this transformation


#########################################################################################
# Second Option: Binary Scale  with middle value in lower category
SoepConsData_t2 <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData.dta")

# transform scale of Soep Data 
SoepConsData_t2 <- SoepConsData_t2 %>%
  mutate(across(A1:A5, ~ case_when(. %in% c(0,1,2,3,4,5) ~ 1,
                                   . %in% c(6,7,8,9,10) ~ 2)),
         survey = "soep") %>%
  rename(weight = W_SOEP)

# transform scale of civey data
NonPropData_t2 <- read_dta("Z:/Eigene Dateien/Non_prob/Data/NonPropData_CalWeighted_v09022025.dta")

# adjust format of nonprob data
NonPropData_wide_t2 <- NonPropData_t2 %>%
  select(ID, Answer, Question, calweight)%>%
  pivot_wider(names_from = Question, values_from = Answer, names_prefix = "A") %>%
  rename(weight = calweight) %>%
  mutate(survey = "civey",
         weight2 = 1,
         mode = 7,
         across(A2:A5, ~ case_when(. %in% c(1,2,3) ~ 1,
                                   . %in% c(4,5) ~ 2)))



# merge both df
conspiracy_data_t2 <- bind_rows(SoepConsData_t2[,c("A2", "A3", "A4", "A5", "survey", "weight", "mode")],
                                NonPropData_wide_t2)

# remove rows with no answer to  A2:A5
conspiracy_data_t2 <- conspiracy_data_t2[rowSums(conspiracy_data_t2[,1:4], na.rm = T) != 0, ]

# calculate kappa statistic
kappa_val <- kappam.fleiss(conspiracy_data_t2[, 1:4])
kappa_val

# kappa of 0.3 -> a bit higher than before but still not good

# calculate cronbachs alpha
alpha_val <- alpha(conspiracy_data_t2[, 1:4])
alpha_val

# alpha of 0.69



#cfa
# configural equivalence
fit_config_t2 <- cfa(model, 
                     data = conspiracy_data_t2,
                     group = "survey",
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"), 
                     missing = "pairwise") 

# metric equivalence
fit_metric_t2 <- cfa(model, 
                  data = conspiracy_data_t2,
                  group = "survey",
                  group.equal = c("loadings"),
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "pairwise")

# scalar equivalence
fit_scalar_t2 <- cfa(model, 
                  data = conspiracy_data_t2,
                  group = "survey",
                  group.equal = c("loadings",  "thresholds"),
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"), 
                  missing = "pairwise")

# save fit indices
fit_extract(list(configural_equivalence = fit_config_t2,
                 metric_equivalence = fit_metric_t2,
                 scalar_equivalencen = fit_scalar_t2)) %>%
  write_csv("./output/fit_table_transformation2.csv")

# cfi values very high!

#########################################################################################
# Third Option: Binary Scale  with middle value in higher category
SoepConsData_t3 <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData.dta")

# transform scale of Soep Data 
SoepConsData_t3 <- SoepConsData_t3 %>%
  mutate(across(A1:A5, ~ case_when(. %in% c(0,1,2,3,4) ~ 1,
                                   . %in% c(5,6,7,8,9,10) ~ 2)),
         survey = "soep") %>%
  rename(weight = W_SOEP)

# transform scale of civey data
NonPropData_t3 <- read_dta("Z:/Eigene Dateien/Non_prob/Data/NonPropData_CalWeighted_v09022025.dta")

# adjust format of nonprob data
NonPropData_wide_t3 <- NonPropData_t3 %>%
  select(ID, Answer, Question, calweight)%>%
  pivot_wider(names_from = Question, values_from = Answer, names_prefix = "A") %>%
  rename(weight = calweight) %>%
  mutate(survey = "civey",
         weight2 = 1,
         mode = 7,
         across(A2:A5, ~ case_when(. %in% c(1,2) ~ 1,
                                   . %in% c(3,4,5) ~ 2)))



# merge both df
conspiracy_data_t3 <- bind_rows(SoepConsData_t3[,c("A2", "A3", "A4", "A5", "survey", "weight", "mode")],
                                NonPropData_wide_t3)

# remove rows with no answer to  A2:A5
conspiracy_data_t3 <- conspiracy_data_t3[rowSums(conspiracy_data_t3[,1:4], na.rm = T) != 0, ]

# calculate kappa statistic
kappa_val <- kappam.fleiss(conspiracy_data_t3[, 1:4])
kappa_val
# kappa of 0.344 -> highest one, but also still low

# calculate cronbachs alpha
alpha_val <- alpha(conspiracy_data_t3[, 1:4])
alpha_val

# alpha of 0.73 -> good reliabilty, but lower than before


#cfa
# configural equivalence
fit_config_t3 <- cfa(model, 
                     data = conspiracy_data_t3,
                     group = "survey",
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"), 
                     missing = "pairwise") 

# metric equivalence
fit_metric_t3 <- cfa(model, 
                     data = conspiracy_data_t3,
                     group = "survey",
                     group.equal = c("loadings"),
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"), 
                     missing = "pairwise")

# scalar equivalence
fit_scalar_t3 <- cfa(model, 
                     data = conspiracy_data_t3,
                     group = "survey",
                     group.equal = c("loadings",  "thresholds"),
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"), 
                     missing = "pairwise")

# save fit indices
fit_extract(list(configural_equivalence = fit_config_t3,
                 metric_equivalence = fit_metric_t3,
                 scalar_equivalencen = fit_scalar_t3)) %>%
  write_csv("./output/fit_table_transformation3.csv")

# cfi values very high!


#############################################################################################
# transform scale by using cumulative distributions
SoepConsData_t4 <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData.dta")


# scale is adjusted seperatly for  each question

# function to transform prob data based on cumulative distribution cumulative distributions
transform_to_cumdist <- function(prob_values, nonprob_values){
  
  prob <- ecdf(prob_values)
  
  quantiles_nonprob <- quantile(nonprob_values, 
                                probs = seq(0,1, length.out = length(unique(nonprob_values))),
                                na.rm = T)

  transformed_prob <- sapply(prob_values, function(x){
    quantile_position <- prob(x)
    approx(seq(0,1, length.out = length(quantiles_nonprob)), quantiles_nonprob, xout = quantile_position, rule = 2)$y
  })
  
  # round to zero digits
  transformed_prob_round <- pmin(pmax(round(transformed_prob),1),5)
  
  return(transformed_prob_round)
}

SoepConsData_t4 <-  SoepConsData_t4 %>%
  mutate(A2 = transform_to_cumdist(A2, NonPropData_wide$A2),
         A3 = transform_to_cumdist(A3, NonPropData_wide$A3),
         A4 = transform_to_cumdist(A4, NonPropData_wide$A4),
         A5 = transform_to_cumdist(A5, NonPropData_wide$A5),
         survey = "soep") %>%
  rename(weight = W_SOEP) %>%
  mutate(weight2 = weight)

# Problem: the quantiles do not overlap exactly, that could be due to
# the limitied amount of distinct values. The first quantile is +1 for
# most questions in the transformed soep data. Median and 3rd quantile are the same

# merge both df
conspiracy_data_t4 <- bind_rows(SoepConsData_t4[,c("A2", "A3", "A4", "A5", "survey", "weight", "weight2", "mode")],
                             NonPropData_wide)

# remove rows with no answer to  A2:A5
conspiracy_data_t4 <- conspiracy_data_t4[rowSums(conspiracy_data_t4[,1:4], na.rm = T) != 0, ]



# calculate kappa statistic
kappa_val <- kappam.fleiss(conspiracy_data_t4[, 1:4])
kappa_val

# kappa of 0.18 -> lower than before

# calculate cronbachs alpha
alpha_val <- alpha(conspiracy_data_t4[, 1:4])
alpha_val

# alpha of 0.8


#cfa
# configural equivalence
fit_config_t4 <- cfa(model, 
                     data = conspiracy_data_t4,
                     group = "survey",
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"), 
                     missing = "pairwise") 

# metric equivalence
fit_metric_t4 <- cfa(model, 
                     data = conspiracy_data_t4,
                     group = "survey",
                     group.equal = c("loadings"),
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"), 
                     missing = "pairwise")

# scalar equivalence
fit_scalar_t4 <- cfa(model, 
                     data = conspiracy_data_t4,
                     group = "survey",
                     group.equal = c("loadings",  "thresholds"),
                     sampling.weights = "weight",
                     estimator = "WLSMV", 
                     ordered = c("A2", "A3", "A4", "A5"), 
                     missing = "pairwise")

# save fit indices
fit_extract(list(configural_equivalence = fit_config_t4,
                 metric_equivalence = fit_metric_t4,
                 scalar_equivalencen = fit_scalar_t4)) %>%
  write_csv("./output/fit_table_transformation4.csv")

# cfi values very high!

################################################################################
# test transformation 4  with continous estimator
#cfa
# configural equivalence
fit_config_t4_continous <- cfa(model, 
                     data = conspiracy_data_t4,
                     group = "survey",
                     sampling.weights = "weight",
                     estimator = "MLR",
                     missing = "fiml") 

# metric equivalence
fit_metric_t4_continous <- cfa(model, 
                     data = conspiracy_data_t4,
                     group = "survey",
                     group.equal = c("loadings"),
                     sampling.weights = "weight",
                     estimator = "MLR",
                     missing = "fiml")

# scalar equivalence
fit_scalar_t4_continous <- cfa(model, 
                     data = conspiracy_data_t4,
                     group = "survey",
                     group.equal = c("loadings", "intercepts"),
                     sampling.weights = "weight",
                     estimator = "MLR",
                     missing = "fiml")

# we don't have full metric equivalence, so we need to figure out, what casues
# that and try to achieve at least partial equivalence

# Lagrange Test to identify problematic contstraints
lavTestScore(fit_scalar_t4_continous)

parTable(fit_scalar_t4_continous)

# there are issues with the loadings for A4 and A5, as well as multiple thresholds
# in all of the items, especially in A2 and A4

# fit partial model
fit_partial_t4_continous <- cfa(model, 
                   data = conspiracy_data_t4,
                   group = "survey",
                   group.equal = c("loadings", "thresholds"), 
                   group.partial = c(#"A2 =~ conspiracy_belief",
                                     #"A3 =~ conspiracy_belief",
                                     #"A4 =~ conspiracy_belief",
                                     "A2 ~ 1",
                                     #"A3 ~ 1",
                                     #"A5 ~ 1",
                                     "A4 ~ 1"),  #Intercepts A2 & A4
                   sampling.weights = "weight",
                   estimator = "MLR",
                   missing = "fiml")


# save fit indices
fit_extract(list(configural_equivalence = fit_config_t4_continous,
                 metric_equivalence = fit_metric_t4_continous,
                 scalar_equivalencen = fit_scalar_t4_continous,
                 partial_equivalence = fit_partial_t4_continous)) %>%
  write_csv("./output/fit_table_transformation4_continous.csv")
