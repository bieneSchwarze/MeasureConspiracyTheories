# script performs analysis of  missingness pattern and Confirmatory Factor Analysis
# of imputed dataset
# only uses questions A2-A5

###################################################################################################

# packages
pacman::p_load(haven, tidyverse, lavaan, semTools, irr, psych, VIM, 
               mice, naniar)

# read data
rm(list=ls())

NonPropData <- read_dta("Z:/Eigene Dateien/Non_prob/Data/NonPropData_CalWeighted_v09022025.dta")
SoepConsData <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData.dta")

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

# adjust format of nonprob data
NonPropData_wide <- NonPropData[,-c(3,5)] %>%
  pivot_wider(names_from = Question, values_from = Answer, names_prefix = "A") %>%
  rename(weight = calweight) %>%
  mutate(survey = "civey")

####################################################################################
# analyse missingnesspattern

# SOEP 
aggr(SoepConsData[,c("A2", "A3", "A4", "A5")])
gg_miss_upset(SoepConsData[,c("A2", "A3", "A4", "A5")])

# seems to be not completely at random, there are more missings in A4 and A% and those two
# are also missing at the same time quite often

# Civey
aggr(NonPropData_wide[,c("A2", "A3", "A4", "A5")])
gg_miss_upset(NonPropData_wide[,c("A2", "A3", "A4", "A5")])

mcar_test(NonPropData_wide[, -c(15,16,21)])
# on the first glimse, the patterns seems random and there are the same amount of missings
# for all Questions. Nevertheless, the mcar test is significant, meaning the Null-Hypothesis
# that the Missings are completely at random is rejected and we have to assume that missings
# are not completely at random.

##################################################################################
# Imputation
SoepConsData <- SoepConsData %>%
  mutate(across(where(is.character), ~as.numeric(as.factor(.))),
         across(where(is.labelled), ~as.numeric(as.factor(.))))

SoepConsData_imp <- mice(SoepConsData, method = "pmm", maxit = 50)

SoepConsData_compl <- complete(SoepConsData_imp)


NonPropData_wide <- NonPropData_wide %>%
  mutate(across(where(is.character), ~as.numeric(as.factor(.))),
         across(where(is.labelled), ~as.numeric(as.factor(.))))

NonPropData_wide_imp <- mice(NonPropData_wide, method = "pmm", maxit = 50)

NonPropData_wide_compl <- complete(NonPropData_wide_imp)

######################################################################################################

# Datatransformation

# transform scale of Soep Data from 0-10 to 1-5
SoepConsData_compl <- SoepConsData_compl %>%
  mutate(across(A1:A5, ~ case_when(. %in% c(1,2) ~ 1,
                                   . %in% c(3,4) ~ 2,
                                   . %in% c(5,6,7) ~ 3,
                                   . %in% c(8,9) ~ 4,
                                   . %in% c(10,11) ~ 5)),
         survey = 2) %>%
  rename(weight = W_SOEP) 

# merge both df
conspiracy_data_compl <- bind_rows(SoepConsData_compl[,c("A2", "A3", "A4", "A5", "survey", "weight")],
                             NonPropData_wide_compl[,c("A2", "A3", "A4", "A5", "survey", "weight")])

colSums(is.na(conspiracy_data_compl))

#########################################################################################
# CFA 

model <- "conspiracy_belief =~ A2 + A3 + A4 + A5"

# configural equivalence
fit_config <- cfa(model, 
                  data = conspiracy_data_compl,
                  group = "survey",
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5")) 

# metric equivalence
fit_metric <- cfa(model, 
                  data = conspiracy_data_compl,
                  group = "survey",
                  group.equal = c("loadings"),
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"))

# scalar equivalence
fit_scalar <- cfa(model, 
                  data = conspiracy_data_compl,
                  group = "survey",
                  group.equal = c("loadings", "thresholds"), # threshold instead of intercept, because we have ordinal measurement
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"))

# we don't have full metric equivalence, so we need to figure out, what casues
# that and try to achieve at least partial equivalence

# Lagrange Test to identify problematic contstraints
lavTestScore(fit_scalar)

parTable(fit_scalar)

# there are issues with the loadings for A4 and A5, as well as multiple thresholds
# in all of the items, especially in A2 and A4

# fit partial model
fit_partial <- cfa(model, 
                   data = conspiracy_data_compl,
                   group = "survey",
                   group.equal = c("loadings", "thresholds"), 
                   group.partial = c(
                     "A2 | t1", "A2 | t3", "A2 | t4", # Thresholds for A2
                     "A4 | t2",  "A4 | t3", "A4 | t4" # Thresholds for A4
                     ),
                   sampling.weights = "weight",
                   estimator = "WLSMV", 
                   ordered = c("A2", "A3", "A4", "A5"))


# save fit indices
fit_extract(list(configural_equivalence = fit_config,
                 metric_equivalence = fit_metric,
                 scalar_equivalence = fit_scalar,
                 partial_equivalence = fit_partial)) %>%
  write_csv("./output/fit_table_MI.csv")
