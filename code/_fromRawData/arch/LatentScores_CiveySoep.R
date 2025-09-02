# estimation of latent scores
# only uses questions A2-A5

###################################################################################################

# packages
pacman::p_load(haven, tidyverse, lavaan, semTools, irr, psych, VIM, 
               mice, naniar)

# read data
rm(list=ls())

NonPropData <- read_dta("Z:/Eigene Dateien/Non_prob/Data/NonPropData_CalWeighted_v09022025.dta")
SoepConsData <- read_dta("Z:/Eigene Dateien/Non_prob/Data/soepConsData.dta")

# transform scale of Soep Data from 0-10 to 1-5
SoepConsData <- SoepConsData %>%
  mutate(across(A1:A5, ~ case_when(. %in% c(0,1) ~ 1,
                                   . %in% c(2,3) ~ 2,
                                   . %in% c(4,5,6) ~ 3,
                                   . %in% c(7,8) ~ 4,
                                   . %in% c(9,10) ~ 5)),
         survey = "soep") %>%
  rename(weight = W_SOEP) 

# adjust format of nonprob data
NonPropData_wide <- NonPropData %>%
  select(ID, Answer, Question, calweight)%>%
  pivot_wider(names_from = Question, values_from = Answer, names_prefix = "A") %>%
  rename(weight = calweight) %>%
  mutate(survey = "civey")

# merge both df
conspiracy_data <- bind_rows(SoepConsData[,c("A2", "A3", "A4", "A5", "survey", "weight")],
                             NonPropData_wide)

# remove rows with no answer to  A2:A5
conspiracy_data <- conspiracy_data[rowSums(conspiracy_data[,1:4], na.rm = T) != 0, ]


########################################################################################
# latente scores
model <- "conspiracy_belief =~ A2 + A3 + A4 + A5"

# combined
fit_combined <- cfa(model, 
                  data = conspiracy_data,
                  sampling.weights = "weight",
                  estimator = "WLSMV", 
                  ordered = c("A2", "A3", "A4", "A5"),
                  missing = "pairwise") 


score_combined <- lavPredict(fit_combined)

# soep
fit_soep <- cfa(model, 
                    data = SoepConsData,
                    sampling.weights = "weight",
                    estimator = "WLSMV", 
                    ordered = c("A2", "A3", "A4", "A5"),
                    missing = "pairwise") 


score_soep <- lavPredict(fit_soep)

# civey
fit_civey <- cfa(model, 
                    data = NonPropData_wide,
                    sampling.weights = "weight",
                    estimator = "WLSMV", 
                    ordered = c("A2", "A3", "A4", "A5"),
                    missing = "pairwise") 


score_civey <- lavPredict(fit_civey)



################################################################################
# latent means and differences in latent means

mean_combined <- mean(score_combined, na.rm = T)
mean_soep <- mean(score_soep, na.rm = T)
mean_civey <- mean(score_civey, na.rm = T)

relative_diff_soep <- (mean_soep - mean_combined) / mean_combined
relative_diff_civey <- (mean_civey - mean_combined) / mean_combined