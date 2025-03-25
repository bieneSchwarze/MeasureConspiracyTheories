################################################################################
# Survey-Mode specific survey weights
# 25.03.2025
# SZinn
################################################################################

# Notes:
# 2 groups (interviewer-based: CAPI, CAPIbyPhone [N=13404], CATI; without interviewer: CAWI, PAPI, CASI [N=4083])
# Weighting by adjustment factor (membership in group 1/2 yes/no, 
#  as IV - same variables as in raking for Civey data: age + sex + FamState + kidsHH + SchoolDegree + EmployStatus + OccDegr + Rel
#  model logit, analysis whether model significantly discriminates between both groups; 
#  if yes: adj factor multiplied to SOEP weight; easy trimming at 95% quantile)

library(haven)
library(mice)
library(labelled)
library(survey)

# ------------------------------------------------------------------------------
# Preparation of data
# ------------------------------------------------------------------------------
# Read in data, check for missing values, generate variables marking the survey mode class
DAT <- read_dta("F:\\VerschwDaten\\Data\\soepConsData.dta")
table(DAT$mode, exclude=NULL)
md <- md.pattern(DAT, plot=FALSE); md[nrow(md),]/nrow(DAT) # check missing values; none in the focal variables
DAT$interviewerMode <- ifelse(DAT$mode %in% c(1,3,5), 1, 0) # 1: CAPI, CATI, CAPIbyPhone; 0: CAWI, PAPI, CASI

# There are zero weights in SOEP data; due to snowball sampling or similar; kick out
table(DAT$W_SOEP==0)
DAT <- DAT[DAT$W_SOEP !=0,]
nrow(DAT) # ok

# Generate dummy variables
X <- model.matrix(
  ~ age + sex + FamState + kidsHH + SchoolDegree + EmployStatus + OccDegr + Rel,
  data = DAT
)
X <- X[, -1] # kick out intercept
X <- X[,!(colnames(X) %in% c("age18 - 29", "sexMänner", "FamStateLedig", # take out reference categories
                             "kidsHHKeine Kinder im Haushalt", "SchoolDegreeAbitur", 
                             "EmployStatusNicht Erwerbstätig", "OccDegrStudium oder höher", 
                             "RelKonfessionslos"))]
D1 <- data.frame(DAT$interviewerMode, DAT$W_SOEP, X); names(D1)[1:2] <- c("interviewerMode", "W_SOEP")
design <- svydesign(ids = ~1, weights = ~W_SOEP, data = D1)

# ------------------------------------------------------------------------------
# Interviewer / self mode
# ------------------------------------------------------------------------------
model_1 <- svyglm(interviewerMode ~ age30...39 + age40...49 + age50...64 + age65. +
                    sexFrauen + FamStateGeschieden + FamStateVerheiratet...Verwitwet + 
                    kidsHHKinder.im.Haushalt + SchoolDegreeAnderes + SchoolDegreeHauptschule...kein.Abschluss +
                    SchoolDegreeMittlere.Reife + EmployStatusErwerbstätig + 
                    OccDegrBerufsausbildung + OccDegrNoch.in.Ausbildung + OccDegrOhne.Abschluss + 
                    RelEine.andere + RelEvangelisch + RelKatholisch,
                  design = design,family = quasibinomial())  
summary(model_1) # impact: age, famState, kids, SchoolDegree, OccDegr
model_1r <- svyglm(interviewerMode ~ age30...39 + age40...49 + age50...64 + age65. +
                    FamStateGeschieden + FamStateVerheiratet...Verwitwet + 
                    kidsHHKinder.im.Haushalt + SchoolDegreeAnderes + SchoolDegreeHauptschule...kein.Abschluss +
                    SchoolDegreeMittlere.Reife + 
                    OccDegrBerufsausbildung + OccDegrNoch.in.Ausbildung + OccDegrOhne.Abschluss,
                  design = design,family = quasibinomial())  
summary(model_1r) 
pr_1 <- predict(model_1r, type = "response")
X_1 <- model.matrix(~ age + FamState + kidsHH + SchoolDegree + OccDegr, data = DAT)
# Discrimination plot: age
par(mfrow=c(2,3))
boxplot(pr_1 ~ DAT$interviewerMode + X[,2]) # ---> no sign. diff. in distribution for age
boxplot(pr_1 ~ DAT$interviewerMode + X[,3])
boxplot(pr_1 ~ DAT$interviewerMode + X[,4])
boxplot(pr_1 ~ DAT$interviewerMode + X[,5])
boxplot(pr_1 ~ DAT$interviewerMode + X[,6]) 
# Discrimination plot: FamState
par(mfrow=c(2,2))
boxplot(pr_1 ~ DAT$interviewerMode + X[,7]) # ---> no sign. diff. in distribution for FamState
boxplot(pr_1 ~ DAT$interviewerMode + X[,8])
boxplot(pr_1 ~ DAT$interviewerMode + X[,9])
# Discrimination plot: kids in HH
par(mfrow=c(1,2))
boxplot(pr_1 ~ DAT$interviewerMode + X[,10]) 
boxplot(pr_1 ~ DAT$interviewerMode + X[,11])
# Discrimination plot: SchoolDegr
par(mfrow=c(2,2))
boxplot(pr_1 ~ DAT$interviewerMode + X[,12]) 
boxplot(pr_1 ~ DAT$interviewerMode + X[,13])
boxplot(pr_1 ~ DAT$interviewerMode + X[,14])
boxplot(pr_1 ~ DAT$interviewerMode + X[,15])
# Discrimination plot: OccDegr
par(mfrow=c(2,2))
boxplot(pr_1 ~ DAT$interviewerMode + X[,16]) 
boxplot(pr_1 ~ DAT$interviewerMode + X[,17])
boxplot(pr_1 ~ DAT$interviewerMode + X[,18])

# There are no significant differences in the group specific 
# propensities (interviewer mode; yes / no) along age, famstate, schooldegr, kids in HH, occdegr

# Derive a weighting factor (anyway)
DAT_interview <- DAT[DAT$interviewerMode %in% 1,]
DAT_interview$W_SOEP_interview <- DAT_interview$W_SOEP * (1/pr_1[DAT$interviewerMode %in% 1])
range(DAT_interview$W_SOEP_interview); hist(DAT_interview$W_SOEP_interview)
q95 <- quantile(DAT_interview$W_SOEP_interview, probs=0.95)
DAT_interview$W_SOEP_interview[DAT_interview$W_SOEP_interview > q95] <- q95 # trim at 95% percentile
range(DAT_interview$W_SOEP_interview); hist(DAT_interview$W_SOEP_interview)

DAT_self <- DAT[DAT$interviewerMode %in% 0,]
DAT_self$W_SOEP_self <- DAT_self$W_SOEP * (1/pr_1[DAT$interviewerMode %in% 0])
range(DAT_self$W_SOEP_self); hist(DAT_self$W_SOEP_self)
q95 <- quantile(DAT_self$W_SOEP_self, probs=0.95)
DAT_self$W_SOEP_self[DAT_self$W_SOEP_self > q95] <- q95 # trim at 95% percentile
range(DAT_self$W_SOEP_self); hist(DAT_self$W_SOEP_self)

# store data as dta file
setwd("F:\\VerschwDaten\\Data")
write_dta(DAT_interview, "soepConsData_interviewerMode.dta")
write_dta(DAT_self, "soepConsData_selfMode.dta")


