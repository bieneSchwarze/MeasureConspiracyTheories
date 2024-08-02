################################################################################
# Modeling Selection by Adjusting To Benchmark Distributions taken from SOEP 
################################################################################

library(haven)
library(questionr)

# ------------
# Read Data
# ------------
setwd("D:\\VerschwDaten\\Data")
S <- read_dta("soepConsData.dta")
C <- read_dta("civeyConsData.dta")
for(i in 1:ncol(S)){
  S[which(unlist(S[, i]) %in% ""),i] <- NA
}
for(i in 1:ncol(C)){
  C[which(unlist(C[, i]) %in% ""),i] <- NA
}

# ------------------
# Raking to the SOEP Distribution
# ------------------

# 1. Derive Distribution from SOEP Data 
#    on Age, Sex, Marital Status, Kids in HH, Educational Degree, Employment Status

    tAge <- wtd.table(x = S$age, weights = S$W_SOEP); sum(tAge) # N=66599734
    tSex <- wtd.table(x = S$sex, weights = S$W_SOEP); sum(tSex) # N=68071874 
    tEmployment <- wtd.table(x = S$EmployStatus, weights = S$W_SOEP); sum(tEmployment) # N=67566062
    tFamState <- wtd.table(x = S$FamState, weights = S$W_SOEP); sum(tFamState) # N=67848713
    tSchoolDegree <- wtd.table(x = S$SchoolDegree, weights = S$W_SOEP); sum(tSchoolDegree) # N=64565795
    tOstWest <- wtd.table(x = S$Ost_west, weights = S$W_SOEP); sum(tOstWest) # N=68075973
    # distinct pop size due to miss: take OstWest Sum since here we only have two missing values
    
    # stand. for getting comparable for population size N=68075973
    N=68075973
    rAge <- tAge/sum(tAge) * N
    rSex <- tSex/sum(tSex) * N
    rEmployment <- tEmployment/sum(tEmployment) * N
    rFamState <- tFamState/sum(tFamState) * N
    rSchoolDegree <- tSchoolDegree/sum(tSchoolDegree) *N
    rOstWest <- tOstWest/sum(tOstWest) * N
    
# 2. Conduct raking (TODO add fct)
    # find factor to mulitply to Civey weights to match weighted pop numbers of SOEP
    
    
