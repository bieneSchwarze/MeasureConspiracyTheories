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

    tAge <- wtd.table(x = S$age, weights = S$W_SOEP) # for getting benchmark not regard NAs
    tSex <- wtd.table(x = S$sex, weights = S$W_SOEP)
    tEmployment <- wtd.table(x = S$employment, weights = S$W_SOEP)
    tFamState <- wtd.table(x = S$FamState, weights = S$W_SOEP)
    tSchoolDegree <- wtd.table(x = S$SchoolDegree, weights = S$W_SOEP)
    tOstWest <- wtd.table(x = S$Ost_West, weights = S$W_SOEP)

# 2. Conduct raking (TODO add fct)
