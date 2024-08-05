################################################################################
# Modeling Selection by Adjusting To Benchmark Distributions taken from SOEP 
################################################################################

library(haven)
library(survey)

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

    tAge <- table(x = S$age)/sum(tAge)
    tSex <- table(x = S$sex)/sum(tSex)
    tEmployment <- table(x = S$EmployStatus)/sum(tEmployment) 
    tFamState <- table(x = S$FamState)/sum(tFamState) 
    tSchoolDegree <- table(x = S$SchoolDegree)/sum(tSchoolDegree) 
    tOstWest <- table(x = S$Ost_west)/sum(tOstWest) 
    # add Occ. Degress, Occ. Position, Population Density

# 2. Conduct raking (TODO add fct from survey pck)
    # find factor to mulitply to Civey data to match the SOEP distribution
    
    
