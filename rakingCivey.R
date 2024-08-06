################################################################################
# Modeling Selection by Adjusting To Benchmark Distributions taken from SOEP 
################################################################################

library(haven)
library(survey)
library(questionr)

# ------------
# Read Data
# ------------
setwd("F:\\VerschwDaten\\Data")
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
#    on Age, Sex, Marital Status, Educational Degree, Employment Status

    tAgeSex <- table(Age = S$age, Sex = S$sex);  tAgeSex <- tAgeSex/sum(tAgeSex)
    tEmploymentDegree <- table(Empl = S$EmployStatus, Edu = S$SchoolDegree); tEmploymentDegree <- tEmploymentDegree/sum(tEmploymentDegree) 
    tOstWestDens <- table(OW = S$Ost_west, Dens = S$PopDens); tOstWestDens <- tOstWestDens/sum(tOstWestDens)
    tOccPosDegr <- table(Pos = S$OccPos, Degr = S$OccDegr); tOccPosDegr <- tOccPosDegr/sum(tOccPosDegr) 
    tFamState <- table(x = S$FamState); tFamState <- tFamState/sum(tFamState) 

# 2. Conduct raking
    # find factor to mulitply to Civey data to match the SOEP distribution
    # do this for each question differently due to distinct sample for each question in Civey data
    N= sum(S$W_SOEP) # total population in 2021 acc official stats (Microzensus)
    pAgeSex <- round(tAgeSex*N) 
    pEmploymentDegree <- round(tEmploymentDegree*N) 
    pOstWestDens <- round(tOstWestDens*N)
    pOccPosDegr <- round(tOccPosDegr*N)
    pFamState <- round(tFamState*N) 

    # Data from A1 in Civey
    C1 <- C[!is.na(C$A1), c("ID", "ZP1", "A1", "w1", "Age1", "G1", "PD1", "SF1", "EW1", "Edu1", "OccEdu1", "OccPos1", "Fam1", "Kids1", "Rel1", "Empl1")]
    colnames(C1) <- c("ID", "end", "A1", "W_Civey", "Age", "Sex", "PopDens", "partyTend", "Ost_West", "SchoolDegree", "OccDegr", "OccPos", "FamState", "kidsHH", "Rel", "EmplyStatus")
    # Age
    ddata <- svydesign(id=~ID, weights=~1, data=C1)
    rownames(pAgeSex)[rownames(pAgeSex) %in% "65+"] <- "65 +"
    
    postSt <- postStratify(ddata, ~Age+Sex, pAgeSex, partial=T)
    C1$calW_AgeSex <- weights(postSt)
    # questionr::wtd.table(Age=C1$Age, Sex=C1$Sex, weights=C1$calW_AgeSex, exclude=NULL); pAgeSex
    
    # TO BE CONTINUED
    
    
    
