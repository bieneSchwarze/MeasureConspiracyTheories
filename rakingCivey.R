################################################################################
# Modeling Selection by Adjusting To Benchmark Distributions taken from SOEP 
# Using Raking
# 09.02.2025
# SZinn
################################################################################

library(haven)
library(survey)
library(mice)

# ------------
# Read Data
# ------------
setwd("F:\\VerschwDaten\\Data")

# a) SOEP Data
S <- read_dta("soepConsData.dta")
md <- md.pattern(S)
md[nrow(md),]/nrow(S) 
for(i in 1:ncol(S)){
  S[which(unlist(S[, i]) %in% ""),i] <- NA
}

# b) Civey Data (name properly, deal with NAs)
C <- read_dta("civeyConsData_A2ToA5.dta")
table(C$Edu)
C$Edu[ C$Edu %in% "Weiß nicht / keine Angabe"] <- "Anderes"
for(i in 1:ncol(C)){
  C[which(unlist(C[, i]) %in% ""),i] <- NA
  C[which(unlist(C[, i]) %in% "Weiß nicht / keine Angabe"),i] <- NA
}
colnames(C) <- c("ID", "Question", "end", "Answer", "W_Civey", "Age", "Sex", "PopDens", 
                 "KK", "partyTend", "Ost_West", "SchoolDegree", "OccDegr", "OccPos", "FamState", "kidsHH", "Rel", "EmplyStatus")
CW <- C[C$Question %in% 2, !(colnames(C) %in% c("Question", "end", "Answer", "W_Civey", "KK"))]
md <- md.pattern(CW)
md[nrow(md),]/nrow(CW) # do not use occ position for weighting (8% missingness, all other variable impute with one random draw - just for weighting)
CW <- CW %>% mutate(across(everything(), as.factor))
imp <- mice(CW, m=1, method="cart", pred = quickpred(CW, exclude="id")) # TODO for robustness checks, produce at least 5 to 10 runs and check whether weights stay close to each other
CM <- complete(imp, action=1)# every entry needs a value - otherwise raking does not work

# ------------------
# Raking to the SOEP Distribution
# ------------------

# 1. Derive Distribution from SOEP Data 
#    on Age+Sex, Family Status x Kids in HH, Employment Status x Educational Degree,
#    Occupational Position x Occupational Degree, EastWest  + PopDensity, Religion, (Party Tendency)

    tAgeSex <- table(Age = S$age, Sex = S$sex);  tAgeSex <- tAgeSex/sum(tAgeSex)
    tEmploymentDegree <- table(EmplyStatus = S$EmployStatus, SchoolDegree = S$SchoolDegree); tEmploymentDegree <- tEmploymentDegree/sum(tEmploymentDegree) 
    tOstWestDens <- table(Ost_West = S$Ost_west, PopDens = S$PopDens); tOstWestDens <- tOstWestDens/sum(tOstWestDens)
    tOccDegr <- table(OccDegr = S$OccDegr); tOccDegr <- tOccDegr/sum(tOccDegr) 
    tFamState <- table(FamState = S$FamState, kidsHH=S$kidsHH); tFamState <- tFamState/sum(tFamState) 
    tRel <- table(Rel = S$Rel); tRel <- tRel/sum(tRel) 
    #tPart <- table(partyTend = S$partyTend); tPart <- tPart/sum(tPart) 

# 2. Conduct raking
    # find factor to multiply to Civey data to match the SOEP distribution
    # do this for each question differently due to distinct sample for each question in Civey data
    N= sum(S$W_SOEP) # total population in private households aged 18+ in 2021 acc official stats (Microzensus); around 68 Mio. persons
    pAgeSex <- round(tAgeSex*N); rownames(pAgeSex)[rownames(pAgeSex) %in% "65+"] <- "65 +"
    pEmploymentDegree <- round(tEmploymentDegree*N) 
    pOstWestDens <- round(tOstWestDens*N)
    pOccDegr <- round(tOccDegr*N)
    pFamState <- round(tFamState*N) 
    pRel <- round(tRel*N)

    # apply to Civey data
    ddata <- svydesign(id=~ID, weights=~1, data=CM)
    postStr <- rake(
      design = ddata,
      sample.margins = list(~Age+Sex, ~EmplyStatus+SchoolDegree, ~Ost_West+PopDens, ~OccDegr, ~FamState+kidsHH, ~Rel),
      population.margins = list(pAgeSex, pEmploymentDegree, pOstWestDens, pOccDegr, pFamState, pRel),
      control = list(maxit = 50)
    )
    calweight <- weights(postStr)
    ddata1 <- svydesign(id=~ID, weights=~calweight, data=CM)
    svytable(~ Age + Sex, design = ddata1); pAgeSex
    svytable(~ EmplyStatus+SchoolDegree, design = ddata1); pEmploymentDegree
    svytable(~ Age + Sex, design = ddata1); pAgeSex
    svytable(~ Ost_West+PopDens, design = ddata1); pOstWestDens
    svytable(~ OccDegr, design = ddata1); pOccDegr
    svytable(~ FamState+kidsHH, design = ddata1); pFamState
    svytable(~ Rel, design = ddata1); pRel    
    # everything is fine: weighted data fits benchmarks
    
  
# ------------------
# Write Civey Data with calibrated weights
# ------------------
w <- cbind.data.frame(CM$ID, calweight)
colnames(w) <- c("ID", "calweight")
w$calweight_std <-w$calweight/mean(w$calweight)  
C <- merge(C, w, by="ID", all.x=TRUE)   
write_dta(C, "NonPropData_CalWeighted_v09022025.dta") 
