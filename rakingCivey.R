################################################################################
# Modeling Selection by Adjusting To Benchmark Distributions taken from SOEP 
# Using Raking
# 09.02.2025
# SZinn
################################################################################

library(haven)
library(survey)
library(mice)
library(tidyverse)

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
colnames(S) <- c("ID","hid","mode","end","Sex", "partyTend","A1", "A2","A3","A4",           
                 "A5","SchoolDegree","CASMIN","OccDegr","OccPos","FamState",     
                 "Ost_West","W_SOEP","Age","kidsHH","EmployStatus","PopDens","Rel")       

# b) Civey Data (name properly, deal with NAs)
C <- read_dta("civeyConsData_A2ToA5.dta")
table(C$Edu)
C$Edu[ C$Edu %in% "Weiß nicht / keine Angabe"] <- "Anderes"
for(i in 1:ncol(C)){
  C[which(unlist(C[, i]) %in% ""),i] <- NA
  C[which(unlist(C[, i]) %in% "Weiß nicht / keine Angabe"),i] <- NA
}
colnames(C) <- c("ID", "cc", "Question", "end", "Answer", "W_Civey", "Age", "Sex", "PopDens", 
                 "KK", "partyTend", "Ost_West", "SchoolDegree", "OccDegr", "OccPos", "FamState", "kidsHH", "Rel", "EmployStatus")
CW <- C[C$Question %in% 2, !(colnames(C) %in% c("Question", "end", "Answer", "W_Civey", "KK"))]
md <- md.pattern(CW)
md[nrow(md),]/nrow(CW) # do not use occ position for weighting (8% missingness, all other variable impute with one random draw - just for weighting)
CW <- CW %>% mutate(across(everything(), as.factor))
imp <- mice(CW, m=1, method="cart", pred = quickpred(CW, exclude="id")) # TODO for robustness checks, produce at least 5 to 10 runs and check whether weights stay close to each other
CM <- complete(imp, action=1)# every entry needs a value - otherwise raking does not work

# ------------------
# Raking to the SOEP Distribution - full sample 
# ------------------

# 1. Derive Distribution from SOEP Data 
#    on Age+Sex, Family Status x Kids in HH, Employment Status x Educational Degree,
#    Occupational Position x Occupational Degree, EastWest  + PopDensity, Religion, (Party Tendency)
#    N= sum(S$W_SOEP) # total population in private households aged 18+ in 2021 acc official stats (Microzensus); around 68 Mio. persons

sdata <- svydesign(id=~ID, weights=~W_SOEP, data=S)    
tAgeSex <- svytable(~ Age + Sex, design = sdata); rownames(tAgeSex)[5] <- "65 +"
tEmploymentDegree <- svytable(~ EmployStatus + SchoolDegree, design = sdata) 
tOstWestDens <- svytable(~ Ost_West + PopDens, design = sdata) 
tOccDegr <- svytable(~ OccDegr, design = sdata)
tFamState <- svytable(~ FamState + kidsHH, design = sdata)
tRel <- svytable(~ Rel, design = sdata) 
    
# 2. Conduct raking
# find factor to multiply to Civey data to match the SOEP distribution
# do this for each question differently due to distinct sample for each question in Civey data
   
ddata <- svydesign(id=~ID, weights=~1, data=CM)
    postStr <- rake(
      design = ddata,
      sample.margins = list(~Age+Sex, ~EmployStatus+SchoolDegree, ~Ost_West+PopDens, ~OccDegr, ~FamState+kidsHH, ~Rel),
      population.margins = list(tAgeSex, tEmploymentDegree, tOstWestDens, tOccDegr, tFamState, tRel),
      control = list(maxit = 50)
    )
calweight <- weights(postStr)
ddata1 <- svydesign(id=~ID, weights=~calweight, data=CM)
cAgeSex <- svytable(~ Age + Sex, design = ddata1); cAgeSex/sum(cAgeSex); tAgeSex/sum(tAgeSex)
cEmploymentDegree <- svytable(~ EmployStatus+SchoolDegree, design = ddata1); cEmploymentDegree/sum(cEmploymentDegree); tEmploymentDegree/sum(tEmploymentDegree)
cOstWestDens <- svytable(~ Ost_West+PopDens, design = ddata1); cOstWestDens/sum(cOstWestDens); tOstWestDens/sum(tOstWestDens)
cOccDegr <- svytable(~ OccDegr, design = ddata1); cOccDegr/sum(cOccDegr); tOccDegr/sum(tOccDegr)
cFamState <- svytable(~ FamState+kidsHH, design = ddata1); cFamState/sum(cFamState); tFamState/sum(tFamState)
cRel <- svytable(~ Rel, design = ddata1); cRel/sum(cRel); tRel/sum(tRel)   

# 3. Write Civey Data with calibrated weights
w <- cbind.data.frame(CM$ID, calweight)
colnames(w) <- c("ID", "calweight")
w$calweight_std <-w$calweight/mean(w$calweight)  
CF <- merge(C, w, by="ID", all.x=TRUE)   
write_dta(CF, "NonPropData_CalWeighted_v11022025.dta") 

# ------------------
# Raking to the SOEP Distribution - only sample of cases with all answers available (A2-A5)
# ------------------

# 1. Conduct raking with complete cases only
CC <- CM[CM$cc %in% 1,]
ddata <- svydesign(id=~ID, weights=~1, data=CC)
postStr <- rake(
  design = ddata,
  sample.margins = list(~Age+Sex, ~EmployStatus+SchoolDegree, ~Ost_West+PopDens, ~OccDegr, ~FamState+kidsHH, ~Rel),
  population.margins = list(tAgeSex, tEmploymentDegree, tOstWestDens, tOccDegr, tFamState, tRel),
  control = list(maxit = 50)
)
calweight <- weights(postStr)
ddata1 <- svydesign(id=~ID, weights=~calweight, data=CC)
cAgeSex <- svytable(~ Age + Sex, design = ddata1); cAgeSex/sum(cAgeSex); tAgeSex/sum(tAgeSex)
cEmploymentDegree <- svytable(~ EmployStatus+SchoolDegree, design = ddata1); cEmploymentDegree/sum(cEmploymentDegree); tEmploymentDegree/sum(tEmploymentDegree)
cOstWestDens <- svytable(~ Ost_West+PopDens, design = ddata1); cOstWestDens/sum(cOstWestDens); tOstWestDens/sum(tOstWestDens)
cOccDegr <- svytable(~ OccDegr, design = ddata1); cOccDegr/sum(cOccDegr); tOccDegr/sum(tOccDegr)
cFamState <- svytable(~ FamState+kidsHH, design = ddata1); cFamState/sum(cFamState); tFamState/sum(tFamState)
cRel <- svytable(~ Rel, design = ddata1); cRel/sum(cRel); tRel/sum(tRel)   

# 3. Write Civey Data with calibrated weights
w <- cbind.data.frame(CC$ID, calweight)
colnames(w) <- c("ID", "calweight")
w$calweight_std <-w$calweight/mean(w$calweight)  
CCC <- merge(C[C$cc %in% 1,], w, by="ID", all.x=TRUE)
CCC <- CCC[!duplicated(CCC$ID), c("ID", "calweight", "calweight_std")]
range(CCC$calweight); hist(CCC$calweight)
q95 <- quantile(CCC$calweight, probs=0.95)
CCC$calweight_trim <- CCC$calweight
CCC$calweight_trim[CCC$calweight_trim>q95] <- q95
range(CCC$calweight_trim); hist(CCC$calweight_trim)
write_dta(CCC, "NonPropData_CompleteCases_CalWeighted_v26032025.dta")
