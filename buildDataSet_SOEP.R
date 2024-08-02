library(haven)
library(naniar)
library(labelled)

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------
setwd("X:\\distribution\\soep-core\\soep.v38.1\\eu\\Stata_DE\\soepdata")
pl <- read_dta("pl.dta")
pg <- read_dta("pgen.dta")
pathl <- read_dta("ppathl.dta")
instr <- read_dta("instrumentation.dta")
hbrutt <- read_dta("hbrutt.dta") 
hl <- read_dta("hl.dta")
setwd("D:\\VerschwDaten\\Data") 
regionl <- read_dta("regionl_2021.dta") # data set contains sensitive spatial data (not part of std. SUF v38)  
popDens <- read.csv2("popDens.txt", sep="\t", header=FALSE) # taken from data base of federal statistical office

# ------------------------------------------------------------------------------
# EXTRACT VARIABLES
# ------------------------------------------------------------------------------
# From pathl
# ----------
pathr <- pathl[pathl$syear %in% "2021", c("pid", "hid", "sex", "gebjahr", "gebmonat")]  
hlr <- hl[hl$syear %in% "2021",]
table(pathr$hid %in% hlr$hid)
pathr <- pathr[pathr$hid %in% hlr$hid,]
# ----------
# From instr
# ----------
# mode: 1 CAPI, 2 CAWI, 3 CATI, 4 PAPI, 5 CAPIbyPhone, 6 CASI
instrr <- instr[instr$syear %in% 2021 & instr$status %in% 1, c("pid", "hid", "mode", "end")]
instrr <- instrr[!duplicated(instrr$pid),] # take last entry
table(instrr$pid %in% pathr$pid)
DAT <- merge(instrr, pathr, by="pid", all.x=TRUE)
DAT <- DAT[,!(colnames(DAT) %in% "hid.y")]
colnames(DAT)[colnames(DAT) %in% "hid.x"] <- "hid"
# ----------
# From pl
# --------
# plh0425i01, plh0425i02, plh0425i03, plh0425i04, plh0425i05: Conspiracy items
# plb0022_v11: employment
# plh0012_h: party tendency; plh0011_h: party preference
# plh0258_h: religion
plr <- pl[pl$syear %in% "2021",
          c("pid","hid", 
             "plb0022_v11", "plh0012_h", "plh0011_h", 
             "plh0425i01", "plh0425i02", "plh0425i03", "plh0425i04", "plh0425i05")]
table(plr$pid %in% instrr$pid) # eleven cases nor included
DAT <- merge(DAT, plr, by="pid", all.x=TRUE)
DAT <- DAT[,!(colnames(DAT) %in% "hid.y")]
colnames(DAT)[colnames(DAT) %in% "hid.x"] <- "hid"
colnames(DAT)[colnames(DAT) %in% "plb0022_v11"] <- "employment"
colnames(DAT)[colnames(DAT) %in% "plh0012_h"] <- "partyTend"
colnames(DAT)[colnames(DAT) %in% "plh0011_h"] <- "partyPref"
colnames(DAT)[colnames(DAT) %in% "plh0425i01"] <- "A1"
colnames(DAT)[colnames(DAT) %in% "plh0425i02"] <- "A2"
colnames(DAT)[colnames(DAT) %in% "plh0425i03"] <- "A3"
colnames(DAT)[colnames(DAT) %in% "plh0425i04"] <- "A4"
colnames(DAT)[colnames(DAT) %in% "plh0425i05"] <- "A5"

plr2019 <- pl[pl$syear %in% "2019", c("pid", "plh0258_h")]
colnames(plr2019)[2] <- "Rel2019"
plr2020 <- pl[pl$syear %in% "2020", c("pid", "plh0258_h")]
colnames(plr2020)[2] <- "Rel2020"
plr2021 <- pl[pl$syear %in% "2021", c("pid", "plh0258_h")]
colnames(plr2021)[2] <- "Rel2021"
plrRel <- merge(plr2021, plr2020, by="pid", all.x=TRUE)
plrRel <- merge(plrRel, plr2019, by="pid", all.x=TRUE)
missInd <- which(is.na(plrRel), arr.ind = TRUE)
plrRel[which(plrRel < 0, arr.ind = TRUE)] <- NA
plrRel$Rel2020[is.na(plrRel$Rel2020)] <- plrRel$Rel2019[is.na(plrRel$Rel2020)]
plrRel$Rel2021[is.na(plrRel$Rel2021)] <- plrRel$Rel2020[is.na(plrRel$Rel2021)]
DAT <- merge(DAT, plrRel[, c("pid", "Rel2021")], by="pid", all.x=TRUE)
# --------
# From pgen
# --------
# pgpsbil: school degree
# pgcasmin
# pgpbbil01: occupation degree
# pgstib: occupation position
# pgfamstd: family state (1,2,6,7,8 married; 3 single; 4 divorced, 5 widowed)
pgr <- pg[pg$syear %in% "2021", c("pid", "hid", "pgpsbil", "pgcasmin", "pgpbbil01", "pgstib", "pgfamstd")]
table(pgr$pid %in% plr$pid) # miss 369 cases that are in pgen but not in pl
table(plr$pid %in% pgr$pid) 
DAT <- merge(DAT, pgr, by="pid", all.x=TRUE)
DAT <- DAT[,!(colnames(DAT) %in% "hid.y")]
colnames(DAT)[colnames(DAT) %in% "hid.x"] <- "hid"
colnames(DAT)[colnames(DAT) %in% "pgpsbil"] <- "SchoolDegree"
colnames(DAT)[colnames(DAT) %in% "pgcasmin"] <- "CASMIN"
colnames(DAT)[colnames(DAT) %in% "pgpbbil01"] <- "OccDegr"
colnames(DAT)[colnames(DAT) %in% "pgstib"] <- "OccPos"
colnames(DAT)[colnames(DAT) %in% "pgfamstd"] <- "FamState"
# --------
# From hh
# --------
# kids in household: measured by kids in HH max 17 years old
# hlk0044_v1: kids in HH max 16 years born before survey time
# hlk0044_v2: kids in HH max 17 years born before survey time
# hlc0043: number kids
kidsHH <- hl[hl$syear %in% "2021", c("hid", "hlk0044_v1", "hlk0044_v2", "hlc0043")]
table(kidsHH$hlk0044_v1, exclude=NULL)
table(kidsHH$hlk0044_v2, exclude=NULL)
table(kidsHH$hlk0044_v1, kidsHH$hlk0044_v2, exclude=NULL)
kidsHH$kid17 <- ifelse(kidsHH$hlk0044_v1 %in% 1 | kidsHH$hlk0044_v2 %in% 1, 1, 
                       ifelse(kidsHH$hlk0044_v1 %in% 2 | kidsHH$hlk0044_v2 %in% 2, 0, NA))
kidsHH$kids <- ifelse(kidsHH$hlc0043 < 0, NA, 1)
table(kidsHH$kid17, exclude=NULL)
table(kidsHH$kid17, kidsHH$kids, exclude=NULL)
DAT <- merge(DAT, kidsHH[, c("hid", "kid17")], by="hid", all.x=TRUE)
# --------
# From regionl
# --------
regionr <- regionl[regionl$syear %in% "2021", c("hid", "bula_ew", "kkz")]
regionr$Ost_west <- ifelse(regionr$bula_ew %in% "22", 0, ifelse(regionr$bula_ew %in% "21", 1, NA)) # East=0, West=1
colnames(popDens) <- c("kkz","Kreis", "PopNumber")
table(regionr$kkz %in% popDens$kkz)
unique(regionr$kkz[!(regionr$kkz %in% popDens$kkz)]) # non-matches: missings in SOEP data
reg <- merge(regionr, popDens, by="kkz", all.x = TRUE)
reg$PopNumber[reg$PopNumber %in% "-"] <- NA
reg$PopNumber <- as.numeric(reg$PopNumber)
quint <- quantile(reg$PopNumber, probs = seq(0, 1, 0.20), na.rm = TRUE)
reg$PopClass <- NA
reg$PopClass[reg$PopNumber <= quint[2]] <- 0
reg$PopClass[reg$PopNumber > quint[2] & reg$PopNumber <= quint[3]] <- 1
reg$PopClass[reg$PopNumber > quint[3] & reg$PopNumber <= quint[4]] <- 2
reg$PopClass[reg$PopNumber > quint[4] & reg$PopNumber <= quint[5]] <- 3
reg$PopClass[reg$PopNumber > quint[5]] <- 4
DAT <- merge(DAT, reg[, c("hid", "Ost_west", "PopClass")], by="hid", all.x=TRUE)
# --------
# Add Weights
# --------
w_soep <- pathl[pathl$syear %in% 2021, c("pid", "phrf")]
DAT <- merge(DAT,w_soep, by="pid", all.x=TRUE)
colnames(DAT)[colnames(DAT) %in% "phrf"] <- "W_SOEP"

# --------------------
# check Miss Pattern
# --------------------
for(i in 1:ncol(DAT)){
  DAT[,i][DAT[,i]<0] <- NA
}
gg_miss_upset(DAT)
gg_miss_upset(DAT[, c("A1", "A2", "A3", "A4", "A5")])
DAT <- merge(DAT, ppathl[ppathl$syear %in% "2021", c("pid", "psample")], by="pid", all.x=TRUE)
DATmiss <- DAT[is.na(DAT$A1) & is.na(DAT$A2) & is.na(DAT$A3) & is.na(DAT$A4) & is.na(DAT$A5),] # N=7400
DATcc <- DAT[!(DAT$pid %in% DATmiss$pid),]
table(remove_labels(DATmiss$psample)) # refugee sample did not get the questions; few are in though since they are non-ref in refugee households
table(remove_labels(DATcc$psample)) 
DAT <- DAT[!(DAT$psample %in% c(17,18,19,24)),] # take refugee samples out
DAT <- DAT[!(is.na(DAT$A1) & is.na(DAT$A2) & is.na(DAT$A3) & is.na(DAT$A4) & is.na(DAT$A5)),]
table(DAT$mode)
DAT <- DAT[,!(colnames(DAT) %in% c("psample"))]
gg_miss_upset(DAT)

# -----------------------------------------------------------
# Make Variable Values comparable to those of Civey Data
# -----------------------------------------------------------
# Age Classes
getAge <- function(gebj, gebm, surveytime){
  if(is.na(gebj) | is.na(gebm) | is.na(surveytime)){
    return(NA)
  } else {
  bdate <- as.Date(paste( c(gebj, gebm, 15), collapse="-"), "%Y-%m-%d")
  if(nchar(as.character(surveytime))==13){
    sst <- unlist(strsplit(as.character(surveytime),split=""))
    surveytime <- paste(c(sst[1:7],"-15"), collapse="")
  }
  sdate <- as.Date(surveytime, "%Y-%m-%d")
  age <- as.numeric((sdate-bdate)/365.25)
  return(age)
  }
}
age <- c()
for(i in 1:nrow(DAT)){
  age <- c(age, getAge(DAT[i,c("gebjahr")], DAT[i,"gebmonat"], DAT[i,"end"]))
}
DAT$age <- NA
DAT$age[age < 30] <- "18 - 29"
DAT$age[age >= 30 & age < 40] <- "30 - 39"
DAT$age[age >= 40 & age < 50] <- "40 - 49"
DAT$age[age >= 50 & age < 65] <- "50 - 64"
DAT$age[age > 65] <- "65+"
DAT <- DAT[,!(colnames(DAT) %in% c("gebjahr", "gebmonat"))]
table(DAT$age, exclude=NULL) # N=1518 missings because of missing birth month (N=1375) and birth year (N=804)
# Occupational Position
DAT$OccPossO <- DAT$OccPos
DAT$OccPos <- NA
DAT$OccPos[DAT$OccPossO %in% c(110,120,130,140,150,510,522,520,530,521,540)] <- "Angestellter"
DAT$OccPos[DAT$OccPossO %in% c(210,220,230,240,250,310,320,330)] <- "Arbeiter"
DAT$OccPos[DAT$OccPossO %in% c(610,620,630,640)] <- "Beamter"
DAT$OccPos[DAT$OccPossO %in% c(340,550,560)] <- "Leitender Angestellter"
DAT$OccPos[DAT$OccPossO %in% c(10,11,12,13,15)] <- "nicht berufstätig"
DAT$OccPos[DAT$OccPossO %in% c(410,411,412,413,420,421,422,423,430,431,432,433,440)] <- "Selbstständig"
table(DAT$OccPossO, exclude=NULL)
table(DAT$OccPos, exclude=NULL)
DAT <- DAT[,!(colnames(DAT) %in% c("OccPossO"))]
# Gender
sexO <- DAT$sex
DAT$sex <- NA
DAT$sex[sexO %in% 1] <- "Männer"
DAT$sex[sexO %in% 2] <- "Frauen"
table(sexO, exclude=NULL)
table(DAT$sex, exclude = NULL)
# Family Status
famState <- DAT$FamState
DAT$FamState <- NA
DAT$FamState[famState %in% c(1,2,5,6,7,8)] <- "Verheiratet / Verwitwet"
DAT$FamState[famState %in% 4] <- "Geschieden"
DAT$FamState[famState %in% 3] <- "Ledig"
table(famState , exclude=NULL)
table(DAT$FamState, exclude = NULL)
# Educational Degree
SchoolDegree <- DAT$SchoolDegree
DAT$SchoolDegree <- NA
DAT$SchoolDegree[SchoolDegree %in% c(3,4)] <- "Abitur"
DAT$SchoolDegree[SchoolDegree %in% c(1,6,7,8)] <- "Hauptschule / kein Abschluss"
DAT$SchoolDegree[SchoolDegree %in% c(2)] <- "Mittlere Reife"
DAT$SchoolDegree[SchoolDegree %in% c(5)] <- "Anderes"
table(SchoolDegree, exclude=NULL)
table(DAT$SchoolDegree, exclude = NULL)
# Kids in Houshold
DAT$kidsHH <- NA
DAT$kidsHH[DAT$kid17 %in% 0 ] <- "Keine Kinder im Haushalt"
DAT$kidsHH[DAT$kid17 %in% 1 ] <- "Kinder im Haushalt"
table(DAT$kid17, exclude = NULL)
table(DAT$kidsHH, exclude = NULL)
DAT <- DAT[,!(colnames(DAT) %in% c("kid17"))]
# Employment Status
DAT$EmployStatus <- NA
DAT$EmployStatus[DAT$employment %in% c(1,2,3,4,7,10,11)] <- "Erwerbstätig"
DAT$EmployStatus[DAT$employment %in% c(5,9)] <- "Nicht Erwerbstätig"
table(DAT$employment, exclude=NULL)
table(DAT$EmployStatus, exclude=NULL)
DAT <- DAT[,!(colnames(DAT) %in% c("employment"))]
# Occupational Degree
DAT$OccDegr <- NA
DAT$OccDegr[DAT$CASMIN %in% c(0)] <- "Noch in Ausbildung"
DAT$OccDegr[DAT$CASMIN %in% c(1,2)] <- "Ohne Abschluss"
DAT$OccDegr[DAT$CASMIN %in% c(3,4,5,6,7)] <- "Berufsausbildung"
DAT$OccDegr[DAT$CASMIN %in% c(8,9)] <- "Studium oder höher"
table(DAT$CASMIN, exclude=NULL)
table(DAT$OccDegr, exclude=NULL)
# Party Tendency
ptend <- DAT$partyTend
DAT$partyTend <- NA
DAT$partyTend[ptend %in% 1] <- "SPD"
DAT$partyTend[ptend %in% c(2,3)] <- "CDU/CSU"
DAT$partyTend[ptend %in% 4] <- "FDP"
DAT$partyTend[ptend %in% 5] <- "Grüne"
DAT$partyTend[ptend %in% 6] <- "Linke"
DAT$partyTend[ptend %in% 27] <- "AfD"
DAT$partyTend[ptend %in% c(7,8)] <- "Sonstige"
table(ptend, exclude=NULL)
table(DAT$partyTend, exclude=NULL)
DAT <- DAT[,!(colnames(DAT) %in% c("partyPref"))]
# Party Tendency
DAT$PopDens <- NA
DAT$PopDens[DAT$PopClass %in% 0] <-"Sehr niedrig"  
DAT$PopDens[DAT$PopClass %in% 1] <-"Niedrig"
DAT$PopDens[DAT$PopClass %in% 2] <-"Mittel"
DAT$PopDens[DAT$PopClass %in% 3] <-"Hoch"
DAT$PopDens[DAT$PopClass %in% 4] <-"Sehr hoch"
table(DAT$PopClass, exclude=NULL)
table(DAT$PopDens, exclude=NULL)
DAT <- DAT[,!(colnames(DAT) %in% c("PopClass"))]
# Eastern / Western Germany 
ow <- DAT$Ost_west
DAT$Ost_west <- NA
DAT$Ost_west[ow %in% 0] <- "Osten"
DAT$Ost_west[ow %in% 1] <- "Westen"
table(ow, exclude = NULL)
table(DAT$Ost_west, exclude=NULL)
# Religion
DAT$Rel <- NA
DAT$Rel[DAT$Rel2021 %in% 2] <- "Evangelisch"
DAT$Rel[DAT$Rel2021 %in% 1] <- "Katholisch"
DAT$Rel[DAT$Rel2021 %in% c(3,4,5,7,8,9,10)] <- "Eine andere"
DAT$Rel[DAT$Rel2021 %in% 6] <- "Konfessionslos"
table(DAT$Rel2021, exclude=NULL)
table(DAT$Rel, exclude=NULL)
DAT <- DAT[,!(colnames(DAT) %in% c("Rel2021"))]

# store data as dta file
write_dta(DAT, "D:\\VerschwDaten\\Data\\soepConsData.dta")

