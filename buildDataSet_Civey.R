# Measuring Conspiracy Theories
# 25.06.2024
# SZ

library(haven)

# ------------------------
# READ DATA FROM FILES
# ------------------------
rm(list=ls())
path <- "D:\\VerschwDaten"
names <- list.files(path, pattern="*.csv", full.names=TRUE)

dat_q11 <- read.csv2(file=names[1])
dat_q12 <- read.csv2(file=names[2])
dat_q1 <- rbind.data.frame(dat_q11, dat_q12) # „Es geschehen viele sehr wichtige Dinge in der Welt, über die die Öffentlichkeit nie informiert wird“?
dat_q21 <- read.csv2(file=names[3])
dat_q22 <- read.csv2(file=names[4])
dat_q2 <- rbind.data.frame(dat_q21, dat_q22) # „Politiker geben uns normalerweise keine Auskunft über die wahren Motive ihrer Entscheidungen“?
dat_q31 <- read.csv2(file=names[5])
dat_q32 <- read.csv2(file=names[6])
dat_q3 <- rbind.data.frame(dat_q31, dat_q32) # "Inwieweit stimmen Sie der Aussage zu: „Regierungsbehörden überwachen alle Bürger genau“?
dat_q41 <- read.csv2(file=names[7])
dat_q42 <- read.csv2(file=names[8])
dat_q4 <- rbind.data.frame(dat_q41, dat_q42) # „Ereignisse, die auf den ersten Blick nicht miteinander in Verbindung zu stehen scheinen, sind oft das Ergebnis geheimer Aktivitäten“?
dat_q51 <- read.csv2(file=names[9])
dat_q52 <- read.csv2(file=names[10])
dat_q5 <- rbind.data.frame(dat_q51, dat_q52) # "Inwieweit stimmen Sie der Aussage zu: „Es gibt geheime Organisationen, die großen Einfluss auf politische Entscheidungen haben“?

dat_q1 <- dat_q1[,!(colnames(dat_q1) %in% "Frage")]; 
colnames(dat_q1)[colnames(dat_q1) %in% "Antwort"] <- "A1"; colnames(dat_q1)[colnames(dat_q1) %in% "Zeitpunkt"] <- "ZP1"
colnames(dat_q1)[colnames(dat_q1) %in% "Gewicht"] <- "w1"; colnames(dat_q1)[colnames(dat_q1) %in% "Alter"] <- "Age1"
colnames(dat_q1)[colnames(dat_q1) %in% "Geschlecht"] <- "G1"; colnames(dat_q1)[colnames(dat_q1) %in% "Bevölkerungsdichte"] <- "PD1"
colnames(dat_q1)[colnames(dat_q1) %in% "Kaufkraft"] <- "KK1"; colnames(dat_q1)[colnames(dat_q1) %in% "Sonntagsfrage"] <- "SF1"
colnames(dat_q1)[colnames(dat_q1) %in% "Ost_West"] <- "EW1"; colnames(dat_q1)[colnames(dat_q1) %in% "Bildungsabschluss"] <- "Edu1"
colnames(dat_q1)[colnames(dat_q1) %in% "Berufsbildung"] <- "OccEdu1"; colnames(dat_q1)[colnames(dat_q1) %in% "Berufliche.Stellung"] <- "OccPos1"
colnames(dat_q1)[colnames(dat_q1) %in% "Familienstand"] <- "Fam1"; colnames(dat_q1)[colnames(dat_q1) %in% "Kinder.im.Haushalt"] <- "Kids1"
colnames(dat_q1)[colnames(dat_q1) %in% "Religion"] <- "Rel1"; colnames(dat_q1)[colnames(dat_q1) %in% "Beschäftigungsstatus"] <- "Empl1"

dat_q2 <- dat_q2[,!(colnames(dat_q2) %in% "Frage")];
colnames(dat_q2)[colnames(dat_q2) %in% "Antwort"] <- "A2"; colnames(dat_q2)[colnames(dat_q2) %in% "Zeitpunkt"] <- "ZP2"
colnames(dat_q2)[colnames(dat_q2) %in% "Gewicht"] <- "w2"; colnames(dat_q2)[colnames(dat_q2) %in% "Alter"] <- "Age2"
colnames(dat_q2)[colnames(dat_q2) %in% "Geschlecht"] <- "G2"; colnames(dat_q2)[colnames(dat_q2) %in% "Bevölkerungsdichte"] <- "PD2"
colnames(dat_q2)[colnames(dat_q2) %in% "Kaufkraft"] <- "KK2"; colnames(dat_q2)[colnames(dat_q2) %in% "Sonntagsfrage"] <- "SF2"
colnames(dat_q2)[colnames(dat_q2) %in% "Ost_West"] <- "EW2"; colnames(dat_q2)[colnames(dat_q2) %in% "Bildungsabschluss"] <- "Edu2"
colnames(dat_q2)[colnames(dat_q2) %in% "Berufsbildung"] <- "OccEdu2"; colnames(dat_q2)[colnames(dat_q2) %in% "Berufliche.Stellung"] <- "OccPos2"
colnames(dat_q2)[colnames(dat_q2) %in% "Familienstand"] <- "Fam2"; colnames(dat_q2)[colnames(dat_q2) %in% "Kinder.im.Haushalt"] <- "Kids2"
colnames(dat_q2)[colnames(dat_q2) %in% "Religion"] <- "Rel2"; colnames(dat_q2)[colnames(dat_q2) %in% "Beschäftigungsstatus"] <- "Empl2"

dat_q3 <- dat_q3[,!(colnames(dat_q3) %in% "Frage")];
colnames(dat_q3)[colnames(dat_q3) %in% "Antwort"] <- "A3"; colnames(dat_q3)[colnames(dat_q3) %in% "Zeitpunkt"] <- "ZP3"
colnames(dat_q3)[colnames(dat_q3) %in% "Gewicht"] <- "w3"; colnames(dat_q3)[colnames(dat_q3) %in% "Alter"] <- "Age3"
colnames(dat_q3)[colnames(dat_q3) %in% "Geschlecht"] <- "G3"; colnames(dat_q3)[colnames(dat_q3) %in% "Bevölkerungsdichte"] <- "PD3"
colnames(dat_q3)[colnames(dat_q3) %in% "Kaufkraft"] <- "KK3"; colnames(dat_q3)[colnames(dat_q3) %in% "Sonntagsfrage"] <- "SF3"
colnames(dat_q3)[colnames(dat_q3) %in% "Ost_West"] <- "EW3"; colnames(dat_q3)[colnames(dat_q3) %in% "Bildungsabschluss"] <- "Edu3"
colnames(dat_q3)[colnames(dat_q3) %in% "Berufsbildung"] <- "OccEdu3"; colnames(dat_q3)[colnames(dat_q3) %in% "Berufliche.Stellung"] <- "OccPos3"
colnames(dat_q3)[colnames(dat_q3) %in% "Familienstand"] <- "Fam3"; colnames(dat_q3)[colnames(dat_q3) %in% "Kinder.im.Haushalt"] <- "Kids3"
colnames(dat_q3)[colnames(dat_q3) %in% "Religion"] <- "Rel3"; colnames(dat_q3)[colnames(dat_q3) %in% "Beschäftigungsstatus"] <- "Empl3"

dat_q4 <- dat_q4[,!(colnames(dat_q4) %in% "Frage")];
colnames(dat_q4)[colnames(dat_q4) %in% "Antwort"] <- "A4"; colnames(dat_q4)[colnames(dat_q4) %in% "Zeitpunkt"] <- "ZP4"
colnames(dat_q4)[colnames(dat_q4) %in% "Gewicht"] <- "w4"; colnames(dat_q4)[colnames(dat_q4) %in% "Alter"] <- "Age4"
colnames(dat_q4)[colnames(dat_q4) %in% "Geschlecht"] <- "G4"; colnames(dat_q4)[colnames(dat_q4) %in% "Bevölkerungsdichte"] <- "PD4"
colnames(dat_q4)[colnames(dat_q4) %in% "Kaufkraft"] <- "KK4"; colnames(dat_q4)[colnames(dat_q4) %in% "Sonntagsfrage"] <- "SF4"
colnames(dat_q4)[colnames(dat_q4) %in% "Ost_West"] <- "EW4"; colnames(dat_q4)[colnames(dat_q4) %in% "Bildungsabschluss"] <- "Edu4"
colnames(dat_q4)[colnames(dat_q4) %in% "Berufsbildung"] <- "OccEdu4"; colnames(dat_q4)[colnames(dat_q4) %in% "Berufliche.Stellung"] <- "OccPos4"
colnames(dat_q4)[colnames(dat_q4) %in% "Familienstand"] <- "Fam4"; colnames(dat_q4)[colnames(dat_q4) %in% "Kinder.im.Haushalt"] <- "Kids4"
colnames(dat_q4)[colnames(dat_q4) %in% "Religion"] <- "Rel4"; colnames(dat_q4)[colnames(dat_q4) %in% "Beschäftigungsstatus"] <- "Empl4"

dat_q5 <- dat_q5[,!(colnames(dat_q5) %in% "Frage")];
colnames(dat_q5)[colnames(dat_q5) %in% "Antwort"] <- "A5"; colnames(dat_q5)[colnames(dat_q5) %in% "Zeitpunkt"] <- "ZP5"
colnames(dat_q5)[colnames(dat_q5) %in% "Gewicht"] <- "w5"; colnames(dat_q5)[colnames(dat_q5) %in% "Alter"] <- "Age5"
colnames(dat_q5)[colnames(dat_q5) %in% "Geschlecht"] <- "G5"; colnames(dat_q5)[colnames(dat_q5) %in% "Bevölkerungsdichte"] <- "PD5"
colnames(dat_q5)[colnames(dat_q5) %in% "Kaufkraft"] <- "KK5"; colnames(dat_q5)[colnames(dat_q5) %in% "Sonntagsfrage"] <- "SF5"
colnames(dat_q5)[colnames(dat_q5) %in% "Ost_West"] <- "EW5"; colnames(dat_q5)[colnames(dat_q5) %in% "Bildungsabschluss"] <- "Edu5"
colnames(dat_q5)[colnames(dat_q5) %in% "Berufsbildung"] <- "OccEdu5"; colnames(dat_q5)[colnames(dat_q5) %in% "Berufliche.Stellung"] <- "OccPos5"
colnames(dat_q5)[colnames(dat_q5) %in% "Familienstand"] <- "Fam5"; colnames(dat_q5)[colnames(dat_q5) %in% "Kinder.im.Haushalt"] <- "Kids5"
colnames(dat_q5)[colnames(dat_q5) %in% "Religion"] <- "Rel5"; colnames(dat_q5)[colnames(dat_q5) %in% "Beschäftigungsstatus"] <- "Empl5"

table(dat_q1$Nutzer %in% dat_q2$Nutzer) # no overlaps
table(dat_q1$Nutzer %in% dat_q3$Nutzer) # no overlaps 
table(dat_q1$Nutzer %in% dat_q4$Nutzer) # no overlaps
table(dat_q1$Nutzer %in% dat_q5$Nutzer) # no overlaps
table(dat_q2$Nutzer %in% dat_q3$Nutzer) # N=3369 overlaps
table(dat_q2$Nutzer %in% dat_q4$Nutzer) # N=3050 overlaps
table(dat_q2$Nutzer %in% dat_q5$Nutzer) # N=3115
table(dat_q3$Nutzer %in% dat_q4$Nutzer) # N=3115
table(dat_q3$Nutzer %in% dat_q5$Nutzer) # N=3076
table(dat_q4$Nutzer %in% dat_q5$Nutzer) # N=3126

dat_q12 <- merge(dat_q1, dat_q2, by="Nutzer", all=TRUE)
dat_q34 <- merge(dat_q3, dat_q4, by="Nutzer", all=TRUE)
dat_q1234 <- merge(dat_q12, dat_q34, by="Nutzer", all=TRUE)
datAll <- merge(dat_q1234, dat_q5, by="Nutzer", all=TRUE)
datAll$ID <- 1:nrow(datAll)
datAll <- datAll[,!(colnames(datAll) %in% "Nutzer")] # N=18861

datAll$A1[datAll$A1 %in% "Stimme gar nicht zu"] <- 1
datAll$A1[datAll$A1 %in% "Stimme eher nicht zu"] <- 2
datAll$A1[datAll$A1 %in% "Teils-teils"] <- 3
datAll$A1[datAll$A1 %in% "Stimme eher zu"] <- 4
datAll$A1[datAll$A1 %in% "Stimme voll und ganz zu"] <- 5
datAll$A1 <- as.numeric(datAll$A1)

datAll$A2[datAll$A2 %in% "Stimme gar nicht zu"] <- 1
datAll$A2[datAll$A2 %in% "Stimme eher nicht zu"] <- 2
datAll$A2[datAll$A2 %in% "Teils-teils"] <- 3
datAll$A2[datAll$A2 %in% "Stimme eher zu"] <- 4
datAll$A2[datAll$A2 %in% "Stimme voll und ganz zu"] <- 5
datAll$A2 <- as.numeric(datAll$A2)

datAll$A3[datAll$A3 %in% "Stimme gar nicht zu"] <- 1
datAll$A3[datAll$A3 %in% "Stimme eher nicht zu"] <- 2
datAll$A3[datAll$A3 %in% "Teils-teils"] <- 3
datAll$A3[datAll$A3 %in% "Stimme eher zu"] <- 4
datAll$A3[datAll$A3 %in% "Stimme voll und ganz zu"] <- 5
datAll$A3 <- as.numeric(datAll$A3)

datAll$A4[datAll$A4 %in% "Stimme gar nicht zu"] <- 1
datAll$A4[datAll$A4 %in% "Stimme eher nicht zu"] <- 2
datAll$A4[datAll$A4 %in% "Teils-teils"] <- 3
datAll$A4[datAll$A4 %in% "Stimme eher zu"] <- 4
datAll$A4[datAll$A4 %in% "Stimme voll und ganz zu"] <- 5
datAll$A4 <- as.numeric(datAll$A4)

datAll$A5[datAll$A5 %in% "Stimme gar nicht zu"] <- 1
datAll$A5[datAll$A5 %in% "Stimme eher nicht zu"] <- 2
datAll$A5[datAll$A5 %in% "Teils-teils"] <- 3
datAll$A5[datAll$A5 %in% "Stimme eher zu"] <- 4
datAll$A5[datAll$A5 %in% "Stimme voll und ganz zu"] <- 5
datAll$A5 <- as.numeric(datAll$A5)

table(datAll$Empl1)
empl <- datAll$Empl1
datAll$Empl1 <- NA
datAll$Empl1[empl %in% c("Arbeitnehmer", "Selbstständige")] <- "Erwerbstätig"
datAll$Empl1[empl %in% c("Arbeitslose / Nichterwerbspersonen", "Rentner", "Studenten")] <- "Nicht Erwerbstätig"
table(datAll$Empl1, exclude=NULL)
table(datAll$Empl2)
empl <- datAll$Empl2
datAll$Empl2 <- NA
datAll$Empl2[empl %in% c("Arbeitnehmer", "Selbstständige")] <- "Erwerbstätig"
datAll$Empl2[empl %in% c("Arbeitslose / Nichterwerbspersonen", "Rentner", "Studenten")] <- "Nicht Erwerbstätig"
table(datAll$Empl2, exclude=NULL)
table(datAll$Empl3)
empl <- datAll$Empl3
datAll$Empl3 <- NA
datAll$Empl3[empl %in% c("Arbeitnehmer", "Selbstständige")] <- "Erwerbstätig"
datAll$Empl3[empl %in% c("Arbeitslose / Nichterwerbspersonen", "Rentner", "Studenten")] <- "Nicht Erwerbstätig"
table(datAll$Empl3, exclude=NULL)
table(datAll$Empl4)
empl <- datAll$Empl4
datAll$Empl4 <- NA
datAll$Empl4[empl %in% c("Arbeitnehmer", "Selbstständige")] <- "Erwerbstätig"
datAll$Empl4[empl %in% c("Arbeitslose / Nichterwerbspersonen", "Rentner", "Studenten")] <- "Nicht Erwerbstätig"
table(datAll$Empl4, exclude=NULL)
table(datAll$Empl1)
empl <- datAll$Empl5
datAll$Empl5 <- NA
datAll$Empl5[empl %in% c("Arbeitnehmer", "Selbstständige")] <- "Erwerbstätig"
datAll$Empl5[empl %in% c("Arbeitslose / Nichterwerbspersonen", "Rentner", "Studenten")] <- "Nicht Erwerbstätig"
table(datAll$Empl5, exclude=NULL)

write_dta(datAll, "D:\\VerschwDaten\\Data\\civeyConsData.dta")

# TODO employment is only yes or no

# ------------------------
# First Glance
# ------------------------
library("ggplot2")  
# „Es geschehen viele sehr wichtige Dinge in der Welt, über die die Öffentlichkeit nie informiert wird“?
ggplot(data=datAll, aes(x=A1)) + geom_bar(stat="bin") + ggtitle("There are many very important things happening in the world\nthat the public is never informed about")
#„Politiker geben uns normalerweise keine Auskunft über die wahren Motive ihrer Entscheidungen“?
ggplot(data=datAll, aes(x=A2)) + geom_bar(stat="bin") + ggtitle("Politicians don't usually tell us the true motives behind their decisions")
# „Regierungsbehörden überwachen alle Bürger genau“?
ggplot(data=datAll, aes(x=A3)) + geom_bar(stat="bin") + ggtitle("Government agencies closely monitor all citizens")
# „Ereignisse, die auf den ersten Blick nicht miteinander in Verbindung zu stehen scheinen, sind oft das Ergebnis geheimer Aktivitäten“?
ggplot(data=datAll, aes(x=A4)) + geom_bar(stat="bin") + ggtitle("Events that seem unrelated at first glance are often the result of secret activities")
# "Inwieweit stimmen Sie der Aussage zu: „Es gibt geheime Organisationen, die großen Einfluss auf politische Entscheidungen haben“?
ggplot(data=datAll, aes(x=A5)) + geom_bar(stat="bin") + ggtitle("There are secret organizations that have great influence on political decisions")
