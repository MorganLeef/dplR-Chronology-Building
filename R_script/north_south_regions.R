#This script combines all 14 sites into two regional groups, and then creates
#a .crn file for each, to export as a text file
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)


###Canaan Region (BFT,COR,DRT,FRE,MAL,MVT,RMR,SEW,VFD)
######################################################
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))
#Northern Canaan Valley Region
#ABBA
cv_region_abba<-combine.rwl(list(BFT_ABBA.rwl,COR_ABBA.rwl,DOL_ABBA.rwl,DRT_ABBA.rwl,FRE_ABBA.rwl,MAL_ABBA.rwl,MVT_ABBA.rwl,RMR_ABBA.rwl,SEW_ABBA.rwl,UPT_ABBA.rwl,VFD_ABBA.rwl))

#Detrend and create a chronology for the ABBA files in the Canaan Region
cv_abba.rwi <- detrend(rwl = cv_region_abba, method = "ModNegExp")
cv_abba.crn<- chron(cv_abba.rwi, prefix = "CVR")

years_ABBA<-as.numeric(rownames(cv_abba.crn))

sub_cv_abba<-subset(cv_abba.crn, years_ABBA > 1959 & years_ABBA < 1986)

cv_abba_crn.txt<-write.table(sub_cv_abba, file = "/Users/aleef/Desktop/cv_abba_crn.txt", row.names = T, col.names = T)

#PCRU
cv_region_pcru<-combine.rwl(list(BFT_PCRU.rwl,COR_PCRU.rwl,DOL_PCRU.rwl,DRT_PCRU.rwl,FRE_PCRU.rwl,MAL_PCRU.rwl,MVT_PCRU.rwl,RMR_PCRU.rwl,SEW_PCRU.rwl,UPT_PCRU.rwl,VFD_PCRU.rwl))

#Detrend and create a chronology for the PCRU files in the Canaan Region
cv_pcru.rwi <- detrend(rwl = cv_region_pcru, method = "ModNegExp")
cv_pcru.crn<- chron(cv_pcru.rwi, prefix = "CVR")

years_PCRU<-as.numeric(rownames(cv_pcru.crn))

sub_cv_pcru<-subset(cv_pcru.crn, years_PCRU > 1959 & years_PCRU < 1986)

cv_pcru_crn.txt<-write.table(sub_cv_pcru, file = "/Users/aleef/Desktop/cv_pcru_crn.txt", row.names = T, col.names = T)


###crn.plot(sub_cv_abba)
###########################################
#Southern Pocahontas/Randolph County Region
###ABBA
poca_rand_abba<-combine.rwl(list(BDR_ABBA.rwl, BRS_ABBA.rwl,MSH_ABBA.rwl))

#Detrend and create a chronology for the ABBA files in the Canaan Region
poca_abba.rwi <- detrend(rwl = poca_rand_abba, method = "ModNegExp")
poca_abba.crn<- chron(poca_abba.rwi, prefix = "PRR")

years_ABBA<-as.numeric(rownames(poca_abba.crn))

sub_poca_abba<-subset(poca_abba.crn, years_ABBA > 1959 & years_ABBA < 1986)

poca_abba_crn.txt<-write.table(sub_poca_abba, file = "/Users/aleef/Desktop/poca_abba_crn.txt", row.names = T, col.names = T)

###crn.plot(sub_poca_abba)

#PCRU
poca_rand_pcru<-combine.rwl(list(BDR_PCRU.rwl, BRS_PCRU.rwl,MSH_PCRU.rwl))

#Detrend and create a chronology for the ABBA files in the Canaan Region
poca_pcru.rwi <- detrend(rwl = poca_rand_pcru, method = "ModNegExp")
poca_pcru.crn<- chron(poca_pcru.rwi, prefix = "PRR")

years_PCRU<-as.numeric(rownames(poca_pcru.crn))

sub_poca_pcru<-subset(poca_pcru.crn, years_PCRU > 1959 & years_PCRU < 1986)

poca_pcru_crn.txt<-write.table(sub_poca_abba, file = "/Users/aleef/Desktop/poca_pcru_crn.txt", row.names = T, col.names = T)

###crn.plot(sub_poca_pcru)



