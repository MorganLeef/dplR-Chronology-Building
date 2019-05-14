###Regional Chronology Calculations

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

#ABBA
cv_region<-c("BFT","COR","DRT","FRE","MAL","MVT","RMR","SEW","VFD")
cv_region_abba<-combine.rwl(list(BFT_ABBA.rwl,COR_ABBA.rwl,DRT_ABBA.rwl,FRE_ABBA.rwl,MAL_ABBA.rwl,MVT_ABBA.rwl,RMR_ABBA.rwl,SEW_ABBA.rwl,VFD_ABBA.rwl))

#Detrend and create a chronology for the ABBA files in the Canaan Region
cv_abba.rwi <- detrend(rwl = cv_region_abba, method = "ModNegExp")
cv_abba.crn<- chron(cv_abba.rwi, prefix = "CVR")

years_ABBA<-as.numeric(rownames(cv_abba.crn))

sub_cv_abba<-subset(cv_abba.crn, years_ABBA > 1949 & years_ABBA < 2017)

crn.plot(sub_cv_abba)

#PCRU
cv_region_pcru<-combine.rwl(list(BFT_PCRU.rwl,COR_PCRU.rwl,DRT_PCRU.rwl,FRE_PCRU.rwl,MAL_PCRU.rwl,MVT_PCRU.rwl,RMR_PCRU.rwl,SEW_PCRU.rwl,VFD_PCRU.rwl))

#Detrend and create a chronology for the PCRU files in the Canaan Region
cv_pcru.rwi <- detrend(rwl = cv_region_pcru, method = "ModNegExp")
cv_pcru.crn<- chron(cv_pcru.rwi, prefix = "CVR")

years_ABBA<-as.numeric(rownames(cv_pcru.crn))

sub_cv_pcru<-subset(cv_pcru.crn, years_ABBA > 1949 & years_ABBA < 2017)

crn.plot(sub_cv_pcru)

###Dolly Sods Wilderness
########################

###ABBA
#Detrend and create a chronology for DOL ABBA
DOL_ABBA.rwi <- detrend(rwl = DOL_ABBA.rwl, method = "ModNegExp")
DOL_ABBA.crn<- chron(DOL_ABBA.rwi, prefix = "DOL")

years_ABBA<-as.numeric(rownames(DOL_ABBA.crn))
sub_dol_abba<-subset(DOL_ABBA.crn, years_ABBA > 1949 & years_ABBA < 2017)

crn.plot(sub_dol_abba)

###PCRU
#Detrend and create a chronology for DOL PCRU
DOL_PCRU.rwi <- detrend(rwl = DOL_PCRU.rwl, method = "ModNegExp")
DOL_PCRU.crn<- chron(DOL_PCRU.rwi, prefix = "DOL")

years_PCRU<-as.numeric(rownames(DOL_PCRU.crn))
sub_dol_pcru<-subset(DOL_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)

crn.plot(sub_dol_pcru)

###Beaver Dam Run Region
########################
###ABBA
#Detrend and create a chronology for BDR ABBA
BDR_ABBA.rwi <- detrend(rwl = BDR_ABBA.rwl, method = "ModNegExp")
BDR_ABBA.crn<- chron(BDR_ABBA.rwi, prefix = "BDR")

years_ABBA<-as.numeric(rownames(BDR_ABBA.crn))
sub_bdr_abba<-subset(BDR_ABBA.crn, years_ABBA > 1949 & years_ABBA < 2017)

crn.plot(sub_bdr_abba)

###PCRU
#Detrend and create a chronology for BDR PCRU
BDR_PCRU.rwi <- detrend(rwl = BDR_PCRU.rwl, method = "ModNegExp")
BDR_PCRU.crn<- chron(BDR_PCRU.rwi, prefix = "BDR")

years_PCRU<-as.numeric(rownames(BDR_PCRU.crn))
sub_bdr_pcru<-subset(BDR_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)

crn.plot(sub_bdr_pcru)

###Blister Run Swamp Region
###########################
###ABBA
#Detrend and create a chronology for BRS ABBA
BRS_ABBA.rwi <- detrend(rwl = BRS_ABBA.rwl, method = "ModNegExp")
BRS_ABBA.crn<- chron(BRS_ABBA.rwi, prefix = "BRS")

years_ABBA<-as.numeric(rownames(BRS_ABBA.crn))
sub_brs_abba<-subset(BRS_ABBA.crn, years_ABBA > 1949 & years_ABBA < 2017)

crn.plot(sub_brs_abba)

###PCRU
#Detrend and create a chronology for BRS PCRU
BRS_PCRU.rwi <- detrend(rwl = BRS_PCRU.rwl, method = "ModNegExp")
BRS_PCRU.crn<- chron(BRS_PCRU.rwi, prefix = "BRS")

years_PCRU<-as.numeric(rownames(BRS_PCRU.crn))
sub_brs_pcru<-subset(BRS_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)

crn.plot(sub_brs_pcru)

###Marlington Region
####################
###ABBA
#Detrend and create a chronology for MSH ABBA
MSH_ABBA.rwi <- detrend(rwl = MSH_ABBA.rwl, method = "ModNegExp")
MSH_ABBA.crn<- chron(MSH_ABBA.rwi, prefix = "MSH")

years_ABBA<-as.numeric(rownames(MSH_ABBA.crn))
sub_msh_abba<-subset(MSH_ABBA.crn, years_ABBA > 1949 & years_ABBA < 2017)

crn.plot(sub_msh_abba)

###PCRU
#Detrend and create a chronology for MSH PCRU
MSH_PCRU.rwi <- detrend(rwl = MSH_PCRU.rwl, method = "ModNegExp")
MSH_PCRU.crn<- chron(MSH_PCRU.rwi, prefix = "MSH")

years_PCRU<-as.numeric(rownames(MSH_PCRU.crn))
sub_msh_pcru<-subset(MSH_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)

crn.plot(sub_msh_pcru)

###Experimental Randolph Region merge (BDR and BRS)
###ABBA

randolph_region_abba<-combine.rwl(list(BDR_ABBA.rwl,BRS_ABBA.rwl))
randolph_abba.rwi <- detrend(rwl = randolph_region_abba, method = "ModNegExp")
randolph_abba.crn<- chron(randolph_abba.rwi, prefix = "RAN")

years_ABBA<-as.numeric(rownames(randolph_abba.crn))

sub_ran_abba<-subset(randolph_abba.crn, years_ABBA > 1949 & years_ABBA < 2017)

crn.plot(sub_ran_abba)


###PCRU
randolph_region_pcru<-combine.rwl(list(BDR_PCRU.rwl,BRS_PCRU.rwl))
randolph_pcru.rwi <- detrend(rwl = randolph_region_pcru, method = "ModNegExp")
randolph_pcru.crn<- chron(randolph_pcru.rwi, prefix = "RAN")

years_PCRU<-as.numeric(rownames(randolph_pcru.crn))

sub_ran_pcru<-subset(randolph_pcru.crn, years_ABBA > 1949 & years_ABBA < 2017)

crn.plot(sub_ran_pcru)





