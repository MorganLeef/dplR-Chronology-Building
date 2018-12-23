#Read in .rwl

rm(list=ls())
library(dplR)

temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

BDR_ABBA.rwi <- detrend(rwl = BDR_ABBA.rwl, method = "ModNegExp") 
BDR_ABBA.crn<- chron(BDR_ABBA.rwi, prefix = "BDR")


BDR_PCRU.rwi <- detrend(rwl = BDR_PCRU.rwl, method = "ModNegExp")
BDR_PCRU.crn<- chron(BDR_PCRU.rwi, prefix = "BDP")

years_ABBA<-as.numeric(rownames(BDR_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(BDR_PCRU.crn))

sub_abba<-subset(BDR_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(BDR_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)

sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

coefficient<-(sd(sub_abba)/(sd(sub_pcru)))

meh<-rowVars(sub_abba, suma = NULL, std = TRUE)
