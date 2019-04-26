#Code to write .rwi and .crn objects into basic .txt files

#Old tucson format
fname<-write.tucson(rwl.df =sub_pcru,fname= tempfile(fileext=".rwl"), prec = 0.001)
library(devtools)

devtools::install_github("chguiterman/dfoliatR")
devtools::install_github("chguiterman/suRge")

#Better Way?
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)
library(dfoliatR)
library(suRge)

temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for BDR ABBA
SEW_ABBA.rwi <- detrend(rwl = SEW_ABBA.rwl, method = "ModNegExp")
SEW_ABBA.crn<- chron(SEW_ABBA.rwi, prefix = "BDR")

#Detrend and create a chonology for BDR PCRU
SEW_PCRU.rwi <- detrend(rwl = SEW_PCRU.rwl, method = "ModNegExp")
SEW_PCRU.crn<- chron(SEW_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(SEW_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(SEW_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(SEW_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(SEW_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

SEW_ABBA_rwi.txt<-write.table(sub_abba, file = "/Users/aleef/Desktop/SEW_ABBA_rwi.txt", row.names = T, col.names = T)
SEW_PCRU_crn.txt<-write.table(sub_pcru, file = "/Users/aleef/Desktop/SEW_PCRU_crn.txt", row.names = T, col.names = T)




