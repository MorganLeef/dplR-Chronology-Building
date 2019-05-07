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
UPT_ABBA.rwi <- detrend(rwl = UPT_ABBA.rwl, method = "ModNegExp")
UPT_ABBA.crn<- chron(UPT_ABBA.rwi, prefix = "UPT")

#Detrend and create a chonology for BDR PCRU
UPT_PCRU.rwi <- detrend(rwl = UPT_PCRU.rwl, method = "ModNegExp")
UPT_PCRU.crn<- chron(UPT_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(UPT_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(UPT_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(UPT_ABBA.crn, years_ABBA > 1943 & years_ABBA < 2017)
sub_pcru<-subset(UPT_PCRU.crn, years_PCRU > 1943 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

UPT_ABBA_rwi.txt<-write.table(sub_abba, file = "/Users/aleef/Desktop/UPT_ABBA_rwi.txt", row.names = T, col.names = T)
UPT_PCRU_crn.txt<-write.table(sub_pcru, file = "/Users/aleef/Desktop/UPT_PCRU_crn.txt", row.names = T, col.names = T)


###Defoliate R + suRge Usage (On board dataset)
data("dmj_h")
data("dmj_nh")

surge<-defoliate_trees(dmj_h,dmj_nh)
gantt_plot(surge)
