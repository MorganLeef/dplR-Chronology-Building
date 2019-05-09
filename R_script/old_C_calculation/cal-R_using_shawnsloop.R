rm(list=ls())
setwd("C:/Users/aleef/Desktop/COFECHA/All_testcode/dplR-Chronology-Building")

library(dplR)

file_list_abba<-list.files(path = "data/", pattern = "*ABBA.rwl", full.names=TRUE)
file_list_pcru<-list.files(path = "data/", pattern = "*PCRU.rwl", full.names=TRUE)

crn.abba <- list()
for (index_abba in 1:length(file_list_abba)){
  crn.abba[[paste0(substring(file_list_abba[index_abba], 6, 8), ".crn")]] <- 
    chron(detrend(read.rwl(file_list_abba[index_abba]), method = "ModNegExp"), 
          prefix = substring(file_list_abba[index_abba], 6, 8))
  crn.abba[[paste0(substring(file_list_abba[index_abba], 6, 8), ".crn")]]$Year <- 
    row.names(crn.abba[[paste0(substring(file_list_abba[index_abba], 6, 8), ".crn")]])
}

crn.pcru <- list()
for (index_pcru in 1:length(file_list_pcru)){
  crn.pcru[[paste0(substring(file_list_pcru[index_pcru], 6, 8), ".crn")]] <-
    chron(detrend(read.rwl(file_list_pcru[index_pcru]), method = "ModNegExp"), 
          prefix = substring(file_list_pcru[index_pcru], 6, 8))
  crn.pcru[[paste0(substring(file_list_pcru[index_pcru], 6, 8), ".crn")]]$Year <- 
    row.names(crn.pcru[[paste0(substring(file_list_pcru[index_pcru], 6, 8), ".crn")]])
}

for (idx in 1:length(crn.abba)){
  sub_abba<-subset(substring(names(crn.abba[idx]),1,3), crn.pcru[[paste0(substring(file_list_pcru[index_pcru], 6, 8), ".crn")]]$Year > 1979 & crn.pcru[[paste0(substring(file_list_pcru[index_pcru], 6, 8), ".crn")]]$Year < 2017)
  sub_pcru<-subset(substring(names(crn.pcru[idx]),1,3), crn.pcru[[paste0(substring(file_list_pcru[index_pcru], 6, 8), ".crn")]]$Year > 1979 & crn.pcru[[paste0(substring(file_list_pcru[index_pcru], 6, 8), ".crn")]]$Year < 2017)
  sub_abba$samp.depth<-NULL
  sub_pcru$samp.depth<-NULL
  abba.hsd<-apply(sub_abba, 2, sd)
  pcru.nsd<-apply(sub_pcru, 2, sd)
  coefficient<-abba.hsd/pcru.nsd
  n.host_mean <- mean(sub_pcru$xxxstd)
  quantity<-sub_pcru-n.host_mean
  R<-coefficient*quantity
  C<-sub_abba-R
}
