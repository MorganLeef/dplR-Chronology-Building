rm(list=ls())

library(dplR)

#Create a list of my files for both species (2 species and 2 files per site for 14 sites)
file_list_abba<-list.files(path = "data/", pattern = "*ABBA.rwl")
file_list_pcru<-list.files(path = "data/", pattern = "*PCRU.rwl")

setwd("data")

for (index_abba in 1:length(file_list_abba)){
  abba.rwl<-read.rwl(file_list_abba[index_abba])    
  abba.crn<-chron(detrend(abba.rwl, method = "ModNegExp"), m = substring(file_list_abba[index_abba], 1,3))
  
  
  for (index_pcru in 1:length(file_list_pcru)){
    pcru.rwl<-read.rwl(file_list_pcru[index_pcru])   
    pcru.crn<-chron(detrend(pcru.rwl, method = "ModNegExp"), m = substring(file_list_pcru[index_pcru], 1,3))
    
    years_ABBA<-as.numeric(rownames(abba.crn))  ###creates a callable "Years" object for the plot (ABBA)
    years_PCRU<-as.numeric(rownames(abba.crn))
    
    sub_abba<-subset(abba.crn, years_ABBA > 1979 & years_ABBA < 2017) ###subsets each chronology to be the same length (1980-2016)
    sub_pcru<-subset(pcru.crn, years_PCRU > 1979 & years_PCRU < 2017) ###not sure why I did this twice (too afraid to change)
    
    ###Data setup for host vs n.host operation
    sub_year_abba<-as.numeric(rownames(sub_abba))
    sub_year_pcru<-as.numeric(rownames(sub_pcru))
    
    #Remove samp.depth column
    sub_abba$samp.depth<-NULL
    sub_pcru$samp.depth<-NULL
    
    #Caluculate standard deviations for the chronologies from each species
    abba.hsd<-apply(sub_abba, 2, sd)
    pcru.nsd<-apply(sub_pcru, 2, sd)
    
    #Divide the standard deviation of the host series by the non-host
    coefficient<-abba.hsd/pcru.nsd
    
    #Caluclate the mean for non-host chronology
    n.host_mean <- mean(sub_pcru$xxxstd)
    
    #Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
    quantity<-sub_pcru-n.host_mean
    
    #Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
    R<-coefficient*quantity
    
    #And subtract that value (R) from the detreneded chronology of the host
    C<-sub_abba-R
  }
}
    
    ggplot(C, aes(Year,RWI))+
      geom_line(color="blue", size=1.0)+
      geom_hline(yintercept = 1)
  }
}
