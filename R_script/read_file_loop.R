rm(list=ls())

library(dplR)

#Create a list of my files for both species (2 species and 2 files per site for 14 sites)
file_list_abba<-list.files(path = "data/", pattern = "*ABBA.rwl")
file_list_pcru<-list.files(path = "data/", pattern = "*PCRU.rwl")

setwd("data") #you should not need to do this, simply paste0('data/", filename)

#Loop that creates mean chronologies for each species for each site, and a difference chronology for each site
#Also creates a 4 x 4 plot with 
#1.) ABBA mean chronology 
#2.) PCRU mean chronology 
#3.) ABBA (host) - PCRU (non-host) difference chronology
#4.) Both chronologies plotted together
##AH replace the a, b, c, d with interpretable names.  you have different data in each 
##AH and the names should reflect what the object actually is - "rw" (ring widths), "index.rw" etc.

for (index_abba in 1:length(file_list_abba)){
  a<-read.rwl(file_list_abba[index_abba])    ###reads in each tuscon format file
  b<-chron(detrend(a, method = "Mean"), m = substring(file_list_abba[index_abba], 1,3))  ###creates a standardized index for each file, then a chronology

##AH excellent listing!  Here again, index should not be specific to pcru, index.rw?
##But only worked on first two sites, then aborted. Something wrong with BRS.rwl
    for (index_pcru in 1:length(file_list_pcru)){
    c<-read.rwl(file_list_pcru[index_pcru])   ###same as the last lines but for PCRU files
    d<-chron(detrend(c, method = "Mean"), m = substring(file_list_pcru[index_pcru], 1,3))
    
    years_ABBA<-as.numeric(rownames(b))  ###creates a callable "Years" object for the plot (ABBA)
    years_PCRU<-as.numeric(rownames(d))  ###creates a callable "Years" object for the plot (PCRU)
    
    sub_abba<-subset(b, years_ABBA > 1979 & years_ABBA < 2017) ###subsets each chronology to be the same length (1980-2016)
    sub_pcru<-subset(d, years_PCRU > 1979 & years_PCRU < 2017) ###not sure why I did this twice (too afraid to change)
    
    ###Subtract non-host index from host index
    diff<-(sub_abba - sub_pcru) ###performs basic index subtraction operation to simulate host vs non-host model (extremely simplified)
    
    sub_year_abba<-as.numeric(rownames(sub_abba))  ###subset sub_abba object so that it remains within bounds

    ## Cleaner script would make this a function and place in functions folder 
    ## Stacking the figures abba, pcru and diff would be easier to interpret
    ## change color of difference to a third color, I tried cyan
    
    par(mfrow=c(2,2))
    plot(years_ABBA, b$xxxstd, type='l', axes=F, xlab='', ylab='', main= file_list_abba[index_abba], col="blue", xlim=c(1980, 2017)) ###ABBA chronology
    axis(1, seq(1980, 2020, by=5))
    mtext(side=1, "Year", line=2)
    axis(2, seq(-0.5, 2.0, by=0.5))
    mtext(side=2, "RWI", line=2)
    abline(h=1.0, col="black")
    
    plot(years_PCRU, d$xxxstd, type='l', col="red", axes=F, xlab='', main = file_list_pcru[index_pcru], ylab='', ylim = c(0.5,2.0), xlim=c(1980, 2017)) ###PCRU chronology
    axis(1, seq(1980, 2020, by=5))
    mtext(side=1, "Year", line=2)
    axis(2, seq(-0.5, 2.0, by=0.5))
    mtext(side=2, "RWI", line=2)
    abline(h=1.0, col="black")
    
<<<<<<< HEAD
    plot(sub_year_abba, diff$xxxstd, type='l', axes=F, xlab='', ylab='', main = "ABBA/PCRU Difference", col="blue", ylim = c(-1.0,2.0),xlim=c(1980, 2017)) ###Difference plot
=======
    plot(sub_year_abba, diff$xxxstd, type='l', axes=F, xlab='', ylab='', main = "ABBA/PCRU Difference", col="cyan", ylim = c(-1.0,2.0),xlim=c(1980, 2017)) ###Difference plot
>>>>>>> 364b4a139abfa03efdc3fba941edadc413ecba0a
    axis(1, seq(1980, 2020, by=5))
    mtext(side=1, "Year", line=2)
    axis(2, seq(-1.5, 2.0, by=0.5))
    mtext(side=2, "RWI", line=2)
    abline(h=1.0, col="black")
    
    plot(years_ABBA, b$xxxstd, type='l', axes=F, xlab='', ylab='', main = "ABBA and PCRU", col="blue", ylim = c(0.5,2.0),xlim=c(1980, 2017))  ###Both plotted together
    axis(1, seq(1980, 2020, by=5))
    mtext(side=1, "Year", line=2)
    axis(2, seq(-0.5, 2.0, by=0.5))
    mtext(side=2, "RWI", line=2)
    abline(h=1.0, col="black")
    
    par(new=TRUE)
    plot(years_PCRU, d$xxxstd, type='l', col="red", axes=F, xlab='', ylab='', ylim = c(0.5,2.0), xlim=c(1980, 2017))
    
    legend(x="topright", legend=c("ABBA", "PCRU"),col= c("blue", "red"), lty=c(1,1), bty="n", xjust=-2, yjust = 4)
  }
}

#Phew #LOL!
