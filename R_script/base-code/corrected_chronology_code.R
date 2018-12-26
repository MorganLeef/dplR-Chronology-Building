#Read in .rwl

rm(list=ls())
library(dplR)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for BDR ABBA
BDR_ABBA.rwi <- detrend(rwl = BDR_ABBA.rwl, method = "ModNegExp") 
BDR_ABBA.crn<- chron(BDR_ABBA.rwi, prefix = "BDR")

#Detrend and create a chonology for BDR PCRU
BDR_PCRU.rwi <- detrend(rwl = BDR_PCRU.rwl, method = "ModNegExp")
BDR_PCRU.crn<- chron(BDR_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(BDR_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(BDR_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(BDR_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(BDR_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
BDR_abba_std<-apply(sub_abba, 2, sd)
BDR_pcru_std<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-((BDR_abba_std)/(BDR_pcru_std))

#Caluclate the mean for non-host chronology (Don't think this works)
mean(sub_pcru$BDPstd)

#Calculate the mean for the non-host detrended INDEX (.rwi) Right???
sub_pcru_mean<-subset(BDR_PCRU.rwi, years_PCRU > 1979 & years_PCRU < 2017)
mean<-rowMeans(sub_pcru_mean)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-((sub_pcru)-(mean))

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

#Neither graph looks correct to me
plot(BDR_ABBA.crn, add.spline=TRUE, nyrs=20)
plot(C, main="Beaver Dam Road", add.spline=TRUE, nyrs=20)

plot(sub_year_abba, C$BDRstd, type='o', axes=F, xlab='', ylab='', main= "Beaver Dam Run", col="blue", xlim=c(1980, 2017))
  axis(1, seq(1980, 2020, by=5))
  mtext(side=1, "Year", line=2)
  axis(2, seq(-0.5, 3.0, by=0.5))
  mtext(side=2, "RWI", line=2)
  abline(h=1.0, col="black")
  