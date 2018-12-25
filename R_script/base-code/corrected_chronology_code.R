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
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

BDR_abba_std<-apply(sub_abba, 2, sd)
BDR_pcru_std<-apply(sub_pcru, 2, sd)

coefficient<-((BDR_abba_std)/(BDR_pcru_std))

mean(sub_pcru$BDPstd)

sub_pcru_mean<-subset(BDR_PCRU.rwi, years_PCRU > 1979 & years_PCRU < 2017)
mean<-rowMeans(sub_pcru_mean)

quantity<-((sub_pcru)-(mean))

R<-coefficient*quantity

C<-sub_abba-R

plot(BDR_ABBA.crn, add.spline=TRUE, nyrs=20)
plot(C, main="Beaver Dam Road", add.spline=TRUE, nyrs=20)

plot(sub_year_abba, C$BDRstd, type='o', axes=F, xlab='', ylab='', main= "Beaver Dam Run", col="blue", xlim=c(1980, 2017))
  axis(1, seq(1980, 2020, by=5))
  mtext(side=1, "Year", line=2)
  axis(2, seq(-0.5, 3.0, by=0.5))
  mtext(side=2, "RWI", line=2)
  abline(h=1.0, col="black")
  