rm(list=ls())
library(dplR)

#Read in two files/species for one site (COR), then standardizes, and creates a mean chronology for each
COR_ABBA<-read.rwl("COR_ABBA.rwl")
COR_ABBA.rwi <- detrend(rwl = COR_ABBA, method = "Mean")
COR_ABBA.crn<- chron(COR_ABBA.rwi, prefix = "COR")

COR_PCRU<-read.rwl("COR_PCRU.rwl")
COR_PCRU.rwi <- detrend(rwl = COR_PCRU, method = "Mean")
COR_PCRU.crn<- chron(COR_PCRU.rwi, prefix = "COP")

#Create "Years" rowname as a callable object
years_ABBA<-as.numeric(rownames(COR_ABBA.crn))
years_PCRU<-as.numeric(rownames(COR_PCRU.crn))

#Since most sites have differenct length chronologies for each species, we must subset each chronology file to be within the same bounds
sub_abba<-subset(COR_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(COR_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)

#Basic host - non-host difference operation
diff<-(sub_abba - sub_pcru)


#Again, we subset the new object to the same lenght (1980 - 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))

#plots an adjusted difference 
plot(sub_year_abba, diff$CORstd, type='l', axes=F, xlab='', ylab='', main = "Courtland Road Diff", col="blue", ylim = c(-1.0,2.0),xlim=c(1980, 2017))
axis(1, seq(1980, 2020, by=5))
mtext(side=1, "Year", line=2)
axis(2, seq(-1.5, 2.0, by=0.5))
mtext(side=2, "RWI", line=2)
abline(h=1.0, col="black")