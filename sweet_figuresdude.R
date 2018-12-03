library(dplR)
COR_ABBA<-read.rwl("COR_ABBA.rwl")

COR_ABBA.rwi <- detrend(rwl = COR_ABBA, method = "Mean")
COR_ABBA.crn<- chron(COR_ABBA.rwi, prefix = "COR")
plot(COR_ABBA.crn, main="Courtland Road ABBA", add.spline=TRUE, nyrs=20)

COR_PCRU<-read.rwl("COR_PCRU.rwl")

COR_PCRU.rwi <- detrend(rwl = COR_PCRU, method = "Mean")
COR_PCRU.crn<- chron(COR_PCRU.rwi, prefix = "COP")
plot(COR_PCRU.crn, main="Courtland Road PCRU", add.spline=TRUE, nyrs=20)

years_ABBA<-as.numeric(rownames(COR_ABBA.crn))
years_PCRU<-as.numeric(rownames(COR_PCRU.crn))

par(mar=c(4,4,1,1))
plot(years_ABBA, COR_ABBA.crn$CORstd, type='l', axes=F, xlab='', ylab='', main = "Courtland Road", col="blue", xlim=c(1980, 2017))
  axis(1, seq(1980, 2020, by=5))
  mtext(side=1, "Year", line=2)
  axis(2, seq(-0.5, 2.0, by=0.5))
  mtext(side=2, "RWI", line=2)

par(new=TRUE)  
  plot(years_PCRU, COR_PCRU.crn$COPstd, type='l', col="red", axes=F, xlab='', ylab='', xlim=c(1980, 2017))

legend(x="center", legend=c("ABBA", "PCRU"),  
       col= c("blue", "red"), lty=c(1,1), bty="n", xjust=-2)



par(mfrow=c(3,1))
par(mar= c(4,4,1,1))
plot(years_ABBA, COR_ABBA.crn$CORstd, type='l', axes=F, xlab='', ylab='', main = "Courtland Road ABBA", col="blue",ylim = c(0.5,2.0), xlim=c(1980, 2017))
axis(1, seq(1980, 2020, by=5))
mtext(side=1, "Year", line=2)
axis(2, seq(-0.5, 2.0, by=0.5))
mtext(side=2, "RWI", line=2)
abline(h=1.0, col="black")
plot(years_PCRU, COR_PCRU.crn$COPstd, type='l', col="red", axes=F, xlab='', main = "Courtland Road PCRU", ylab='', ylim = c(0.5,2.0), xlim=c(1980, 2017))
axis(1, seq(1980, 2020, by=5))
mtext(side=1, "Year", line=2)
axis(2, seq(-0.5, 2.0, by=0.5))
mtext(side=2, "RWI", line=2)
abline(h=1.0, col="black")
plot(years_ABBA, COR_ABBA.crn$CORstd, type='l', axes=F, xlab='', ylab='', main = "Courtland Road", col="blue", ylim = c(0.5,2.0),xlim=c(1980, 2017))
axis(1, seq(1980, 2020, by=5))
mtext(side=1, "Year", line=2)
axis(2, seq(-0.5, 2.0, by=0.5))
mtext(side=2, "RWI", line=2)
abline(h=1.0, col="black")
par(new=TRUE)  
plot(years_PCRU, COR_PCRU.crn$COPstd, type='l', col="red", axes=F, xlab='', ylab='', ylim = c(0.5,2.0), xlim=c(1980, 2017))

legend(x="topright", legend=c("ABBA", "PCRU"),col= c("blue", "red"), lty=c(1,1), bty="n", xjust=-2)
