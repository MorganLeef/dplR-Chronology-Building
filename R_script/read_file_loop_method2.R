rm(list=ls())
library(dplR)

file_list<-list.files(pattern = "*ABBA.rwl")

for (index in 1:length(file_list)){
  a<-read.rwl(file_list[index])
  b<-chron(detrend(a, method = "Mean"), m = substring(file_list[index], 1,3))
  
  years_ABBA<-as.numeric(rownames(b))
  
  par(mar=c(4,4,1,1))
  plot(years_ABBA, b$xxxstd, type='l', axes=F, xlab='', ylab='', main = "Site ABBA", col="blue", xlim=c(1980, 2017))
  axis(1, seq(1980, 2020, by=5))
  mtext(side=1, "Year", line=2)
  axis(2, seq(-0.5, 2.0, by=0.5))
  mtext(side=2, "RWI", line=2)
}
