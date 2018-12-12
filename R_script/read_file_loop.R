rm(list=ls())
library(dplR)

filenames<-c("BDR", "BFT", "BRS", "COR", "DOL", "DRT", "FRE", "MAL", "MSH", "MVT", "RMR", "SEW", "UPT", "VFD")


for (filename in filenames){
  ABBA<-read.rwl(paste("data/",filename, "_ABBA.rwl", sep = ""))
  PCRU<-read.rwl(paste("data/",filename, "_PCRU.rwl", sep = ""))

  z <- detrend(rwl = ABBA, method = "Mean")
  assign(paste(ABBA),z)
  write.csv(z, paste(filename, "_ABBA.rwi", sep = ""))
  rm(z)
  y <- detrend(rwl = PCRU, method = "Mean")
  assign(paste(PCRU),y)
  write.csv(y, paste(filename, "_PCRU.rwi", sep = ""))
  rm(y)
}
  
  r<- chron(r, prefix = "sit")
  assign(paste(ABBA),z)
  write.csv(r, paste(filename, ".rwi", sep=""))
  
  
}
  
