rm(list=ls())
library(dplR)

#Interactive Detrend

BDR_ABBA<-read.rwl("BDR_ABBA.rwl")

temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

BDR01AAB <- BDR_ABBA[, "BDR01AAB"] 
names(BDR01AAB)<-rownames(BDR_ABBA)
BDR01AAB.rwi <- detrend.series(y = BDR01AAB, y.name = "BDR01AAB", verbose=TRUE)

series<-detrend(rwl = BDR_ABBA.rwl, verbose = TRUE, make.plot = TRUE)


#BDR_ABBA
plot(BDR_ABBA.rwl, plot.type = "spag")

i.detrend(BDR_ABBA.rwl, f = 0.1)


#BDR_PCRU
plot(BDR_PCRU.rwl, plot.type = "spag")

i.detrend(BDR_PCRU.rwl)

pcru<-detrend(rwl=BDR_PCRU.rwl, verbose = TRUE, make.plot = TRUE)
#BFT_ABBA
plot(BFT_ABBA.rwl, plot.type = "spag")

i.detrend(BFT_ABBA.rwl)


#BFT_PCRU
plot(BFT_PCRU.rwl, plot.type = "spag")

i.detrend(BFT_PCRU.rwl)


#BRS_ABBA
plot(BRS_ABBA.rwl, plot.type = "spag")

i.detrend(BRS_ABBA.rwl)


#BRS_PCRU
plot(BRS_PCRU.rwl, plot.type = "spag")

i.detrend(BRS_PCRU.rwl)


#COR_ABBA
plot(COR_ABBA.rwl, plot.type = "spag")

i.detrend(COR_ABBA.rwl)


#COR_PCRU
plot(COR_PCRU.rwl, plot.type = "spag")

i.detrend(COR_PCRU.rwl)


#DOL_ABBA
plot(DOL_ABBA.rwl, plot.type = "spag")

i.detrend(DOL_ABBA.rwl)


#BDR_PCRU
plot(DOL_PCRU.rwl, plot.type = "spag")

i.detrend(BDR_PCRU.rwl)



#DRT_ABBA
plot(DRT_ABBA.rwl, plot.type = "spag")

i.detrend(DRT_ABBA.rwl)


#DRT_PCRU
plot(DRT_PCRU.rwl, plot.type = "spag")

i.detrend(DRT_PCRU.rwl)


#FRE_ABBA
plot(FRE_ABBA.rwl, plot.type = "spag")

i.detrend(FRE_ABBA.rwl)


#FRE_PCRU
plot(FRE_PCRU.rwl, plot.type = "spag")

i.detrend(FRE_PCRU.rwl)


#MAL_ABBA
plot(MAL_ABBA.rwl, plot.type = "spag")

i.detrend(MAL_ABBA.rwl)


#MAL_PCRU
plot(MAL_PCRU.rwl, plot.type = "spag")

i.detrend(MAL_PCRU.rwl)


#MSH_ABBA
plot(MSH_ABBA.rwl, plot.type = "spag")

i.detrend(MSH_ABBA.rwl)


#MSH_PCRU
plot(MSH_PCRU.rwl, plot.type = "spag")

i.detrend(MSH_PCRU.rwl)


#MVT_ABBA
plot(MVT_ABBA.rwl, plot.type = "spag")

i.detrend(MVT_ABBA.rwl)


#MVT_PCRU
plot(MVT_PCRU.rwl, plot.type = "spag")

i.detrend(MVT_PCRU.rwl)


#RMR_ABBA
plot(RMR_ABBA.rwl, plot.type = "spag")

i.detrend(RMR_ABBA.rwl)


#RMR_PCRU
plot(RMR_PCRU.rwl, plot.type = "spag")

i.detrend(RMR_PCRU.rwl)


#SEW_ABBA
plot(SEW_ABBA.rwl, plot.type = "spag")

i.detrend(SEW_ABBA.rwl)


#SEW_PCRU
plot(SEW_PCRU.rwl, plot.type = "spag")

i.detrend(SEW_PCRU.rwl)


#UPT_ABBA
plot(UPT_ABBA.rwl, plot.type = "spag")

i.detrend(UPT_ABBA.rwl)


#UPT_PCRU
plot(UPT_PCRU.rwl, plot.type = "spag")

i.detrend(UPT_PCRU.rwl)


#VFD_ABBA
plot(VFD_ABBA.rwl, plot.type = "spag")

i.detrend(VFD_ABBA.rwl)


#VFD_PCRU
plot(VFD_PCRU.rwl, plot.type = "spag")

i.detrend(VFD_PCRU.rwl)