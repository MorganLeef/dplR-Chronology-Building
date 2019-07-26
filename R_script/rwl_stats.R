###Basic script to obtain all descriptive statistics for each site chronology
rm(list=ls())
library(dplR)

temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

##ABBA

#BFT ABBA
BFT_ABBA_stats<-rwl.stats(BFT_ABBA.rwl)
BFT_ABBA_fo_auto_cor<-data.frame(mean(BFT_ABBA_stats$ar1))

#BDR ABBA
BDR_ABBA_stats<-rwl.stats(BDR_ABBA.rwl)
BDR_ABBA_fo_auto_cor<-data.frame(mean(BDR_ABBA_stats$ar1))

#BRS ABBA
BRS_ABBA_stats<-rwl.stats(BRS_ABBA.rwl)
BRS_ABBA_fo_auto_cor<-data.frame(mean(BRS_ABBA_stats$ar1))

#COR ABBA
COR_ABBA_stats<-rwl.stats(COR_ABBA.rwl)
COR_ABBA_fo_auto_cor<-data.frame(mean(COR_ABBA_stats$ar1))

#DRT ABBA
DRT_ABBA_stats<-rwl.stats(DRT_ABBA.rwl)
DRT_ABBA_fo_auto_cor<-data.frame(mean(DRT_ABBA_stats$ar1))

#DOL ABBA
DOL_ABBA_stats<-rwl.stats(DOL_ABBA.rwl)
DOL_ABBA_fo_auto_cor<-data.frame(mean(DOL_ABBA_stats$ar1))

#FRE ABBA
FRE_ABBA_stats<-rwl.stats(FRE_ABBA.rwl)
FRE_ABBA_fo_auto_cor<-data.frame(mean(FRE_ABBA_stats$ar1))

#MAL ABBA
MAL_ABBA_stats<-rwl.stats(MAL_ABBA.rwl)
MAL_ABBA_fo_auto_cor<-data.frame(mean(MAL_ABBA_stats$ar1))

#MVT ABBA
MVT_ABBA_stats<-rwl.stats(MVT_ABBA.rwl)
MVT_ABBA_fo_auto_cor<-data.frame(mean(MVT_ABBA_stats$ar1))

#MSH ABBA
MSH_ABBA_stats<-rwl.stats(MSH_ABBA.rwl)
MSH_ABBA_fo_auto_cor<-data.frame(mean(MSH_ABBA_stats$ar1))
  
#RMR ABBA
RMR_ABBA_stats<-rwl.stats(RMR_ABBA.rwl)
RMR_ABBA_fo_auto_cor<-data.frame(mean(RMR_ABBA_stats$ar1))

#SEW ABBA
SEW_ABBA_stats<-rwl.stats(SEW_ABBA.rwl)
SEW_ABBA_fo_auto_cor<-data.frame(mean(SEW_ABBA_stats$ar1))

#UPT ABBA
UPT_ABBA_stats<-rwl.stats(UPT_ABBA.rwl)
UPT_ABBA_fo_auto_cor<-data.frame(mean(UPT_ABBA_stats$ar1))

#VFD ABBA
VFD_ABBA_stats<-rwl.stats(VFD_ABBA.rwl)
VFD_ABBA_fo_auto_cor<-data.frame(mean(VFD_ABBA_stats$ar1))

##PCRU

#BFT PCRU
BFT_PCRU_stats<-rwl.stats(BFT_PCRU.rwl)
BFT_PCRU_fo_auto_cor<-data.frame(mean(BFT_PCRU_stats$ar1))

#BDR PCRU
BDR_PCRU_stats<-rwl.stats(BDR_PCRU.rwl)
BDR_PCRU_fo_auto_cor<-data.frame(mean(BDR_PCRU_stats$ar1))

#BRS PCRU
BRS_PCRU_stats<-rwl.stats(BRS_PCRU.rwl)
BRS_PCRU_fo_auto_cor<-data.frame(mean(BRS_PCRU_stats$ar1))

#COR PCRU
COR_PCRU_stats<-rwl.stats(COR_PCRU.rwl)
COR_PCRU_fo_auto_cor<-data.frame(mean(COR_PCRU_stats$ar1))

#DRT PCRU
DRT_PCRU_stats<-rwl.stats(DRT_PCRU.rwl)
DRT_PCRU_fo_auto_cor<-data.frame(mean(DRT_PCRU_stats$ar1))

#DOL PCRU
DOL_PCRU_stats<-rwl.stats(DOL_PCRU.rwl)
DOL_PCRU_fo_auto_cor<-data.frame(mean(DOL_PCRU_stats$ar1))

#FRE PCRU
FRE_PCRU_stats<-rwl.stats(FRE_PCRU.rwl)
FRE_PCRU_fo_auto_cor<-data.frame(mean(FRE_PCRU_stats$ar1))

#MAL PCRU
MAL_PCRU_stats<-rwl.stats(MAL_PCRU.rwl)
MAL_PCRU_fo_auto_cor<-data.frame(mean(MAL_PCRU_stats$ar1))

#MVT PCRU
MVT_PCRU_stats<-rwl.stats(MVT_PCRU.rwl)
MVT_PCRU_fo_auto_cor<-data.frame(mean(MVT_PCRU_stats$ar1))

#MSH PCRU
MSH_PCRU_stats<-rwl.stats(MSH_PCRU.rwl)
MSH_PCRU_fo_auto_cor<-data.frame(mean(MSH_PCRU_stats$ar1))

#RMR PCRU
RMR_PCRU_stats<-rwl.stats(RMR_PCRU.rwl)
RMR_PCRU_fo_auto_cor<-data.frame(mean(RMR_PCRU_stats$ar1))

#SEW PCRU
SEW_PCRU_stats<-rwl.stats(SEW_PCRU.rwl)
SEW_PCRU_fo_auto_cor<-data.frame(mean(SEW_PCRU_stats$ar1))

#UPT PCRU
UPT_PCRU_stats<-rwl.stats(UPT_PCRU.rwl)
UPT_PCRU_fo_auto_cor<-data.frame(mean(UPT_PCRU_stats$ar1))

#VFD PCRU
VFD_PCRU_stats<-rwl.stats(VFD_PCRU.rwl)
VFD_PCRU_fo_auto_cor<-data.frame(mean(VFD_PCRU_stats$ar1))


