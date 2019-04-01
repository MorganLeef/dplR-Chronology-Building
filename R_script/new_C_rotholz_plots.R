rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)

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
BDR_abba.hsd<-apply(sub_abba, 2, sd)
BDR_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-BDR_abba.hsd/BDR_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BDPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

###Rotholz Dataframe Prep
BDR_rot<-read.csv("BDR.csv")

df<-data.frame(Time=sub_year_abba, Host=sub_abba$BDRstd, Non_Host=sub_pcru$BDPstd, RWI=sub_abba$BDRstd)
t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$BDRstd
C_sub<-subset(C$BDRstd, C$BDRstd > 1.0)
C_sub2<-subset(C$BDRstd, C$BDRstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Beaver Dam Run Corrected Chronology")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))
  
df<-melt(BDR_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")
  
ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###BFT
rm(list=ls())
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))
#Detrend and create a chronology for BFT ABBA
BFT_ABBA.rwi <- detrend(rwl = BFT_ABBA.rwl, method = "ModNegExp")
BFT_ABBA.crn<- chron(BFT_ABBA.rwi, prefix = "BFT")

#Detrend and create a chonology for BFT PCRU
BFT_PCRU.rwi <- detrend(rwl = BFT_PCRU.rwl, method = "ModNegExp")
BFT_PCRU.crn<- chron(BFT_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(BFT_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(BFT_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(BFT_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(BFT_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
BFT_abba.hsd<-apply(sub_abba, 2, sd)
BFT_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-BFT_abba.hsd/BFT_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

BFT_rot<-read.csv("BFT.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$BFTstd
C_sub<-subset(C$BFTstd, C$BFTstd > 1.0)
C_sub2<-subset(C$BFTstd, C$BFTstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Balsam Fir Trail Corrected Chronology")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(BFT_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###BRS
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for BRS ABBA
BRS_ABBA.rwi <- detrend(rwl = BRS_ABBA.rwl, method = "ModNegExp")
BRS_ABBA.crn<- chron(BRS_ABBA.rwi, prefix = "BRS")

#Detrend and create a chonology for BRS PCRU
BRS_PCRU.rwi <- detrend(rwl = BRS_PCRU.rwl, method = "ModNegExp")
BRS_PCRU.crn<- chron(BRS_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(BRS_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(BRS_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(BRS_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(BRS_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
BRS_abba.hsd<-apply(sub_abba, 2, sd)
BRS_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-BRS_abba.hsd/BRS_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

BRS_rot<-read.csv("BRS.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$BRSstd
C_sub<-subset(C$BRSstd, C$BRSstd > 1.0)
C_sub2<-subset(C$BRSstd, C$BRSstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Blister Run Swamp")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(BRS_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###COR
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for COR ABBA
COR_ABBA.rwi <- detrend(rwl = COR_ABBA.rwl, method = "ModNegExp")
COR_ABBA.crn<- chron(COR_ABBA.rwi, prefix = "COR")

#Detrend and create a chonology for COR PCRU
COR_PCRU.rwi <- detrend(rwl = COR_PCRU.rwl, method = "ModNegExp")
COR_PCRU.crn<- chron(COR_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(COR_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(COR_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(COR_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(COR_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
COR_abba.hsd<-apply(sub_abba, 2, sd)
COR_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-COR_abba.hsd/COR_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

COR_rot<-read.csv("COR.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$CORstd
C_sub<-subset(C$CORstd, C$CORstd > 1.0)
C_sub2<-subset(C$CORstd, C$CORstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Courtland Road")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(COR_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###DOL
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for DOL ABBA
DOL_ABBA.rwi <- detrend(rwl = DOL_ABBA.rwl, method = "ModNegExp")
DOL_ABBA.crn<- chron(DOL_ABBA.rwi, prefix = "DOL")

#Detrend and create a chonology for DOL PCRU
DOL_PCRU.rwi <- detrend(rwl = DOL_PCRU.rwl, method = "ModNegExp")
DOL_PCRU.crn<- chron(DOL_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(DOL_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(DOL_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(DOL_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(DOL_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
DOL_abba.hsd<-apply(sub_abba, 2, sd)
DOL_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-DOL_abba.hsd/DOL_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

DOL_rot<-read.csv("DOL.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$DOLstd
C_sub<-subset(C$DOLstd, C$DOLstd > 1.0)
C_sub2<-subset(C$DOLstd, C$DOLstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Dolly Sods Wilderness")+
  scale_fill_manual(values=c(cblue, cred), name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(DOL_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###DRT
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for DRT ABBA
DRT_ABBA.rwi <- detrend(rwl = DRT_ABBA.rwl, method = "ModNegExp")
DRT_ABBA.crn<- chron(DRT_ABBA.rwi, prefix = "DRT")

#Detrend and create a chonology for DRT PCRU
DRT_PCRU.rwi <- detrend(rwl = DRT_PCRU.rwl, method = "ModNegExp")
DRT_PCRU.crn<- chron(DRT_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(DRT_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(DRT_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(DRT_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(DRT_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
DRT_abba.hsd<-apply(sub_abba, 2, sd)
DRT_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-DRT_abba.hsd/DRT_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

DRT_rot<-read.csv("DRT.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$DRTstd
C_sub<-subset(C$DRTstd, C$DRTstd > 1.0)
C_sub2<-subset(C$DRTstd, C$DRTstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Deer Run Trail")+
  scale_fill_manual(values=c(cblue, cred), name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(DRT_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###FRE
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for FRE ABBA
FRE_ABBA.rwi <- detrend(rwl = FRE_ABBA.rwl, method = "ModNegExp")
FRE_ABBA.crn<- chron(FRE_ABBA.rwi, prefix = "FRE")

#Detrend and create a chonology for FRE PCRU
FRE_PCRU.rwi <- detrend(rwl = FRE_PCRU.rwl, method = "ModNegExp")
FRE_PCRU.crn<- chron(FRE_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(FRE_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(FRE_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(FRE_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(FRE_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
FRE_abba.hsd<-apply(sub_abba, 2, sd)
FRE_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-FRE_abba.hsd/FRE_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

FRE_rot<-read.csv("FRE.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$FREstd
C_sub<-subset(C$FREstd, C$FREstd > 1.0)
C_sub2<-subset(C$FREstd, C$FREstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Freeland Road")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(FRE_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###MAL
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for MAL ABBA
MAL_ABBA.rwi <- detrend(rwl = MAL_ABBA.rwl, method = "ModNegExp")
MAL_ABBA.crn<- chron(MAL_ABBA.rwi, prefix = "MAL")

#Detrend and create a chonology for MAL PCRU
MAL_PCRU.rwi <- detrend(rwl = MAL_PCRU.rwl, method = "ModNegExp")
MAL_PCRU.crn<- chron(MAL_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(MAL_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(MAL_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(MAL_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(MAL_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
MAL_abba.hsd<-apply(sub_abba, 2, sd)
MAL_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-MAL_abba.hsd/MAL_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

MAL_rot<-read.csv("MAL.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$MALstd
C_sub<-subset(C$MALstd, C$MALstd > 1.0)
C_sub2<-subset(C$MALstd, C$MALstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Mallow Lake")+
  scale_fill_manual(values=c(cblue, cred), name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(MAL_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###MSH
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for MSH ABBA
MSH_ABBA.rwi <- detrend(rwl = MSH_ABBA.rwl, method = "ModNegExp")
MSH_ABBA.crn<- chron(MSH_ABBA.rwi, prefix = "MSH")

#Detrend and create a chonology for MSH PCRU
MSH_PCRU.rwi <- detrend(rwl = MSH_PCRU.rwl, method = "ModNegExp")
MSH_PCRU.crn<- chron(MSH_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(MSH_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(MSH_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(MSH_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(MSH_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
MSH_abba.hsd<-apply(sub_abba, 2, sd)
MSH_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-MSH_abba.hsd/MSH_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

MSH_rot<-read.csv("MSH.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$MSHstd
C_sub<-subset(C$MSHstd, C$MSHstd > 1.0)
C_sub2<-subset(C$MSHstd, C$MSHstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Marlington - Scenic Highway")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(MSH_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###MVT
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for MVT ABBA
MVT_ABBA.rwi <- detrend(rwl = MVT_ABBA.rwl, method = "ModNegExp")
MVT_ABBA.crn<- chron(MVT_ABBA.rwi, prefix = "MVT")

#Detrend and create a chonology for MSH PCRU
MVT_PCRU.rwi <- detrend(rwl = MVT_PCRU.rwl, method = "ModNegExp")
MVT_PCRU.crn<- chron(MVT_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(MVT_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(MVT_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(MVT_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(MVT_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
MVT_abba.hsd<-apply(sub_abba, 2, sd)
MVT_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-MVT_abba.hsd/MVT_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

MVT_rot<-read.csv("MVT.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$MVTstd
C_sub<-subset(C$MVTstd, C$MVTstd > 1.0)
C_sub2<-subset(C$MVTstd, C$MVTstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)


df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Middle Valley Trail")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(MVT_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###RMR
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for RMR ABBA
RMR_ABBA.rwi <- detrend(rwl = RMR_ABBA.rwl, method = "ModNegExp")
RMR_ABBA.crn<- chron(RMR_ABBA.rwi, prefix = "RMR")

#Detrend and create a chonology for RMR PCRU
RMR_PCRU.rwi <- detrend(rwl = RMR_PCRU.rwl, method = "ModNegExp")
RMR_PCRU.crn<- chron(RMR_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(RMR_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(RMR_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(RMR_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(RMR_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
RMR_abba.hsd<-apply(sub_abba, 2, sd)
RMR_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-RMR_abba.hsd/RMR_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

RMR_rot<-read.csv("RMR.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$RMRstd
C_sub<-subset(C$RMRstd, C$RMRstd > 1.0)
C_sub2<-subset(C$RMRstd, C$RMRstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Route 32/Manor Road Int.")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(RMR_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###SEW
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for SEW ABBA
SEW_ABBA.rwi <- detrend(rwl = SEW_ABBA.rwl, method = "ModNegExp")
SEW_ABBA.crn<- chron(SEW_ABBA.rwi, prefix = "SEW")

#Detrend and create a chonology for SEW PCRU
SEW_PCRU.rwi <- detrend(rwl = SEW_PCRU.rwl, method = "ModNegExp")
SEW_PCRU.crn<- chron(SEW_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(SEW_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(SEW_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(SEW_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(SEW_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
SEW_abba.hsd<-apply(sub_abba, 2, sd)
SEW_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-SEW_abba.hsd/SEW_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

SEW_rot<-read.csv("SEW.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$SEWstd
C_sub<-subset(C$SEWstd, C$SEWstd > 1.0)
C_sub2<-subset(C$SEWstd, C$SEWstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("CV Resort Sewer Plant")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(SEW_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###UPT
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for UPT ABBA
UPT_ABBA.rwi <- detrend(rwl = UPT_ABBA.rwl, method = "ModNegExp")
UPT_ABBA.crn<- chron(UPT_ABBA.rwi, prefix = "UPT")

#Detrend and create a chonology for SEW PCRU
UPT_PCRU.rwi <- detrend(rwl = UPT_PCRU.rwl, method = "ModNegExp")
UPT_PCRU.crn<- chron(UPT_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(UPT_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(UPT_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(UPT_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(UPT_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
UPT_abba.hsd<-apply(sub_abba, 2, sd)
UPT_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-UPT_abba.hsd/UPT_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R


UPT_rot<-read.csv("UPT.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$UPTstd
C_sub<-subset(C$UPTstd, C$UPTstd > 1.0)
C_sub2<-subset(C$UPTstd, C$UPTstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Upper Tract")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(UPT_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###VFD
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for VFD ABBA
VFD_ABBA.rwi <- detrend(rwl = VFD_ABBA.rwl, method = "ModNegExp")
VFD_ABBA.crn<- chron(VFD_ABBA.rwi, prefix = "VFD")

#Detrend and create a chonology for VFD PCRU
VFD_PCRU.rwi <- detrend(rwl = VFD_PCRU.rwl, method = "ModNegExp")
VFD_PCRU.crn<- chron(VFD_PCRU.rwi, prefix = "BFP")

#Create years object
years_ABBA<-as.numeric(rownames(VFD_ABBA.crn)) 
years_PCRU<-as.numeric(rownames(VFD_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(VFD_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(VFD_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

#Caluculate standard deviations for the chronologies from each species
VFD_abba.hsd<-apply(sub_abba, 2, sd)
VFD_pcru.nsd<-apply(sub_pcru, 2, sd)

#Divide the standard deviation of the host series by the non-host
coefficient<-VFD_abba.hsd/VFD_pcru.nsd

#Caluclate the mean for non-host chronology (Don't think this works)
n.host_mean <- mean(sub_pcru$BFPstd)

#Subtract the mean of the non-host detreneded index from the detrended non-host CHRONOLOGY (.crn)
quantity<-sub_pcru-n.host_mean

#Finally, multiplied the standard deviations ratio by the (detrened .crn - detrended index mean)
R<-coefficient*quantity

#And subtract that value (R) from the detreneded chronology of the host
C<-sub_abba-R

VFD_rot<-read.csv("VFD.csv")

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$VFDstd
C_sub<-subset(C$VFDstd, C$VFDstd > 1.0)
C_sub2<-subset(C$VFDstd, C$VFDstd < 1.0)  

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
corange = rgb(250,125,100, max=255) 
cblue2 = rgb(100,150,225, max=255)

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=1000)
df2i <- data.frame(Year=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"

p1<-ggplot(data=df2i, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("CV Volunteer Fire Dept.")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"))+
  theme(legend.position=c(.05,.1))

df<-melt(VFD_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

