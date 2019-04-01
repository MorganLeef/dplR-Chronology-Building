###DRT (1960)
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
sub_abba<-subset(DRT_ABBA.crn, years_ABBA > 1949 & years_ABBA < 2017)
sub_pcru<-subset(DRT_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)
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

DRT_rot<-read.csv("DRT_long.csv")

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

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=10000)
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
  scale_fill_manual(values=c(cred, cblue), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1950,2016))

###MAL (1964)
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
sub_abba<-subset(MAL_ABBA.crn, years_ABBA > 1949 & years_ABBA < 2017)
sub_pcru<-subset(MAL_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)
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

MAL_rot<-read.csv("MAL_long.csv")

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

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=10000)
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
  scale_fill_manual(values=c(cred, cblue), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1950,2016))

###BFT (1978)
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
sub_abba<-subset(BFT_ABBA.crn, years_ABBA > 1949 & years_ABBA < 2017)
sub_pcru<-subset(BFT_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)
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

BFT_rot<-read.csv("BFT_long.csv")

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

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=10000)
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
  scale_fill_manual(values=c(cred, cblue), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1950,2016))

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
sub_abba<-subset(SEW_ABBA.crn, years_ABBA > 1949 & years_ABBA < 2017)
sub_pcru<-subset(SEW_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)
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

SEW_rot<-read.csv("SEW_long.csv")

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

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=10000)
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
  scale_fill_manual(values=c(cred, cblue), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1950,2016))

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
sub_abba<-subset(UPT_ABBA.crn, years_ABBA > 1949 & years_ABBA < 2017)
sub_pcru<-subset(UPT_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)
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


UPT_rot<-read.csv("UPT_long.csv")

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

df2<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Year, df2$RWI, n=10000)
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
  scale_fill_manual(values=c(cred, cblue), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1950,2016))
