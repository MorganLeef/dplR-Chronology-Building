###Rotholz/Resin Canal Data Plotting

rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)

BDR_rot<-read.csv("BDR.csv")

#WIP line plot
ggplot(BDR_rot, aes(x=Year))+
  geom_line(aes(y=BDR_rot$Rotholz_n,linetype = "Rotholz_n"))+
  geom_line(aes(y=BDR_rot$Rotholz_p,linetype = "Rotholz_p"))+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Percent of Cores"))+
  labs(y = "Number of Cores",x = "Year")+
  theme(legend.position = c(.05,.2))+
  scale_linetype_manual(name="Legend",values=c(Rotholz_n="solid", Rotholz_p="dashed"))

###Test for first double plot
#Full plot (one per site)
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

df<-data.frame(Time=sub_year_abba, Host=sub_abba$BDRstd, Non_Host=sub_pcru$BDPstd, RWI=sub_abba$BDRstd)
t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})
Year<-sub_year_abba
RWI<-C$BDRstd
C_sub<-subset(C$BDRstd, C$BDRstd > 1.0)
C_sub2<-subset(C$BDRstd, C$BDRstd < 1.0)  
df_small<-data.frame(Year=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Year=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Year=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)
df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)

p1<-ggplot( data=df, aes(x=Time, y=RWI))+ 
  geom_bar(stat = 'identity', aes(fill = RWI<1), position = 'dodge', col = 'transparent')+
  scale_y_continuous(trans = t_shift)+
  theme(legend.position=c(.04,.2))+
  ggtitle("Corrected Host Chronology (C)")

p2<-ggplot(BDR_rot, aes(x=Year))+
  geom_line(aes(y=BDR_rot$Rotholz_n))+
  geom_line(aes(y=BDR_rot$Rotholz_p),linetype = "dashed")+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Percent of Cores"))+
  labs(y = "Number of Cores",x = "Year")+
  theme(legend.position = c(.05,.2))

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))


#Arguement to change xy bounds
coord_cartesian(ylim = c(0, 100))   

  
#Bar chart method  
df<-melt(BDR_rot,id.vars = "Year")
  
ggplot(df, aes(x = Year, y = value,fill=variable)) +
  geom_bar(stat='identity')  
