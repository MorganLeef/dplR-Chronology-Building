#Read in .rwl

rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)

#Read in all files as an .rwl dataframe
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#########
###BDR###
#########
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


#ggplot2 plots 


df<-data.frame(Year=sub_year_abba, "RWI"=sub_abba$BDRstd, "RWI_PCRU"=sub_pcru$BDPstd) 
p1<-ggplot(df, aes(Year))+
  geom_line(aes(y = RWI, colour = "RWI ABBA"))+
  geom_line(aes(y = RWI_PCRU, colour = "RWI PCRU"))+
  geom_hline(yintercept = 1)+
  ggtitle("Beaver Dam Run")+
  theme(legend.position=c(.05,.1))

Year<-sub_year_abba
RWI<-C$BDRstd
C_sub<-subset(C$BDRstd, C$BDRstd > 1.0)
C_sub2<-subset(C$BDRstd, C$BDRstd < 1.0)  
df_small<-data.frame(Year=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Year=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Year=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)

cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)

p2<-ggplot( data=df, aes(x=Year, y=RWI))+ 
  geom_ribbon(aes(ymax=large, ymin=1,  fill = ">Mean"))+
  geom_ribbon(aes(ymax=1,  ymin=small, fill = "<Mean"))+
  geom_line()+
  geom_hline(yintercept = 1)+
  ggtitle("Beaver Dam Run Corrected Chronology")+
  scale_fill_manual(name='RWI', values=c("<Mean"=cblue,">Mean"=cred))+
  theme(legend.position=c(.05,.1))

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

  
Year<-sub_year_abba
RWI<-C$BDRstd
C_sub<-subset(C$BDRstd, C$BDRstd > 1.0)
C_sub2<-subset(C$BDRstd, C$BDRstd < 1.0)

  
    
ggplot(C, aes(Year,RWI))+
  geom_line(color="blue", size=1.0)+
  geom_hline(yintercept = 1)+
  ggtitle("Beaver Dam Run Corrected Chronology")


df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})

cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)

ggplot( data=df, aes(x=Time, y=RWI))+ 
  geom_ribbon(aes(ymax=large, ymin=1,  fill = ">Mean"))+
  geom_ribbon(aes(ymax=1,  ymin=small, fill = "<Mean"))+
  geom_hline(yintercept = 1)+
  ggtitle("Beaver Dam Run Corrected Chronology")+
  scale_fill_manual(name='RWI', values=c("<Mean"=cblue,">Mean"=cred))+
  theme(legend.position=c(.05,.1))

###Area plot for C, works but is deformed?
df2<-data.frame(Time=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI)  
interp <- approx(df2$Time, df2$RWI, n=1000)
df2i <- data.frame(Time=interp$x, RWI=interp$y)
df2i$Mean[df2i$RWI < 1] <- "<Mean"
df2i$Mean[df2i$RWI > 1] <- ">Mean"



ggplot(data=df2i, aes(x=Time, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Beaver Dam Run Corrected Chronology")+
  theme(legend.position=c(.05,.1))


  scale_fill_manual(values=c(cblue, cred), guide=FALSE)
  

###Area plot, works without color
ggplot(data=df, aes(x=Time))+
  geom_area(aes(y=small, fill="small"))+
  geom_area(aes(y=large, fill="large"))+
  scale_fill_manual(name="",values = c("small"=cblue, "large"=cred))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Beaver Dam Run Corrected Chronology")+
  theme(legend.position=c(.05,.1))
  

ggplot(data = df, aes(x=Time, y=RWI))+
  geom_area()+
  scale_y_continuous(trans=t_shift)

#########
###BFT###
#########
rm(list=ls())
BFT_ABBA.rwi <- detrend(rwl = BFT_ABBA.rwl, method = "ModNegExp") 
BFT_ABBA.crn<- chron(BFT_ABBA.rwi, prefix = "BFT")

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


#ggplot2 plots  
Year<-sub_year_abba
RWI<-C$BFTstd


ggplot(C, aes(Year,RWI))+
  geom_line(color="blue", size=1.0)+
  geom_hline(yintercept = 1)+
  ggtitle("Balsam Fir Trail Corrected Chronology")

df_small<-data.frame(Time=Year, RWI=ifelse(c(RWI)>1,NA,RWI))
df_large<-data.frame(Time=Year, RWI=ifelse(c(RWI)<1,NA,RWI))
df<-data.frame(Time=Year, RWI=RWI, small=df_small$RWI, large=df_large$RWI)

cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)

ggplot( data=df, aes(x=Time, y=RWI))+ 
  geom_ribbon(aes(ymax=large, ymin=1,  fill = ">Mean"))+
  geom_ribbon(aes(ymax=1,  ymin=small, fill = "<Mean"))+
  geom_line()+
  geom_hline(yintercept = 1)+
  ggtitle("Balsam Fir Trail Corrected Chronology")+
  scale_fill_manual(name='RWI', values=c("<Mean"=cblue,">Mean"=cred))+
  theme(legend.position=c(.05,.1))

t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})

cols<-c(".99<="="blue", ">=1.0"="red")


##Using Factor, legend messed up
ggplot( data=df, aes(x=Time, y=RWI,colour=factor(RWI)))+ 
  geom_bar(stat="identity", aes(fill=small))+
  geom_bar(stat="identity", aes(fill=large))+
  scale_y_continuous(trans = t_shift)+
  scale_fill_manual(name="legend", values = cols)+
  ggtitle("Balsam Fir Trail Corrected Chronology")+
  theme(legend.position=c(.05,.1))

##Bar with only 2 colors --NO GO
ggplot( data=df, aes(x=Time, y=RWI,colour=RWI))+ 
  geom_bar(stat="identity", aes(fill=small))+
  geom_bar(stat="identity", aes(fill=large))+
  scale_y_continuous(trans = t_shift)+
  ggtitle("Balsam Fir Trail Corrected Chronology")+
  theme(legend.position=c(.05,.1))

#Colored Bar plot that works!
ggplot( data=df, aes(x=Time, y=RWI))+ 
  geom_bar(stat = 'identity', aes(fill = RWI<1), position = 'dodge', col = 'transparent') +
  scale_y_continuous(trans = t_shift)+
  ggtitle("Balsam Fir Trail Corrected Chronology")+
  theme(legend.position=c(.05,.1))
