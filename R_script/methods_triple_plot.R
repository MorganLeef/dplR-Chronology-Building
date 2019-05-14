###New Corrected chronology code (using dfoliatR and suRge) - Triple - Methods Walkthrogh Plot
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)
library(dfoliatR)
library(suRge)
library(suRge)      ###overrite masking

temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for BDR ABBA
BDR_ABBA.rwi <- detrend(rwl = BDR_ABBA.rwl, method = "ModNegExp")
BDR_ABBA.crn<- chron(BDR_ABBA.rwi, prefix = "BDR")

#Detrend and create a chonology for BDR PCRU
BDR_PCRU.rwi <- detrend(rwl = BDR_PCRU.rwl, method = "ModNegExp")
BDR_PCRU.crn<- chron(BDR_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(BDR_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(BDR_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(BDR_ABBA.crn, years_ABBA > 1979 & years_ABBA < 2017)
sub_abba_rwi<-subset(BDR_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(BDR_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_abba_rwi$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(sub_abba_rwi, sub_pcru)

###
defol_object<-ef_defol[c(2:352),c(1:5)]
###plot_defol(defol_object, col_defol = "red")

###
###gantt_object<-ef_defol[c(2:200),c(1:5)]
###gantt_plot(gantt_object)


###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
###plot_outbreak(ef_comp)



###Advanced ggplots
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})

Year<-sub_year_abba
RWI<-ef_comp$mean_gsi

linetype = rep('dashed')

df<-data.frame(Time=sub_year_abba, Host=sub_abba$BDRstd, Non_Host=sub_pcru$BDPstd, RWI=sub_abba$BDRstd)

p1<-ggplot(df, aes(x=Time, y=RWI))+
  geom_line(aes(y = Host, linetype="Host"),size=.75)+
  geom_line(aes(y = Non_Host, linetype="Non_Host"), size=.75)+
  scale_linetype_manual(name="Index",values=c(Host="solid", Non_Host="dashed"))+
  ggtitle("Beaver Dam Run Host/Non-Host Chronology")+
  theme(legend.position=c(.95,-.1))+
  ylim(-1.5,2.5)

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


df_R<-data.frame(Time=sub_year_abba,Non_Host=R$BDPstd, Host=sub_abba$BDRstd, RWI=sub_abba$BDRstd)


p2<-ggplot(data=df_R, aes(x=Time, y=RWI))+
  geom_line(aes(y=Host, linetype= "Host"), size=.75)+
  geom_line(aes(y=Non_Host, linetype="Non_Host"), size=.75)+
  scale_linetype_manual(name="Index",values=c(Host="solid", Non_Host="dashed"))+
  ggtitle("Host and Adjusted Non-host Indices (R)")+
  theme(legend.position=c(.95,-.1))+
  ylim(-1.5,2.5)

df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p3<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Beaver Dam Run - Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Below Mean","Above Mean"))+
  theme(legend.position=c(.95,-.05))

ggarrange(p1, p2, p3, ncol=1, nrow=3, widths=c(1980,2016))
