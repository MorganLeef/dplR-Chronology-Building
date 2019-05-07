###New Corrected chronology code (using dfoliatR and suRge)
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)
library(dfoliatR)
library(suRge)

temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for BDR ABBA
SEW_ABBA.rwi <- detrend(rwl = SEW_ABBA.rwl, method = "ModNegExp")
###SEW_ABBA.crn<- chron(SEW_ABBA.rwi, prefix = "BDR")

#Detrend and create a chonology for BDR PCRU
SEW_PCRU.rwi <- detrend(rwl = SEW_PCRU.rwl, method = "ModNegExp")
SEW_PCRU.crn<- chron(SEW_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(SEW_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(SEW_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(SEW_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(SEW_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
plot_defol(ef_defol, col_defol = "red")

###
gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
plot_outbreak(ef_comp)



###Advanced ggplots
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
t_shift <- scales::trans_new("shift",transform = function(x) {x-1},inverse = function(x) {x+1})

Year<-sub_year_abba
RWI<-ef_comp$mean_gsi
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("CV Sewer Plant Corrected Chronology")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

SEW_rot<-read.csv("rotholz_data/SEW.csv")

df<-melt(SEW_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"))+
  scale_y_continuous(limits = c(0, 50))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))





