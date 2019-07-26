###New Corrected chronology code (using dfoliatR and suRge)

###BDR
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)
library(tidyverse)
library(dfoliatR)
library(suRge)

bblue3 = rgb(100,150,255, max=255)
borange2 = rgb(255,150,100, max=255)
bblue = rgb(100,175,200, max=255)
bblue2 = rgb(100,175,150, max=255)
borange = rgb(175,100,100, max=255)
bred  = rgb(255,100,100, max=255)

temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

###BFT
#Detrend and create a chronology for BFT ABBA
BFT_ABBA.rwi <- detrend(rwl = BFT_ABBA.rwl, method = "ModNegExp")
###BFT_ABBA.crn<- chron(BFT_ABBA.rwi, prefix = "BFT")

#Detrend and create a chonology for BFT PCRU
BFT_PCRU.rwi <- detrend(rwl = BFT_PCRU.rwl, method = "ModNegExp")
BFT_PCRU.crn<- chron(BFT_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(BFT_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(BFT_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(BFT_ABBA.rwi, years_ABBA > 1949 & years_ABBA < 2017)
sub_pcru<-subset(BFT_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###Series 25A registered no infestation. Removing for gannt plot
#sub_abba$BFT25AAB<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)



###Advanced ggplots
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
bblue3 = rgb(100,150,255, max=255)
borange2 = rgb(255,150,100, max=255)
bblue = rgb(100,175,200, max=255)
bblue2 = rgb(100,175,150, max=255)
borange = rgb(175,100,100, max=255)
bred  = rgb(255,100,100, max=255)
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
  ggtitle("Balsam Fir Trail Full Growth Suppression Index")+
  scale_fill_manual(values=c(bblue3, borange2),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

BFT_rot<-read.csv("rotholz_data/BFT_long.csv")


df<-melt(BFT_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity', position = 'identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###DRT
#Detrend and create a chronology for ABBA
DRT_ABBA.rwi <- detrend(rwl = DRT_ABBA.rwl, method = "ModNegExp")


#Detrend and create a chonology for PCRU
DRT_PCRU.rwi <- detrend(rwl = DRT_PCRU.rwl, method = "ModNegExp")
DRT_PCRU.crn<- chron(DRT_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(DRT_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(DRT_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(DRT_ABBA.rwi, years_ABBA > 1949 & years_ABBA < 2017)
sub_pcru<-subset(DRT_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL


###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

#plot_outbreak(ef_comp)


###Advanced ggplots
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
bblue3 = rgb(100,150,255, max=255)
borange2 = rgb(255,150,100, max=255)
bblue = rgb(100,175,200, max=255)
bblue2 = rgb(100,175,150, max=255)
borange = rgb(175,100,100, max=255)
bred  = rgb(255,100,100, max=255)
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
  ggtitle("Deer Run Trail Full Growth Suppression Index")+
  scale_fill_manual(values=c(bblue3, borange2),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

DRT_rot<-read.csv("rotholz_data/DRT_long.csv")


df<-melt(DRT_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity', position = 'identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###MAL
#Detrend and create a chronology for ABBA
MAL_ABBA.rwi <- detrend(rwl = MAL_ABBA.rwl, method = "ModNegExp")


#Detrend and create a chonology for PCRU
MAL_PCRU.rwi <- detrend(rwl = MAL_PCRU.rwl, method = "ModNegExp")
MAL_PCRU.crn<- chron(MAL_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(MAL_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(MAL_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(MAL_ABBA.rwi, years_ABBA > 1949 & years_ABBA < 2017)
sub_pcru<-subset(MAL_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL


###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

#plot_outbreak(ef_comp)


###Advanced ggplots
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
bblue3 = rgb(100,150,255, max=255)
borange2 = rgb(255,150,100, max=255)
bblue = rgb(100,175,200, max=255)
bblue2 = rgb(100,175,150, max=255)
borange = rgb(175,100,100, max=255)
bred  = rgb(255,100,100, max=255)
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
  ggtitle("Mallow Lake Full Growth Suppression Index")+
  scale_fill_manual(values=c(bblue3, borange2),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

MAL_rot<-read.csv("rotholz_data/MAL_long.csv")


df<-melt(MAL_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity', position = 'identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###SEW
#Detrend and create a chronology for ABBA
SEW_ABBA.rwi <- detrend(rwl = SEW_ABBA.rwl, method = "ModNegExp")


#Detrend and create a chonology for PCRU
SEW_PCRU.rwi <- detrend(rwl = SEW_PCRU.rwl, method = "ModNegExp")
SEW_PCRU.crn<- chron(SEW_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(SEW_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(SEW_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(SEW_ABBA.rwi, years_ABBA > 1949 & years_ABBA < 2017)
sub_pcru<-subset(SEW_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL


###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

#plot_outbreak(ef_comp)


###Advanced ggplots
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
bblue3 = rgb(100,150,255, max=255)
borange2 = rgb(255,150,100, max=255)
bblue = rgb(100,175,200, max=255)
bblue2 = rgb(100,175,150, max=255)
borange = rgb(175,100,100, max=255)
bred  = rgb(255,100,100, max=255)
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
  ggtitle("CV Sewer Plant Full Growth Suppression Index")+
  scale_fill_manual(values=c(bblue3, borange2),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

SEW_rot<-read.csv("rotholz_data/SEW_long.csv")


df<-melt(SEW_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity', position = 'identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###UPT
#Detrend and create a chronology for ABBA
UPT_ABBA.rwi <- detrend(rwl = UPT_ABBA.rwl, method = "ModNegExp")


#Detrend and create a chonology for PCRU
UPT_PCRU.rwi <- detrend(rwl = UPT_PCRU.rwl, method = "ModNegExp")
UPT_PCRU.crn<- chron(UPT_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(UPT_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(UPT_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(UPT_ABBA.rwi, years_ABBA > 1949 & years_ABBA < 2017)
sub_pcru<-subset(UPT_PCRU.crn, years_PCRU > 1949 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL


###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

#plot_outbreak(ef_comp)


###Advanced ggplots
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)
bblue3 = rgb(100,150,255, max=255)
borange2 = rgb(255,150,100, max=255)
bblue = rgb(100,175,200, max=255)
bblue2 = rgb(100,175,150, max=255)
borange = rgb(175,100,100, max=255)
bred  = rgb(255,100,100, max=255)
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
  ggtitle("Upper Tract Full Growth Suppression Index")+
  scale_fill_manual(values=c(bblue3, borange2),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

UPT_rot<-read.csv("rotholz_data/UPT_long.csv")


df<-melt(UPT_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity', position = 'identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

