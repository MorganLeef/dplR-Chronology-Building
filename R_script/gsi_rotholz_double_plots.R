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

temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for BDR ABBA
BDR_ABBA.rwi <- detrend(rwl = BDR_ABBA.rwl, method = "ModNegExp")
###SEW_ABBA.crn<- chron(SEW_ABBA.rwi, prefix = "BDR")

#Detrend and create a chonology for BDR PCRU
BDR_PCRU.rwi <- detrend(rwl = BDR_PCRU.rwl, method = "ModNegExp")
BDR_PCRU.crn<- chron(BDR_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(BDR_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(BDR_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(BDR_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(BDR_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Beaver Dam Run Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

BDR_rot<-read.csv("rotholz_data/BDR.csv")

df<-melt(BDR_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###BFT
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
sub_abba<-subset(BFT_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(BFT_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Balsam Fir Trail Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

BFT_rot<-read.csv("rotholz_data/BFT.csv")

df<-melt(BFT_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###BRS
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

#Detrend and create a chronology for BRS ABBA
BRS_ABBA.rwi <- detrend(rwl = BRS_ABBA.rwl, method = "ModNegExp")
###BRS_ABBA.crn<- chron(BRS_ABBA.rwi, prefix = "BRS")

#Detrend and create a chonology for BRS PCRU
BRS_PCRU.rwi <- detrend(rwl = BRS_PCRU.rwl, method = "ModNegExp")
BRS_PCRU.crn<- chron(BRS_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(BRS_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(BRS_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(BRS_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(BRS_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Blister Run Swamp Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

BRS_rot<-read.csv("rotholz_data/BRS.csv")

df<-melt(BRS_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###COR
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

#Detrend and create a chronology for COR ABBA
COR_ABBA.rwi <- detrend(rwl = COR_ABBA.rwl, method = "ModNegExp")
###COR_ABBA.crn<- chron(COR_ABBA.rwi, prefix = "COR")

#Detrend and create a chonology for COR PCRU
COR_PCRU.rwi <- detrend(rwl = COR_PCRU.rwl, method = "ModNegExp")
COR_PCRU.crn<- chron(COR_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(COR_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(COR_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(COR_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(COR_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
#plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)



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
  ggtitle("Courtland Road Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

COR_rot<-read.csv("rotholz_data/COR.csv")

df<-melt(COR_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###DOL
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

#Detrend and create a chronology for DOL ABBA
DOL_ABBA.rwi <- detrend(rwl = DOL_ABBA.rwl, method = "ModNegExp")
###DOL_ABBA.crn<- chron(DOL_ABBA.rwi, prefix = "DOL")

#Detrend and create a chonology for DOL PCRU
DOL_PCRU.rwi <- detrend(rwl = DOL_PCRU.rwl, method = "ModNegExp")
DOL_PCRU.crn<- chron(DOL_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(DOL_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(DOL_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(DOL_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(DOL_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Dolly Sods Wilderness Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

DOL_rot<-read.csv("rotholz_data/DOL.csv")

df<-melt(DOL_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###DRT
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

#Detrend and create a chronology for DRT ABBA
DRT_ABBA.rwi <- detrend(rwl = DRT_ABBA.rwl, method = "ModNegExp")
###DRT_ABBA.crn<- chron(DRT_ABBA.rwi, prefix = "DRT")

#Detrend and create a chonology for DRT PCRU
DRT_PCRU.rwi <- detrend(rwl = DRT_PCRU.rwl, method = "ModNegExp")
DRT_PCRU.crn<- chron(DRT_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(DRT_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(DRT_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(DRT_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(DRT_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Deer Run Trail Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

DRT_rot<-read.csv("rotholz_data/DRT.csv")

df<-melt(DRT_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###FRE
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

#Detrend and create a chronology for FRE ABBA
FRE_ABBA.rwi <- detrend(rwl = FRE_ABBA.rwl, method = "ModNegExp")
###FRE_ABBA.crn<- chron(FRE_ABBA.rwi, prefix = "FRE")

#Detrend and create a chonology for FRE PCRU
FRE_PCRU.rwi <- detrend(rwl = FRE_PCRU.rwl, method = "ModNegExp")
FRE_PCRU.crn<- chron(FRE_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(FRE_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(FRE_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(FRE_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(FRE_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Freeland Road Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

FRE_rot<-read.csv("rotholz_data/FRE.csv")

df<-melt(FRE_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###MAL
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

#Detrend and create a chronology for MAL ABBA
MAL_ABBA.rwi <- detrend(rwl = MAL_ABBA.rwl, method = "ModNegExp")
###MAL_ABBA.crn<- chron(MAL_ABBA.rwi, prefix = "MAL")

#Detrend and create a chonology for MAL PCRU
MAL_PCRU.rwi <- detrend(rwl = MAL_PCRU.rwl, method = "ModNegExp")
MAL_PCRU.crn<- chron(MAL_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(MAL_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(MAL_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(MAL_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(MAL_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Mallow Lake Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

MAL_rot<-read.csv("rotholz_data/MAL.csv")

df<-melt(MAL_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###MSH
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

#Detrend and create a chronology for MSH ABBA
MSH_ABBA.rwi <- detrend(rwl = MSH_ABBA.rwl, method = "ModNegExp")
###MSH_ABBA.crn<- chron(MSH_ABBA.rwi, prefix = "MSH")

#Detrend and create a chonology for MSH PCRU
MSH_PCRU.rwi <- detrend(rwl = MSH_PCRU.rwl, method = "ModNegExp")
MSH_PCRU.crn<- chron(MSH_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(MSH_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(MSH_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(MSH_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(MSH_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Marlington/Scenic Highway Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

MSH_rot<-read.csv("rotholz_data/MSH.csv")

df<-melt(MSH_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###MVT
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

#Detrend and create a chronology for MVT ABBA
MVT_ABBA.rwi <- detrend(rwl = MVT_ABBA.rwl, method = "ModNegExp")
###MVT_ABBA.crn<- chron(MVT_ABBA.rwi, prefix = "MVT")

#Detrend and create a chonology for MVT PCRU
MVT_PCRU.rwi <- detrend(rwl = MVT_PCRU.rwl, method = "ModNegExp")
MVT_PCRU.crn<- chron(MVT_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(MVT_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(MVT_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(MVT_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(MVT_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Middle Valley Trail Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

MVT_rot<-read.csv("rotholz_data/MVT.csv")

df<-melt(MVT_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###RMR
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

#Detrend and create a chronology for RMR ABBA
RMR_ABBA.rwi <- detrend(rwl = RMR_ABBA.rwl, method = "ModNegExp")
###RMR_ABBA.crn<- chron(RMR_ABBA.rwi, prefix = "RMR")

#Detrend and create a chonology for RMR PCRU
RMR_PCRU.rwi <- detrend(rwl = RMR_PCRU.rwl, method = "ModNegExp")
RMR_PCRU.crn<- chron(RMR_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(RMR_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(RMR_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(RMR_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(RMR_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Route 32/Manor Road Int. Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

RMR_rot<-read.csv("rotholz_data/RMR.csv")

df<-melt(RMR_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###SEW
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

#Detrend and create a chronology for SEW ABBA
SEW_ABBA.rwi <- detrend(rwl = SEW_ABBA.rwl, method = "ModNegExp")
###SEW_ABBA.crn<- chron(SEW_ABBA.rwi, prefix = "SEW")

#Detrend and create a chonology for SEW PCRU
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
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))



###UPT
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

#Detrend and create a chronology for UPT ABBA
UPT_ABBA.rwi <- detrend(rwl = UPT_ABBA.rwl, method = "ModNegExp")
###UPT_ABBA.crn<- chron(UPT_ABBA.rwi, prefix = "UPT")

#Detrend and create a chonology for UPT PCRU
UPT_PCRU.rwi <- detrend(rwl = UPT_PCRU.rwl, method = "ModNegExp")
UPT_PCRU.crn<- chron(UPT_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(UPT_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(UPT_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(UPT_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(UPT_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Upper Tract Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

UPT_rot<-read.csv("rotholz_data/UPT.csv")

df<-melt(UPT_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))

###VFD
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

#Detrend and create a chronology for VFD ABBA
VFD_ABBA.rwi <- detrend(rwl = VFD_ABBA.rwl, method = "ModNegExp")
###VFD_ABBA.crn<- chron(VFD_ABBA.rwi, prefix = "VFD")

#Detrend and create a chonology for VFD PCRU
VFD_PCRU.rwi <- detrend(rwl = VFD_PCRU.rwl, method = "ModNegExp")
VFD_PCRU.crn<- chron(VFD_PCRU.rwi, prefix = "BDP")

#Create years object
years_ABBA<-as.numeric(rownames(VFD_ABBA.rwi)) 
years_PCRU<-as.numeric(rownames(VFD_PCRU.crn))

#Subset all data to be between 1980 and 2016
sub_abba<-subset(VFD_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(VFD_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 8, list_output = FALSE)

###
###plot_defol(ef_defol, col_defol = "red")

###
###gantt_plot(ef_defol)

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
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

dfi <- data.frame(Year=interp$x, RWI=interp$y)
dfi$Mean[dfi$RWI < 1] <- "<Mean"
dfi$Mean[dfi$RWI > 1] <- ">Mean"

p1<-ggplot(data=dfi, aes(x=Year, y=RWI))+
  geom_area(aes(fill = Mean))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(trans=t_shift)+
  ggtitle("Volunteer Fire Dept. Growth Suppression Index")+
  scale_fill_manual(values=c(cblue, cred),name="C RWI", label=c("Above Mean","Below Mean"), guide = FALSE)+
  theme(legend.position=c(.05,.1))

VFD_rot<-read.csv("rotholz_data/VFD.csv")

df<-melt(VFD_rot,id.vars = "Year")

p2<-ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat='identity')+
  theme(legend.position=c(.05,.1))+
  scale_fill_manual(values=c("brown2", "steelblue4"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE)+
  scale_y_continuous(limits = c(0, 65))+
  ylab("% of Live Samples")

ggarrange(p1, p2, ncol=1, nrow=2, widths=c(1980,2016))



