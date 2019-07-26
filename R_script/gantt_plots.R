###Gantt Plotting for site by site infestation severity. Gantt plots are 
##plotted manually through the same method used in dfoliatR.
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

###BDR
#Detrend and create a chronology for BDR ABBA
BDR_ABBA.rwi <- detrend(rwl = BDR_ABBA.rwl, method = "ModNegExp")
###BDR_ABBA.crn<- chron(BDR_ABBA.rwi, prefix = "BDR")

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

###New method of calculating corrected chronologies using dfoliatR/suRge

###Series 25A registered no infestation. Removing for gannt plot
#sub_abba$BDR01CCB<-NULL
sub_abba$BDR04BBB<-NULL
sub_abba$BDR06AAB<-NULL
#sub_abba$BDR07A<-NULL
#sub_abba$BDR08A<-NULL
#sub_abba$BDR08B<-NULL
#sub_abba$BDR09A<-NULL
#sub_abba$BDR10A<-NULL
#sub_abba$BDR12A<-NULL
#sub_abba$BDR13A<-NULL
#sub_abba$BDR14B<-NULL
##Function subtracts values from non-host .crn from values in host index
ef_defol_BDR <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_BDR)
e.stats <- get_defol_events(ef_defol_BDR)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))
                        


ggplot(ef_defol_BDR, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                                   xend = "last",
                                                   y = "series",
                                                   yend = "series"),
                               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                                   xend = "end_year",
                                                   y = "series",
                                                   yend = "series",
                                                   colour = "Severity"),
                               linetype = 'solid',
                               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(),
                 legend.position = "bottom")+
  ggtitle("BDR Outbreak History")
###BFT
#Detrend and create a chronology for BDR ABBA
BFT_ABBA.rwi <- detrend(rwl = BFT_ABBA.rwl, method = "ModNegExp")
###BFT_ABBA.crn<- chron(SEW_ABBA.rwi, prefix = "BFT")

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

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series 25A registered no infestation. Removing for gannt plot
#sub_abba$BDR01CCB<-NULL
sub_abba$BFT05BBB<-NULL
sub_abba$BFT11BBB<-NULL
#sub_abba$BFT15BBB<-NULL
#sub_abba$BFT19AAB<-NULL
#sub_abba$BFT19BBB<-NULL
#sub_abba$BFT24AAB<-NULL
sub_abba$BFT25AAB<-NULL
sub_abba$BFT25BBB<-NULL

##Function subtracts values from non-host .crn from values in host index
ef_defol_BFT <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_BFT)
e.stats <- get_defol_events(ef_defol_BFT)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_BFT, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("BFT Outbreak History")

###BRS
#Detrend and create a chronology for BRS ABBA
BRS_ABBA.rwi <- detrend(rwl = BRS_ABBA.rwl, method = "ModNegExp")

#Detrend and create a chonology for BFT PCRU
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

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series registered no infestation. Removing for gannt plot

sub_abba$BRS02BBB<-NULL
sub_abba$BRS04AAB<-NULL
sub_abba$BRS06AAB<-NULL

##Function subtracts values from non-host .crn from values in host index
ef_defol_BRS <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_BRS)
e.stats <- get_defol_events(ef_defol_BRS)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_BRS, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("BRS Outbreak History")

###COR
#Detrend and create a chronology for ABBA
COR_ABBA.rwi <- detrend(rwl = COR_ABBA.rwl, method = "ModNegExp")

#Detrend and create a chonology for PCRU
COR_PCRU.rwi <- detrend(rwl = COR_PCRU.rwl, method = "ModNegExp")
COR_PCRU.crn<- chron(BRS_PCRU.rwi, prefix = "BDP")

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

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series registered no infestation. Removing for gannt plot

sub_abba$COR01AAC<-NULL
sub_abba$COR01BBC<-NULL
sub_abba$COR02AAC<-NULL
sub_abba$COR02BBC<-NULL
sub_abba$COR05AAC<-NULL
sub_abba$COR08BBC<-NULL
sub_abba$COR09AAC<-NULL
sub_abba$COR10AAC<-NULL
sub_abba$COR12AAC<-NULL
sub_abba$COR15AAC<-NULL
sub_abba$COR19BBC<-NULL
sub_abba$COR21BBC<-NULL
sub_abba$COR30AAU<-NULL


##Function subtracts values from non-host .crn from values in host index
ef_defol_COR <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_COR)
e.stats <- get_defol_events(ef_defol_COR)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_COR, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("COR Outbreak History")

###DOL
#Detrend and create a chronology for ABBA
DOL_ABBA.rwi <- detrend(rwl = DOL_ABBA.rwl, method = "ModNegExp")

#Detrend and create a chonology for PCRU
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

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series registered no infestation. Removing for gannt plot

sub_abba$DOL01AAD<-NULL
sub_abba$DOL01BBD<-NULL
sub_abba$DOL02BBC<-NULL
sub_abba$DOL03BBD<-NULL
sub_abba$DOL04BBD<-NULL
sub_abba$DOL08AAD<-NULL
sub_abba$DOL10AAD<-NULL
sub_abba$DOL10BBD<-NULL
sub_abba$DOL11BBD<-NULL


##Function subtracts values from non-host .crn from values in host index
ef_defol_DOL <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_DOL)
e.stats <- get_defol_events(ef_defol_DOL)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_DOL, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("DOL Outbreak History")

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
sub_abba<-subset(DRT_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(DRT_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series registered no infestation. Removing for gannt plot

sub_abba$DRT01AAD<-NULL
sub_abba$DRT04BBD<-NULL
sub_abba$DRT06AAD<-NULL
sub_abba$DRT06BBD<-NULL
sub_abba$DRT09BBD<-NULL
sub_abba$DRT13AAD<-NULL
sub_abba$DRT13BBD<-NULL
sub_abba$DRT18AAD<-NULL
sub_abba$DRT21BBD<-NULL

##Function subtracts values from non-host .crn from values in host index
ef_defol_DRT <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_DRT)
e.stats <- get_defol_events(ef_defol_DRT)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_DRT, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("DRT Outbreak History")

###FRE
#Detrend and create a chronology for ABBA
FRE_ABBA.rwi <- detrend(rwl = FRE_ABBA.rwl, method = "ModNegExp")

#Detrend and create a chonology for PCRU
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

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series registered no infestation. Removing for gannt plot
sub_abba$FRE02BBF<-NULL
sub_abba$FRE07BBF<-NULL
sub_abba$FRE06AAF<-NULL
sub_abba$FRE06BBF<-NULL
sub_abba$FRE14BBF<-NULL
sub_abba$FRE19BBF<-NULL
sub_abba$FRE21BBF<-NULL
sub_abba$FRE22BBF<-NULL
sub_abba$FRE63BBF<-NULL
sub_abba$FRE60AAF<-NULL
sub_abba$FRE60BBF<-NULL
sub_abba$FRE65AAF<-NULL
sub_abba$FRE65BBF<-NULL
sub_abba$FRE06AAF<-NULL
sub_abba$FRE06BBF<-NULL
sub_abba$FRE12AAF<-NULL
sub_abba$FRE12BBF<-NULL
sub_abba$FRE15AAF<-NULL
sub_abba$FRE15BBF<-NULL
sub_abba$FRE26AAF<-NULL
sub_abba$FRE26BBF<-NULL
sub_abba$FRE28AAF<-NULL
sub_abba$FRE28BBF<-NULL
sub_abba$FRE16AAF<-NULL
sub_abba$FRE16BBF<-NULL
sub_abba$FRE18AAF<-NULL
sub_abba$FRE18BBF<-NULL
sub_abba$FRE22AAF<-NULL
sub_abba$FRE30AAF<-NULL
sub_abba$FRE30BBF<-NULL
##Function subtracts values from non-host .crn from values in host index
ef_defol_FRE <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_FRE)
e.stats <- get_defol_events(ef_defol_FRE)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_FRE, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("FRE Outbreak History")

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
sub_abba<-subset(MAL_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(MAL_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series registered no infestation. Removing for gannt plot
sub_abba$MAL05BBM<-NULL
sub_abba$MAL06AAM<-NULL
sub_abba$MAL09AAM<-NULL
sub_abba$MAL10BBM<-NULL
sub_abba$MAL23BBM<-NULL
sub_abba$MAL24BBM<-NULL
sub_abba$MAL39AAM<-NULL
sub_abba$MAL51BBM<-NULL


##Function subtracts values from non-host .crn from values in host index
ef_defol_MAL <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_MAL)
e.stats <- get_defol_events(ef_defol_MAL)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_MAL, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("MAL Outbreak History")

###MSH
#Detrend and create a chronology for ABBA
MSH_ABBA.rwi <- detrend(rwl = MSH_ABBA.rwl, method = "ModNegExp")

#Detrend and create a chonology for PCRU
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

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series registered no infestation. Removing for gannt plot
sub_abba$MSH02B<-NULL
sub_abba$MSH03A<-NULL
sub_abba$MSH06A<-NULL
sub_abba$MSH06B<-NULL
sub_abba$MSH07B<-NULL
sub_abba$MSH11B<-NULL
sub_abba$MSH15B<-NULL
sub_abba$MSH18B<-NULL
sub_abba$MSH21B<-NULL
sub_abba$MSH24A<-NULL
sub_abba$MSH26A<-NULL




##Function subtracts values from non-host .crn from values in host index
ef_defol_MSH <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_MSH)
e.stats <- get_defol_events(ef_defol_MSH)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_MSH, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("MSH Outbreak History")

###MVT
#Detrend and create a chronology for ABBA
MVT_ABBA.rwi <- detrend(rwl = MVT_ABBA.rwl, method = "ModNegExp")

#Detrend and create a chonology for PCRU
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

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series registered no infestation. Removing for gannt plot
sub_abba$MVT02aaM<-NULL
sub_abba$MVT02bbM<-NULL
sub_abba$MVT03AAM<-NULL
sub_abba$MVT03BBM<-NULL
sub_abba$MVT03CCM<-NULL
sub_abba$MVT04BBM<-NULL
sub_abba$MVT07BBM<-NULL
sub_abba$MVT15AAM<-NULL
sub_abba$MVT20BBM<-NULL





##Function subtracts values from non-host .crn from values in host index
ef_defol_MVT <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_MVT)
e.stats <- get_defol_events(ef_defol_MVT)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_MVT, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("MVT Outbreak History")

###RMR
#Detrend and create a chronology for ABBA
RMR_ABBA.rwi <- detrend(rwl = RMR_ABBA.rwl, method = "ModNegExp")

#Detrend and create a chonology for PCRU
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

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series registered no infestation. Removing for gannt plot
sub_abba$RMR14AAR<-NULL
sub_abba$RMR15AAR<-NULL
sub_abba$RMR26AAR<-NULL
sub_abba$RMR27AAR<-NULL
sub_abba$RMR27BBR<-NULL


##Function subtracts values from non-host .crn from values in host index
ef_defol_RMR <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_RMR)
e.stats <- get_defol_events(ef_defol_RMR)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_RMR, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("RMR Outbreak History")

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
sub_abba<-subset(SEW_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(SEW_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series registered no infestation. Removing for gannt plot
sub_abba$SEW10AAS<-NULL
sub_abba$SEW13BBS<-NULL
sub_abba$SEW18BBS<-NULL
sub_abba$SEW32BBS<-NULL
sub_abba$SEW44AAS<-NULL
sub_abba$SEW45BBS<-NULL

##Function subtracts values from non-host .crn from values in host index
ef_defol_SEW <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_SEW)
e.stats <- get_defol_events(ef_defol_SEW)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_SEW, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("SEW Outbreak History")

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
sub_abba<-subset(UPT_ABBA.rwi, years_ABBA > 1979 & years_ABBA < 2017)
sub_pcru<-subset(UPT_PCRU.crn, years_PCRU > 1979 & years_PCRU < 2017)
sub_year_abba<-as.numeric(rownames(sub_abba))
sub_year_pcru<-as.numeric(rownames(sub_pcru))

#Remove samp.depth column
sub_abba$samp.depth<-NULL
sub_pcru$samp.depth<-NULL

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series registered no infestation. Removing for gannt plot
sub_abba$UPT01BBU<-NULL
sub_abba$UPT05AAU<-NULL
sub_abba$UPT07BBU<-NULL
sub_abba$UPT09AAU<-NULL
sub_abba$UPT11AAU<-NULL
sub_abba$UPT16AAU<-NULL
sub_abba$UPT27AAU<-NULL
sub_abba$UPT27BBU<-NULL



##Function subtracts values from non-host .crn from values in host index
ef_defol_UPT <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_UPT)
e.stats <- get_defol_events(ef_defol_UPT)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_UPT, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("UPT Outbreak History")
  

###VFD
#Detrend and create a chronology for ABBA
VFD_ABBA.rwi <- detrend(rwl = VFD_ABBA.rwl, method = "ModNegExp")

#Detrend and create a chonology for PCRU
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

###New method of calculating corrected chronologies using dfoliatR/suRge
#Remove samples that don't register infestations
###Series registered no infestation. Removing for gannt plot
sub_abba$VFD01AAV<-NULL
sub_abba$VFD06AAV<-NULL
sub_abba$VFD08BBV<-NULL
sub_abba$VFD10AAV<-NULL
sub_abba$VFD10BBV<-NULL
sub_abba$VFD11AAV<-NULL
sub_abba$VFD14AAV<-NULL
sub_abba$VFD14BBV<-NULL
sub_abba$VFD17BBV<-NULL
sub_abba$VFD18AAV<-NULL
sub_abba$VFD18BBV<-NULL
sub_abba$VFD19AAV<-NULL
sub_abba$VFD19BBV<-NULL
sub_abba$VFD22BBV<-NULL
sub_abba$VFD23AAV<-NULL
sub_abba$VFD23BBV<-NULL
sub_abba$VFD25AAV<-NULL
sub_abba$VFD27BBV<-NULL
sub_abba$VFD28AAV<-NULL
sub_abba$VFD60AAV<-NULL
sub_abba$VFD64AAV<-NULL
sub_abba$VFD66AAV<-NULL
sub_abba$VFD72BBV<-NULL


##Function subtracts values from non-host .crn from values in host index
ef_defol_VFD <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 6, list_output = FALSE)


###Sample-Wise 
s.stats <- defol_stats(ef_defol_VFD)
e.stats <- get_defol_events(ef_defol_VFD)



e.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, 0.75, 1.00, Inf),
                        right = FALSE,
                        labels = c("Minor", "Moderate", "Severe"))



ggplot(ef_defol_VFD, aes_string(x="year", y="series"))+
  geom_segment(data = s.stats,aes_string(x = "first",
                                         xend = "last",
                                         y = "series",
                                         yend = "series"),
               linetype = 'dotted')+
  geom_segment(data = e.stats,aes_string(x = "start_year",
                                         xend = "end_year",
                                         y = "series",
                                         yend = "series",
                                         colour = "Severity"),
               linetype = 'solid',
               size=1.25)+
  scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  theme_bw() +
  theme(panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom")+
  ggtitle("VFD Outbreak History")


###Site-Wise
###BDR
#BDR_stats<-data.frame(Year=ef_defol_BDR$year,Series=ef_defol_BDR$series, NGSI=ef_defol_BDR$ngsi)
#BDR_sort <- BDR_stats[order(BDR_stats$Year),]
#BDR_mean<-aggregate(BDR_sort[, 3], list(BDR_sort$Year), mean)
#BDR_final<-data.frame(Year=BDR_mean$Group.1, series="BDR", ngsi=BDR_mean$x)

###BFT
#BFT_stats<-data.frame(Year=ef_defol_BFT$year,Series=ef_defol_BFT$series, NGSI=ef_defol_BFT$ngsi)
#BFT_sort <- BFT_stats[order(BFT_stats$Year),]
#BFT_mean<-aggregate(BFT_sort[, 3], list(BFT_sort$Year), mean)
#BFT_final<-data.frame(Year=BFT_mean$Group.1, series="BFT", ngsi=BFT_mean$x)
#sites<-rbind(BDR_final,BFT_final)

#ggplot(BDR_final, aes_string(x="year", y="series"))+
  #geom_segment(data = e.stats,aes_string(x = "start_year",
                                         #xend = "end_year",
                                         #y = "series",
                                         #yend = "series",
                                         #colour = "Severity"),
               #linetype = 'solid',
               #size=1.25)+
  #scale_color_manual(values = c("#00BFC4","#00BA38","#F8766D"))+
  #theme_bw() +
  #theme(panel.grid.major.y = ggplot2::element_blank(),
        #panel.grid.minor.y = ggplot2::element_blank(),
        #axis.title.x = ggplot2::element_blank(),
        #axis.title.y = ggplot2::element_blank(),
        #legend.title = ggplot2::element_blank(),
        #legend.position = "bottom")+
  #ggtitle("VFD Outbreak History")




