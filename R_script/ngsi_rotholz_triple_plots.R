###New Corrected chronology code (using dfoliatR and suRge)

rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)
library(tidyverse)
library(dfoliatR)
library(suRge)
library(grid)

#Import Raw Series 
temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Site by site plotting
###BDR
cred  = rgb(200,125,125, max=255)
#Detrend and create a chronology for ABBA
BDR_ABBA.rwi <- detrend(rwl = BDR_ABBA.rwl, method = "ModNegExp")

#Detrend and create a chonology for PCRU
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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
BDR_rot<-read.csv("rotholz_data/BDR.csv")
BDR_df<-melt(BDR_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("BDR nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = BDR_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))
  
 
#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

#######
##BFT##
#######

#Detrend and create a chronology for ABBA
BFT_ABBA.rwi <- detrend(rwl = BFT_ABBA.rwl, method = "ModNegExp")

#Detrend and create a chonology for PCRU
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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
BFT_rot<-read.csv("rotholz_data/BFT.csv")
BFT_df<-melt(BFT_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("BFT nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = BFT_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))


#######
##BRS##
#######

#Detrend and create a chronology for ABBA
BRS_ABBA.rwi <- detrend(rwl = BRS_ABBA.rwl, method = "ModNegExp")

#Detrend and create a chonology for PCRU
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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
BRS_rot<-read.csv("rotholz_data/BRS.csv")
BRS_df<-melt(BRS_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("BRS nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = BRS_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

#######
##COR##
#######

#Detrend and create a chronology for ABBA
COR_ABBA.rwi <- detrend(rwl = COR_ABBA.rwl, method = "ModNegExp")

#Detrend and create a chonology for PCRU
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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
COR_rot<-read.csv("rotholz_data/COR.csv")
COR_df<-melt(COR_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("COR nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = COR_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

#######
##DOL##
#######

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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
DOL_rot<-read.csv("rotholz_data/DOL.csv")
DOL_df<-melt(DOL_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("DOL nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = DOL_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

#######
##DRT##
#######

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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
DRT_rot<-read.csv("rotholz_data/DRT.csv")
DRT_df<-melt(DRT_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("DRT nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = DRT_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

#######
##FRE##
#######

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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
FRE_rot<-read.csv("rotholz_data/FRE.csv")
FRE_df<-melt(FRE_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("FRE nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = FRE_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

#######
##MAL##
#######

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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
MAL_rot<-read.csv("rotholz_data/MAL.csv")
MAL_df<-melt(MAL_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("MAL nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = MAL_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

#######
##MSH##
#######

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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
MSH_rot<-read.csv("rotholz_data/MSH.csv")
MSH_df<-melt(MSH_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("MSH nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = MSH_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

#######
##MVT##
#######

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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
MVT_rot<-read.csv("rotholz_data/MVT.csv")
MVT_df<-melt(MVT_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("MVT nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = MVT_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

#######
##RMR##
#######

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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
RMR_rot<-read.csv("rotholz_data/RMR.csv")
RMR_df<-melt(RMR_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("RMR nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = RMR_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

#######
##SEW##
#######

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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
SEW_rot<-read.csv("rotholz_data/SEW.csv")
SEW_df<-melt(SEW_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("SEW nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = SEW_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

#######
##UPT##
#######

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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
UPT_rot<-read.csv("rotholz_data/UPT.csv")
UPT_df<-melt(UPT_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("UPT nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = UPT_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

#######
##VFD##
#######

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


##Function subtracts values from non-host .crn from values in host index
ef_defol <- defoliate_trees(host_tree = sub_abba, nonhost_chron = sub_pcru, duration_years = 4 ,list_output = FALSE, bridge_events = TRUE)

###Standard plotting of defoliate trees function (high gsi in red)
###plot_defol(ef_defol, col_defol = "red")
###Plots varying intensity infestation events for each series over time
#gantt_plot(ef_defol)

###
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)

###
#plot_outbreak(ef_comp)

###Advanced ggplots
#Load in separate rotholz data
VFD_rot<-read.csv("rotholz_data/VFD.csv")
VFD_df<-melt(VFD_rot,id.vars = "Year")

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- ef_comp[! is.na(ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#Top plot
index <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2017)) +
  ggtitle("VFD nGSI/Percent Rotholz/Percent Defoliated") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



#Middle Rotholz Plot
rot_count <- p +
  geom_bar(data = VFD_df,aes(x = Year, y = value,fill=variable),
           stat='identity', position = 'identity') +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  scale_fill_manual(values=c("gray70", "gray50"), name="Sample Damage", label=c("% Rotholz", "% Resin"), guide = FALSE) +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1980, 2017)) +
  ylab("% Live Samples") +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Bottom % Defoliated Plot
prop <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_ribbon(aes_string(ymax="perc_defol", ymin=0)) +
  scale_y_continuous(name = "% Defoliated") +
  scale_x_continuous(limits = c(1980, 2017))


#plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#ggarrange(index, rot_count, prop, ncol=1, nrow=3, widths=c(1980,2016))
grid.newpage()
grid.draw(rbind(ggplotGrob(index), ggplotGrob(rot_count), ggplotGrob(prop), size = "last"))

