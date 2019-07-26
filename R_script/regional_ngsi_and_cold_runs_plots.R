###Script that creates a dual regional plot with GSI on top, and number
##of cold periods (5 consecutive negative degree days) per year (1980-2016)
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)
library(tidyverse)
library(dfoliatR)
library(suRge)
library(lubridate)
library(grid)

###Plot 1: Regional (CV North/Poca-Rand South) growth suppression index plots
###Canaan Region (BFT,COR,DRT,FRE,MAL,MVT,RMR,SEW,VFD,DOL)
######################################################
temp = list.files(pattern="*.rwl", path = "Data/", full.names = TRUE)
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

bblue3 = rgb(100,150,255, max=255)
borange2 = rgb(255,150,100, max=255)
bblue = rgb(100,175,200, max=255)
bblue2 = rgb(100,175,150, max=255)
borange = rgb(175,100,100, max=255)
bred  = rgb(255,100,100, max=255)
cred  = rgb(200,125,125, max=255)
#ABBA
cv_region<-c("BFT","COR","DRT","FRE","MAL","MVT","RMR","SEW","VFD","DOL")
cv_region_abba<-combine.rwl(list(`Data/BFT_ABBA.rwl`,`Data/COR_ABBA.rwl`,
                                 `Data/DOL_ABBA.rwl`,`Data/DRT_ABBA.rwl`,
                                 `Data/FRE_ABBA.rwl`,`Data/MAL_ABBA.rwl`,
                                 `Data/MVT_ABBA.rwl`,`Data/RMR_ABBA.rwl`,
                                 `Data/SEW_ABBA.rwl`,`Data/UPT_ABBA.rwl`,
                                 `Data/VFD_ABBA.rwl`))

#Detrend and create a chronology for the ABBA files in the Canaan Region
cv_abba.rwi <- detrend(rwl = cv_region_abba, method = "ModNegExp")
cv_abba.crn<- chron(cv_abba.rwi, prefix = "CVR")

#Used for ABBA Chronology (We just need the ABBA index/.rwi for this plot)
#sub_cv_abba<-subset(cv_abba.crn, years_ABBA > 1931 & years_ABBA < 2018)

#cv_abba_crn.txt<-write.table(sub_cv_abba, file = "/Users/aleef/Desktop/cv_abba_crn.txt", row.names = T, col.names = T)

###crn.plot(sub_cv_abba)

#PCRU
cv_region_pcru<-combine.rwl(list(`Data/BFT_PCRU.rwl`,`Data/COR_PCRU.rwl`,
                                 `Data/DOL_PCRU.rwl`,`Data/DRT_PCRU.rwl`,
                                 `Data/FRE_PCRU.rwl`,`Data/MAL_PCRU.rwl`,
                                 `Data/MVT_PCRU.rwl`,`Data/RMR_PCRU.rwl`,
                                 `Data/SEW_PCRU.rwl`,`Data/UPT_PCRU.rwl`,
                                 `Data/VFD_PCRU.rwl`))

#Detrend and create a chronology for the PCRU files in the Canaan Region
cv_pcru.rwi <- detrend(rwl = cv_region_pcru, method = "ModNegExp")
cv_pcru.crn<- chron(cv_pcru.rwi, prefix = "CVR")

years_ABBA<-as.numeric(rownames(cv_abba.rwi))
years_PCRU<-as.numeric(rownames(cv_pcru.crn))

sub_cv_abba<-subset(cv_abba.rwi, years_ABBA > 1980 & years_ABBA < 2017)
sub_cv_pcru<-subset(cv_pcru.crn, years_PCRU > 1980 & years_PCRU < 2017)
sub_cv_year_abba<-as.numeric(rownames(sub_cv_abba))
sub_cv_year_pcru<-as.numeric(rownames(sub_cv_pcru))

###Remove sample depth (Needed for nonhost crn in suRge)
#sub_abba$samp.depth<-NULL
sub_cv_pcru$samp.depth<-NULL

cv_ef_defol <- defoliate_trees(host_tree = sub_cv_abba, nonhost_chron = sub_cv_pcru, duration_years = 4, list_output = FALSE)
cv_ef_comp <- outbreak(cv_ef_defol, filter_perc = 25,filter_min_series = 3)

#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- cv_ef_comp[! is.na(cv_ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = cv_ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source

#ggplot2 plots
cv_gsi_p <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI", limits = c(-1.5,1.5)) +
  scale_x_discrete(limits = c(1981, 2016)) +
  ggtitle("Canaan Valley Regional nGSI") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

###dfoliatR plotting for GSI/NGSI
#plot_outbreak(ef_comp)


###Plot 2: Number of annual 5-day periods with negative mean degree days
##(Degrees Celcius)
source('Functions/read.prism.R')

pgrid.list <- read.prism("cv","daily")

#Select just the mean of the dataframes to work with
pgrid <- pgrid.list$pgrid.df

thresh <- 0
pgrid$thresh <- pgrid$tmax < thresh

pgrid$year <- year(pgrid$pdate)
pgrid$month <- month(pgrid$pdate)
pgrid$day <- day(pgrid$pdate)

#Adjust Nov and Dec to be subsequent year
prev.win <- c(11, 12)
adj.year <- ifelse (pgrid$month %in% prev.win, pgrid$year+1, pgrid$year)
pgrid$year <- adj.year

thresh.event <- pgrid[pgrid$thresh %in% 'TRUE', -c(1:5)] 

#Count number of days tmax < thresh
thresh.count <- aggregate(thresh.event, by=list(thresh.event$year), sum)[,1:2]
names(thresh.count) <- c("Year", "Count")
thresh.count <- thresh.count[-38,]

runs <- rle(pgrid$thresh)

pgrid$runs <- rep(runs$lengths, runs$lengths) #add a new vector that tells the lengths and positions of the runs

#give each run a unique identifier to aggregate on
events <- length(runs$values)
pgrid$event.id <- rep(1:events, c(runs$lengths))

long.event <- pgrid[pgrid$thresh %in% 'TRUE' & pgrid$runs >= 5, -c(1:6)] 

#start date and length of each run below threshold
long.st.date <- aggregate(long.event, by=list(long.event$event.id), min)

num.long.yr <- aggregate(long.st.date, by=list(long.st.date$year), length)[, c(1, 6)]

sub_num.long.yr<-subset(num.long.yr, num.long.yr$Group.1 > 1980 & num.long.yr$Group.1 < 2017)

# Plot
cv_clim_p<-ggplot(sub_num.long.yr, aes(x=Group.1, y=runs)) + 
  geom_point(size=2) + 
  geom_segment(aes(x=Group.1, 
                   xend=Group.1, 
                   y=0, 
                   yend=runs)) + 
  labs(x="Year",
       y="Number of Cold Periods") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  scale_x_discrete (limits=c(1981:2016))
  

#Final Plot Arrange
grid.newpage()
grid.draw(rbind(ggplotGrob(cv_gsi_p), ggplotGrob(cv_clim_p), size = "last"))

#ggarrange(cv_gsi_p, cv_clim_p, ncol=1, nrow=2)

######################################################
###Poca/Rand Region (BDR,BRS,MSH)
######################################################
temp = list.files(pattern="*.rwl", path = "Data/", full.names = TRUE)
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))


#ABBA
poca_region_abba<-combine.rwl(list(`Data/BDR_ABBA.rwl`,`Data/BRS_ABBA.rwl`,
                                 `Data/MSH_ABBA.rwl`))

#Detrend and create a chronology for the ABBA files in the Poca Rand Region
poca_abba.rwi <- detrend(rwl = poca_region_abba, method = "ModNegExp")
poca_abba.crn<- chron(poca_abba.rwi, prefix = "POC")

#Used for ABBA Chronology (We just need the ABBA index/.rwi for this plot)
#sub_poca_abba<-subset(poca_abba.crn, years_ABBA > 1931 & years_ABBA < 2018)

#poca_abba_crn.txt<-write.table(sub_poca_abba, file = "/Users/aleef/Desktop/poca_abba_crn.txt", row.names = T, col.names = T)

###crn.plot(sub_poca_abba)

#PCRU
poca_region_pcru<-combine.rwl(list(`Data/BDR_PCRU.rwl`,`Data/BRS_PCRU.rwl`,
                                 `Data/MSH_PCRU.rwl`))

#Detrend and create a chronology for the PCRU files in the Poca Rand Region
poca_pcru.rwi <- detrend(rwl = poca_region_pcru, method = "ModNegExp")
poca_pcru.crn<- chron(poca_pcru.rwi, prefix = "POC")

years_ABBA<-as.numeric(rownames(poca_abba.rwi))
years_PCRU<-as.numeric(rownames(poca_pcru.crn))

sub_poca_abba<-subset(poca_abba.rwi, years_ABBA > 1980 & years_ABBA < 2017)
sub_poca_pcru<-subset(poca_pcru.crn, years_PCRU > 1980 & years_PCRU < 2017)
sub_poca_year_abba<-as.numeric(rownames(sub_poca_abba))
sub_poca_year_pcru<-as.numeric(rownames(sub_poca_pcru))

###Remove sample depth (Needed for nonhost crn in suRge)
#sub_abba$samp.depth<-NULL
sub_poca_pcru$samp.depth<-NULL

poca_ef_defol <- defoliate_trees(host_tree = sub_poca_abba, nonhost_chron = sub_poca_pcru, duration_years = 4, list_output = FALSE)
poca_ef_comp <- outbreak(poca_ef_defol, filter_perc = 25,filter_min_series = 3)

#ggplot2 plots
#Preformatting
disp_index = "mean_ngsi"
col_defol = cred
y_intercept <- 0

outbrk_events <- poca_ef_comp[! is.na(poca_ef_comp$outbreak_status), ]
#Draw axis
p <- ggplot(data = poca_ef_comp, aes_string(x="year"))
#Extract other axis
foo <- p + geom_line(aes_string(y=disp_index))
minor_labs <- ggplot_build(foo)$layout$panel_params[[1]]$x.minor_source


poca_gsi_p <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI", limits = c(-1.5,1.5)) +
  scale_x_discrete(limits = c(1981, 2016)) +
  ggtitle("Poca/Rand Regional nGSI") +
  #theme_pubr() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

###dfoliatR plotting for GSI/NGSI
#plot_outbreak(poca_ef_comp)


###Plot 2: Number of annual 5-day periods with negative mean degree days
##(Degrees Celcius)
source('Functions/read.prism.R')

pgrid.list <- read.prism("rand","daily")

#Select just the mean of the dataframes to work with
pgrid <- pgrid.list$pgrid.df

thresh <- 0
pgrid$thresh <- pgrid$tmax < thresh

pgrid$year <- year(pgrid$pdate)
pgrid$month <- month(pgrid$pdate)
pgrid$day <- day(pgrid$pdate)

#Adjust Nov and Dec to be subsequent year
prev.win <- c(11, 12)
adj.year <- ifelse (pgrid$month %in% prev.win, pgrid$year+1, pgrid$year)
pgrid$year <- adj.year

thresh.event <- pgrid[pgrid$thresh %in% 'TRUE', -c(1:5)] 

#Count number of days tmax < thresh
thresh.count <- aggregate(thresh.event, by=list(thresh.event$year), sum)[,1:2]
names(thresh.count) <- c("Year", "Count")
thresh.count <- thresh.count[-38,]

runs <- rle(pgrid$thresh)

pgrid$runs <- rep(runs$lengths, runs$lengths) #add a new vector that tells the lengths and positions of the runs

#give each run a unique identifier to aggregate on
events <- length(runs$values)
pgrid$event.id <- rep(1:events, c(runs$lengths))

long.event <- pgrid[pgrid$thresh %in% 'TRUE' & pgrid$runs >= 5, -c(1:6)] 

#start date and length of each run below threshold
long.st.date <- aggregate(long.event, by=list(long.event$event.id), min)

num.long.yr <- aggregate(long.st.date, by=list(long.st.date$year), length)[, c(1, 6)]

sub_num.long.yr<-subset(num.long.yr, num.long.yr$Group.1 > 1980 & num.long.yr$Group.1 < 2017)

# Plot
poca_clim_p<-ggplot(sub_num.long.yr, aes(x=Group.1, y=runs)) + 
  geom_point(size=2) + 
  geom_segment(aes(x=Group.1, 
                   xend=Group.1, 
                   y=0, 
                   yend=runs)) + 
  labs(x="Year",
       y="Number of Cold Periods") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  scale_x_discrete (limits=c(1981:2016))
  


#Final Plot Arrange
grid.newpage()
grid.draw(rbind(ggplotGrob(poca_gsi_p), ggplotGrob(poca_clim_p), size = "last"))

#ggarrange(poca_gsi_p, poca_clim_p, ncol=1, nrow=2)

