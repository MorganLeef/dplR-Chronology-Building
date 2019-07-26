###New Corrected chronology code (using dfoliatR and suRge) - Triple - Methods Walkthrogh Plot
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)
library(dfoliatR)
library(suRge)
library(grid)


temp = list.files(pattern="*.rwl")
for (i in 1:length(temp)) assign(temp[i], read.rwl(temp[i]))

#Detrend and create a chronology for ABBA
BDR_ABBA.rwi <- detrend(rwl = BDR_ABBA.rwl, method = "ModNegExp")
BDR_ABBA.crn<- chron(BDR_ABBA.rwi, prefix = "BDR")

#Detrend and create a chonology for PCRU
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
ef_defol <- defoliate_trees(sub_abba_rwi, sub_pcru, duration_years = 4)

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
#P1 host/non-host mean chronologies 
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
  theme(legend.position=c(.90,.05),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylim(-1.5,2.5)

#P2 Host and corrected non-host chronologies
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
  scale_linetype_manual(name="Index",values=c(Host="solid", Non_Host="dashed"), guide = FALSE)+
  ggtitle("Host and Adjusted Non-host Indices")+
  theme(legend.position=c(.95,-.1),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylim(-1.5,2.5)



#P3 Growth Suppression Index (GSI) calculation (Host - corrected non-host)
df<-data.frame(Year=Year, Mean=ifelse(c(RWI)>1,"<Mean",">Mean"), RWI=RWI) 
interp <- approx(df$Year, df$RWI, n=10000)

#dfi <- data.frame(Year=interp$x, RWI=interp$y)
#dfi$Mean[dfi$RWI < 1] <- "<Mean"
#dfi$Mean[dfi$RWI > 1] <- ">Mean"

p3<-ggplot(data=df, aes(x=Year, y=RWI))+
  geom_line()+
  geom_hline(yintercept = 1, colour = "grey80")+
  scale_y_continuous(trans=t_shift)+
  ylab("GSI")+
  ggtitle("BDR - Growth Suppression Index (GSI)")+
  theme(legend.position=c(.95,-.05),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#P4 nGSI plot (normalized Growth Suppression Index) with outbreaks highlighted (> 25% defoliation)
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
p4 <- p +
  geom_vline(xintercept = minor_labs, colour="grey50") +
  geom_hline(yintercept = y_intercept, colour = "grey80") +
  geom_line(aes_string(y=disp_index)) +
  geom_segment(data = outbrk_events,
               aes_string(x="year", xend="year",
                          y=y_intercept, yend=disp_index),
               size=2, colour = 'red') +
  scale_y_continuous(name = "nGSI") +
  scale_x_continuous(limits = c(1980, 2016)) +
  ggtitle("BDR nGSI/Percent Rotholz/Percent Defoliated")
  #theme_pubr() +
  #theme(axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), ggplotGrob(p4), size = "last"))

