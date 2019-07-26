###Script that plots USFS data vs rotholz and GSI data
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)
library(tidyverse)
library(grid)

bblue3 = rgb(100,150,255, max=255)
borange2 = rgb(255,150,100, max=255)
bblue = rgb(100,175,200, max=255)
bblue2 = rgb(100,175,150, max=255)
borange = rgb(175,100,100, max=255)
bred  = rgb(255,100,100, max=255)

###Freeland Road
fre_rec_rot<-read.csv("Data/forest_service_data/FRE_rot_num.csv")
fre_rec_rot$bwa_pop <- factor(fre_rec_rot$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))
fre<-data.frame(bwa_pop=fre_rec_rot$bwa_pop, num_tree=fre_rec_rot$num_tree, rot_num=fre_rec_rot$rot_num)
fre_melt<-melt(fre,id.vars = "bwa_pop")
fre_year<-data.frame(fre_melt$bwa_pop, variable=fre_melt$variable, value=fre_melt$value, Year=fre_rec_rot$Year)


fre_p1<-ggplot(fre_rec_rot, aes(x = Year, y=mean_gsi, colour = bwa_pop))+
  scale_color_manual(values=c("red","blue","white","white"), 
                     label=c("None","Light","",""),
                     name="")+
  geom_point()+
  geom_line()+
  ggtitle("Freeland Road BWA GSI/Rotholz Summary")+
  ylim(-1,1.5)+
  xlim(2013.67,2016.35)+
  ylab("Mean GSI")+
  theme(legend.position="none")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position=c(.94,.83))

 
fre_p2<-ggplot(fre_year, aes(x=fre_melt.bwa_pop, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'identity')+ facet_grid(~ Year)+
  scale_fill_manual(values=c(bblue3, borange2),name="", label=c("# of Trees","# w/ Rotholz"))+
  ylim(c(0,25))+
  ylab("No. of Trees")+
  xlab("BWA Population Size")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position=c(.94,.78))

ggarrange(fre_p1, fre_p2, ncol=1, nrow=2)

#geom_label(mapping=NULL,data=NULL,stat="identity",position_dodge(width = 1), size = 3,show.legend = FALSE)
#scale_fill_manual(values=c(bblue, bblue2, borange, bred),name="BWA Population", label=c("None","Light", "Moderate", "Heavy"))+

###Mallow Lake
mal_rec_rot<-read.csv("Data/forest_service_data/MAL_rot_num.csv")
mal_rec_rot$bwa_pop <- factor(mal_rec_rot$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))
mal<-data.frame(bwa_pop=mal_rec_rot$bwa_pop, num_tree=mal_rec_rot$num_tree, rot_num=mal_rec_rot$rot_num)
mal_melt<-melt(mal,id.vars = "bwa_pop")
mal_year<-data.frame(mal_melt$bwa_pop, variable=mal_melt$variable, value=mal_melt$value, Year=mal_rec_rot$Year)


mal_p1<-ggplot(mal_rec_rot, aes(x = Year, y=mean_gsi, colour = bwa_pop))+
  scale_color_manual(values=c("red","blue","white","white"), 
                     label=c("None","Light","",""),
                     name="")+
  geom_point()+
  geom_line()+
  ggtitle("Mallow Lake BWA GSI/Rotholz Summary")+
  ylim(-1,1.5)+
  xlim(2013.67,2016.35)+
  ylab("Mean GSI")+
  theme(legend.position="none")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position=c(.94,.83))


mal_p2<-ggplot(mal_year, aes(x=mal_melt.bwa_pop, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'identity')+ facet_grid(~ Year)+
  scale_fill_manual(values=c(bblue3, borange2),name="", label=c("# of Trees","# w/ Rotholz"))+
  ylim(c(0,25))+
  ylab("No. of Trees")+
  xlab("BWA Population Size")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position=c(.94,.78))

ggarrange(mal_p1, mal_p2, ncol=1, nrow=2)

###Middle Valley Trail
mvt_rec_rot<-read.csv("Data/forest_service_data/MVT_rot_num.csv")
mvt_rec_rot$bwa_pop <- factor(mvt_rec_rot$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))
mvt<-data.frame(bwa_pop=mvt_rec_rot$bwa_pop, num_tree=mvt_rec_rot$num_tree, rot_num=mvt_rec_rot$rot_num)
mvt_melt<-melt(mvt,id.vars = "bwa_pop")
mvt_year<-data.frame(mvt_melt$bwa_pop, variable=mvt_melt$variable, value=mvt_melt$value, Year=mvt_rec_rot$Year)


mvt_p1<-ggplot(mvt_rec_rot, aes(x = Year, y=mean_gsi, colour = bwa_pop))+
  scale_color_manual(values=c("red","blue","brown","cyan"), 
                     label=c("None","Light","Moderate","Heavy"),
                     name="")+
  geom_point()+
  geom_line()+
  ggtitle("Middle Valley Trail BWA GSI/Rotholz Summary")+
  ylim(-1,1.5)+
  xlim(2013.67,2016.35)+
  ylab("Mean GSI")+
  theme(legend.position="none")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position=c(.94,.83))


mvt_p2<-ggplot(mvt_year, aes(x=mvt_melt.bwa_pop, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'identity')+ facet_grid(~ Year)+
  scale_fill_manual(values=c(bblue3, borange2),name="", label=c("# of Trees","# w/ Rotholz"))+
  ylim(c(0,25))+
  ylab("No. of Trees")+
  xlab("BWA Population Size")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position=c(.94,.78))

ggarrange(mvt_p1, mvt_p2, ncol=1, nrow=2)

###Canaan Valley Sewer Plant
sew_rec_rot<-read.csv("Data/forest_service_data/SEW_rot_num.csv")
sew_rec_rot$bwa_pop <- factor(sew_rec_rot$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))
sew<-data.frame(bwa_pop=sew_rec_rot$bwa_pop, num_tree=sew_rec_rot$num_tree, rot_num=sew_rec_rot$rot_num)
sew_melt<-melt(sew,id.vars = "bwa_pop")
sew_year<-data.frame(sew_melt$bwa_pop, variable=sew_melt$variable, value=sew_melt$value, Year=sew_rec_rot$Year)


sew_p1<-ggplot(sew_rec_rot, aes(x = Year, y=mean_gsi, colour = bwa_pop))+
  scale_color_manual(values=c("red","blue","brown","white"), 
                     label=c("None","Light","Moderate",""),
                     name="")+
  geom_point()+
  geom_line()+
  ggtitle("CV Sewer Plant BWA GSI/Rotholz Summary")+
  ylim(-1,1.5)+
  xlim(2013.67,2016.35)+
  ylab("Mean GSI")+
  theme(legend.position="none")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position=c(.94,.83))


sew_p2<-ggplot(sew_year, aes(x=sew_melt.bwa_pop, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'identity')+ facet_grid(~ Year)+
  scale_fill_manual(values=c(bblue3, borange2),name="", label=c("# of Trees","# w/ Rotholz"))+
  ylim(c(0,25))+
  ylab("No. of Trees")+
  xlab("BWA Population Size")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position=c(.94,.78))

ggarrange(sew_p1, sew_p2, ncol=1, nrow=2)

###CV Volunteer Fire Department
vfd_rec_rot<-read.csv("Data/forest_service_data/VFD_rot_num.csv")
vfd_rec_rot$bwa_pop <- factor(vfd_rec_rot$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))
vfd<-data.frame(bwa_pop=vfd_rec_rot$bwa_pop, num_tree=vfd_rec_rot$num_tree, rot_num=vfd_rec_rot$rot_num)
vfd_melt<-melt(vfd,id.vars = "bwa_pop")
vfd_year<-data.frame(vfd_melt$bwa_pop, variable=vfd_melt$variable, value=vfd_melt$value, Year=vfd_rec_rot$Year)


vfd_p1<-ggplot(vfd_rec_rot, aes(x = Year, y=mean_gsi, colour = bwa_pop))+
  scale_color_manual(values=c("red","blue","white","white"), 
                     label=c("None","Light","",""),
                     name="")+
  geom_point()+
  geom_line()+
  ggtitle("CV Volunteer Fire Dept. BWA GSI/Rotholz Summary")+
  ylim(-1,1.5)+
  xlim(2013.67,2016.35)+
  ylab("Mean GSI")+
  theme(legend.position="none")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position=c(.94,.83))


vfd_p2<-ggplot(vfd_year, aes(x=vfd_melt.bwa_pop, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'identity')+ facet_grid(~ Year)+
  scale_fill_manual(values=c(bblue3, borange2),name="", label=c("# of Trees","# w/ Rotholz"))+
  ylim(c(0,25))+
  ylab("No. of Trees")+
  xlab("BWA Population Size")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position=c(.94,.78))

ggarrange(vfd_p1, vfd_p2, ncol=1, nrow=2)
