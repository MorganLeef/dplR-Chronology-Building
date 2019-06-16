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
  ylab("Number of Trees")+
  xlab("BWA Population Size")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position=c(.94,.78))

ggarrange(fre_p1, fre_p2, ncol=1, nrow=2)

#geom_label(mapping=NULL,data=NULL,stat="identity",position_dodge(width = 1), size = 3,show.legend = FALSE)
#scale_fill_manual(values=c(bblue, bblue2, borange, bred),name="BWA Population", label=c("None","Light", "Moderate", "Heavy"))+




