#All USFS FRE data from 2014-16
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)
library(tidyverse)


fre_rec<-read.csv("forest_service_data/FRE_rot_freq_by_pop_all_time.csv")
#Old plot
#ggplot(fre_rec, aes(x = bwa_pop, y = rot_tree, fill = bwa_pop ))+
#  geom_bar(colour="black", stat="identity")+
#  geom_bar(aes(p1))+
#  ylim(0,70)

df<-melt(fre_rec_2014,id.vars = "bwa_pop")
df$bwa_pop <- factor(df$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))
  
ggplot(df, aes(x = bwa_pop, y = value,fill=variable))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=c("brown2", "steelblue4"),name="",label=c("% Rotholz", "# of Trees"))+
  scale_y_continuous(limits = c(0, 100),sec.axis = sec_axis(~.*1, name = "Sample Depth"))+
  ylab("% Rotholz")+
  xlab("BWA Population Size")+
  theme(legend.position=c(.85,.83))+
  ggtitle("Freeland Road BWA Population- % Rotholz (2014)")

fre_gsi<-read.csv("forest_service_data/FRE_inf_data.csv")

ggplot(fre_gsi, aes(x=tree_id, y=tree_gsi))+
  geom_line()
  


