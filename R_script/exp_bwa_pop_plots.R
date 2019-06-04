#All USFS FRE data from 2014-16
rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)
library(tidyverse)


#sub_defol<-subset(ef_defol, ef_defol$year > 2015 & ef_defol$year < 2017)
bblue = rgb(100,175,200, max=255)
bblue2 = rgb(100,175,150, max=255)
borange = rgb(175,100,100, max=255)
bred  = rgb(255,100,100, max=255)

######################
###BWA pop - % Rotholz
######################

###FRE

fre_rec_rot<-read.csv("data/forest_service_data/FRE_rot_freq_by_pop_full_record.csv")
fre_rec_rot$bwa_pop <- factor(fre_rec_rot$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))

#Base R plots
colours <- c("cyan", "blue", "orange", "red")

plot.new()
bp<-barplot(fre_rec_rot$rot_tree, main = "Freeland Road BWA Population - % Rotholz", 
        beside = TRUE, width = 1, col = colours, ylab = "% Rotholz", xlab = "BWA Pop", ylim = c(0,200),
        names.arg = c("","2014","","","","2015","","","","2016","",""))

legend("topright",c("None","Light","Moderate","Heavy"), cex=0.5, fill=colours)
text(bp, 0, round(fre_rec_rot$num_tree, 1),cex=0.75, pos = 3)
        
par(new = TRUE)
plot(fre_rec_rot$Year[fre_rec_rot$bwa_pop=="None"],fre_rec_rot$mean_gsi[fre_rec_rot$bwa_pop=="None"], 
     type = "l", ylab = "", xlab = "", ylim = c(-2,2),
     xaxt = "n", yaxt = "n", col=colours, bty="n")     
lines(fre_rec_rot$Year[fre_rec_rot$bwa_pop=="Light"],fre_rec_rot$mean_gsi[fre_rec_rot$bwa_pop=="Light"], type = "l", col="blue")
axis(side = 4)
mtext(side = 4, line = 3, 'Mean GSI')



#ggplot
ggplot(fre_rec_rot, aes(x = Year, y=mean_gsi, colour = bwa_pop))+
  geom_line()+
  ylim(-1,2)

ggplot(fre_rec_rot, aes(x=Year, y=rot_tree, fill=bwa_pop, label = num_tree))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(bblue, bblue2, borange, bred),name="BWA Population", label=c("None","Light", "Moderate", "Heavy"))+
  ylim(c(0,100))+
  ylab("% Rotholz")+
  ggtitle("Freeland Road BWA Population-Rotholz Summary")+
  geom_label(mapping=NULL,data=NULL,stat="identity",position_dodge(width = 1), size = 3)

#geom_line(data = fre_rec_rot, aes(x = Year, y = mean_gsi))+
###MAL
mal_rec_rot<-read.csv("data/forest_service_data/MAL_rot_freq_by_pop_full_record.csv")
mal_rec_rot$bwa_pop <- factor(mal_rec_rot$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))

ggplot(mal_rec_rot, aes(x=Year, y=rot_tree, fill=bwa_pop, label = num_tree))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(bblue, bblue2, borange, bred),name="BWA Population", label=c("None","Light", "Moderate", "Heavy"))+
  ylim(c(0,100))+
  ylab("% Rotholz")+
  ggtitle("Mallow Lake BWA Population-Rotholz Summary")+
  geom_label(mapping=NULL,data=NULL,stat="identity",position_dodge(width = 1), size = 3)

###MVT
mvt_rec_rot<-read.csv("data/forest_service_data/MVT_rot_freq_by_pop_full_record.csv")
mvt_rec_rot$bwa_pop <- factor(mvt_rec_rot$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))

ggplot(mvt_rec_rot, aes(x=Year, y=rot_tree, fill=bwa_pop, label = num_tree))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(bblue, bblue2, borange, bred),name="BWA Population", label=c("None","Light", "Moderate", "Heavy"))+
  ylim(c(0,100))+
  ylab("% Rotholz")+
  ggtitle("Middle Valley Trail BWA Population-Rotholz Summary")+
  geom_label(mapping=NULL,data=NULL,stat="identity",position_dodge(width = 1), size = 3)

###SEW
sew_rec_rot<-read.csv("data/forest_service_data/SEW_rot_freq_by_pop_full_record.csv")
sew_rec_rot$bwa_pop <- factor(sew_rec_rot$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))

ggplot(sew_rec_rot, aes(x=Year, y=rot_tree, fill=bwa_pop, label = num_tree))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(bblue, bblue2, borange, bred),name="BWA Population", label=c("None","Light", "Moderate", "Heavy"))+
  ylim(c(0,100))+
  ylab("% Rotholz")+
  ggtitle("CV Sewer Plant BWA Population-Rotholz Summary")+
  geom_label(mapping=NULL,data=NULL,stat="identity",position_dodge(width = 1), size = 3)

###VFD
vfd_rec_rot<-read.csv("data/forest_service_data/VFD_rot_freq_by_pop_full_record.csv")
vfd_rec_rot$bwa_pop <- factor(vfd_rec_rot$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))

ggplot(vfd_rec_rot, aes(x=Year, y=rot_tree, fill=bwa_pop, label = num_tree))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(bblue, bblue2, borange, bred),name="BWA Population", label=c("None","Light", "Moderate", "Heavy"))+
  ylim(c(0,100))+
  ylab("% Rotholz")+
  ggtitle("Volunteer Fire Dept. BWA Population-Rotholz Summary")+
  geom_label(mapping=NULL,data=NULL,stat="identity",position_dodge(width = 1), size = 3)
