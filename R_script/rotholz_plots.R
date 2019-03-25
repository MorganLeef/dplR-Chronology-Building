###Rotholz/Resin Canal Data Plotting

rm(list=ls())

library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)

BDR_rot<-read.csv("BDR.csv")

#WIP line plot
ggplot(BDR_rot, aes(x=Year))+
  geom_line(aes(y=BDR_rot$Rotholz_n))+
  geom_line(aes(y=BDR_rot$Rotholz_p),linetype = "dashed")+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Percent of Cores"))+
  labs(y = "Number of Cores",x = "Year")+
  theme(legend.position = c(.05,.2))

#Arguement to change xy bounds
coord_cartesian(ylim = c(0, 100))   

  
#Bar chart method  
df<-melt(BDR_rot,id.vars = "Year")
  
ggplot(df, aes(x = Year, y = value,fill=variable)) +
  geom_bar(stat='identity')  
