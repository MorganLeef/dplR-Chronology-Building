#Summary Figures Code

rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)

###Number of samples with rotholz
rot<-read.csv("rotholz_years.csv")
sampdepth<-read.csv("sample_depth.csv")
df<-melt(rot,id.vars = "Year")



###Plotting
ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat = 'identity', show.legend = FALSE, fill='gray60')+
  geom_line(y=sampdepth$Samples)+
  ggtitle("Rotholz Summary")+
  ylim(0,700)+
  ylab("# of Samples With Rotholz")+
  theme(legend.position=c(.05,.2))

       