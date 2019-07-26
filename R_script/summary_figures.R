#Summary Figures Code

rm(list=ls())
library(dplR)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(reshape2)

###Number of samples with rotholz
#rot<-read.csv("Data/rotholz_data/rotholz_years.csv")
rot<-read.csv("Data/rotholz_data/rotholz_years_percent.csv")
sampdepth<-read.csv("Data/rotholz_data/sample_depth.csv")
df<-melt(rot,id.vars = "Year")



###Plotting
ggplot(df, aes(x = Year, y = value,fill=variable))+
  geom_bar(stat = 'identity', show.legend = FALSE, fill='gray60')+
  geom_line(y=sampdepth$scale)+
  ggtitle("Rotholz Summary")+
  scale_y_continuous(limits = c(0,60),sec.axis = sec_axis(~.*12, name = "Sample Depth"))+
  ylab("Percent of Samples With Rotholz")+
  theme(legend.position=c(.05,.2))

       