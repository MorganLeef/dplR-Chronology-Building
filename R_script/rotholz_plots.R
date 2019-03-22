###Rotholz/Resin Canal Data Plotting

rm(list=ls())

library(ggplot2)
library(magrittr)
library(ggpubr)

rot<-read.csv("PsuedoRot.csv")

ggplot(rot, aes(x=Year, y= ))