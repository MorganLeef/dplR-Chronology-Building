###Script to create plot of Canaan Valley - North/ Poca/Rand South regional 
#climate summaries (correlation of tmax, tmin, tmax, ppt on regional .crn)
#****REQUIRES ClimResp.Rmd + data****
#Must change non-host coef.cal in ClimResp.Rmd to differentiate species
#Must run each segment as you update dataframes from "ClimResp repository"

library(magrittr)
library(ggpubr)
library(reshape2)
library(tidyverse)

###Canaan Valley tmean 0.05 cint
df<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df_melt<-melt(df, id.vars = "Month")
df_melt$Month <- factor(df_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)

cv_tmean<-ggplot(df_melt, aes(x = Month, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"))+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("CV Region Mean Temp")

###Canaan Valley tmin
df<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df_melt<-melt(df, id.vars = "Month")
df_melt$Month <- factor(df_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))


cv_tmin<-ggplot(df_melt, aes(x = Month, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"))+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("CV Region Min Temp")

###Canaan Valley tmax
df<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df_melt<-melt(df, id.vars = "Month")
df_melt$Month <- factor(df_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))


cv_tmax<-ggplot(df_melt, aes(x = Month, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"))+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("CV Region Max Temp")

###Canaan Valley ppt
df<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df_melt<-melt(df, id.vars = "Month")
df_melt$Month <- factor(df_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))


cv_ppt<-ggplot(df_melt, aes(x = Month, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"))+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("CV Region Mean Precipitation")


###Final Plot
ggarrange(cv_tmin, cv_tmean, cv_tmax, cv_ppt, ncol=2, nrow=2)

############################
###Poca-Rand tmean 0.05 cint
############################
df<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df_melt<-melt(df, id.vars = "Month")
df_melt$Month <- factor(df_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)

poca_tmean<-ggplot(df_melt, aes(x = Month, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"))+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Poca Region Mean Temp")

###Poca-Rand tmin
df<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df_melt<-melt(df, id.vars = "Month")
df_melt$Month <- factor(df_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))


poca_tmin<-ggplot(df_melt, aes(x = Month, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"))+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Poca Region Min Temp")

###Poca-Rand tmax
df<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df_melt<-melt(df, id.vars = "Month")
df_melt$Month <- factor(df_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))


poca_tmax<-ggplot(df_melt, aes(x = Month, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"))+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Poca Region Max Temp")

###Poca-Rand ppt
df<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df_melt<-melt(df, id.vars = "Month")
df_melt$Month <- factor(df_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))


poca_ppt<-ggplot(df_melt, aes(x = Month, y=value, fill=variable))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"))+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Poca Region Mean Precipitation")


###Final Plot
ggarrange(poca_tmin, poca_tmean, poca_tmax, poca_ppt, ncol=2, nrow=2)



