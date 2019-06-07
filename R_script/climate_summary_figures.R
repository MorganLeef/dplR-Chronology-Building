###Script to create plot of Canaan Valley - North/ Poca/Rand South regional 
#climate summaries (correlation of tmax, tmin, tmax, ppt, pdsi on regional .crn)
#****REQUIRES ClimResp.Rmd + data****
#Must run each segment as you run all chunks of ClimResp.Rmd for each
#climate variable to update dataframes from "ClimResp repository" as they
#are overwritten
#Climate variables must also be run in the order they appear in this script

library(magrittr)
library(ggpubr)
library(reshape2)
library(tidyverse)

#Create colors for host (cred) and non-host (cblue)
cblue = rgb(125,200,200, max=255)
cred  = rgb(200,125,125, max=255)

#Segment #1
###Canaan Valley tmean 0.10 cint -- Data frame reformatting
df1<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
#Melting to create long format for bar plots
df1_melt<-melt(df1, id.vars = "Month")
#Reorder the months to assure they plot in correct order
df1_melt$Month <- factor(df1_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))
#Create new dataframe containing the significance values 
df1_sig<-data.frame(Month=coef.cal$month, sig_h=coef.cal$significant, sig_nh=coef.cal_nh$significant)
df1_sig_melt<-melt(df1_sig, id.vars = "Month")

#Plots the correlation for tmean for both species, with significant values
#highlighted in black
cv_tmean<-ggplot(df1_melt, aes(x = Month, y=value, fill=variable, colour=df1_sig_melt$value))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"), guide=FALSE)+
  scale_colour_manual(values=c("white", "black"),name="",label=c("Not Sig", "Sig"), guide=FALSE)+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("CV Region Mean Temp")
  

#Segment #2
###Canaan Valley tmin
df2<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df2_melt<-melt(df2, id.vars = "Month")
df2_melt$Month <- factor(df2_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

df2_sig<-data.frame(Month=coef.cal$month, sig_h=coef.cal$significant, sig_nh=coef.cal_nh$significant)
df2_sig_melt<-melt(df2_sig, id.vars = "Month")

cv_tmin<-ggplot(df2_melt, aes(x = Month, y=value, fill=variable, colour=df2_sig_melt$value))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"), guide=FALSE)+
  scale_colour_manual(values=c("white", "black"),name="",label=c("Not Sig", "Sig"), guide=FALSE)+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("CV Region Min Temp")


#Segment #3
###Canaan Valley tmax
df3<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df3_melt<-melt(df3, id.vars = "Month")
df3_melt$Month <- factor(df3_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

df3_sig<-data.frame(Month=coef.cal$month, sig_h=coef.cal$significant, sig_nh=coef.cal_nh$significant)
df3_sig_melt<-melt(df3_sig, id.vars = "Month")

cv_tmax<-ggplot(df3_melt, aes(x = Month, y=value, fill=variable, colour=df3_sig_melt$value))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"), guide=FALSE)+
  scale_colour_manual(values=c("white", "black"),name="",label=c("Not Sig", "Sig"), guide=FALSE)+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("CV Region Max Temp")

#Segment #4
###Canaan Valley ppt
df4<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df4_melt<-melt(df4, id.vars = "Month")
df4_melt$Month <- factor(df4_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

df4_sig<-data.frame(Month=coef.cal$month, sig_h=coef.cal$significant, sig_nh=coef.cal_nh$significant)
df4_sig_melt<-melt(df4_sig, id.vars = "Month")

cv_ppt<-ggplot(df4_melt, aes(x = Month, y=value, fill=variable, colour=df4_sig_melt$value))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"), guide=FALSE)+
  scale_colour_manual(values=c("white", "black"),name="",label=c("Not Sig", "Sig"), guide=FALSE)+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("CV Region Mean Precipitation")


###Final Plot 
ggarrange(cv_tmean, cv_tmin, cv_tmax, cv_ppt, ncol=2, nrow=2)

#png(p1.png)
#p1<-ggarrange(cv_tmean, cv_tmin, cv_tmax, cv_ppt, ncol=2, nrow=2)
#dev.off()

############################
###Poca-Rand 0.10 cint
############################
###Poca tmean
df5<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df5_melt<-melt(df5, id.vars = "Month")
df5_melt$Month <- factor(df5_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

df5_sig<-data.frame(Month=coef.cal$month, sig_h=coef.cal$significant, sig_nh=coef.cal_nh$significant)
df5_sig_melt<-melt(df5_sig, id.vars = "Month")

poca_tmean<-ggplot(df5_melt, aes(x = Month, y=value, fill=variable, colour=df5_sig_melt$value))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"), guide=FALSE)+
  scale_colour_manual(values=c("white", "black"),name="",label=c("Not Sig", "Sig"), guide=FALSE)+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Poca Region Mean Temp")

###Poca-Rand tmin
df6<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df6_melt<-melt(df6, id.vars = "Month")
df6_melt$Month <- factor(df6_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

df6_sig<-data.frame(Month=coef.cal$month, sig_h=coef.cal$significant, sig_nh=coef.cal_nh$significant)
df6_sig_melt<-melt(df6_sig, id.vars = "Month")

poca_tmin<-ggplot(df6_melt, aes(x = Month, y=value, fill=variable, colour=df6_sig_melt$value))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"), guide=FALSE)+
  scale_colour_manual(values=c("white", "black"),name="",label=c("Not Sig", "Sig"), guide=FALSE)+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Poca Region Min Temp")

###Poca-Rand tmax
df7<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df7_melt<-melt(df7, id.vars = "Month")
df7_melt$Month <- factor(df7_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

df7_sig<-data.frame(Month=coef.cal$month, sig_h=coef.cal$significant, sig_nh=coef.cal_nh$significant)
df7_sig_melt<-melt(df7_sig, id.vars = "Month")

poca_tmax<-ggplot(df7_melt, aes(x = Month, y=value, fill=variable, colour=df7_sig_melt$value))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"), guide=FALSE)+
  scale_colour_manual(values=c("white", "black"),name="",label=c("Not Sig", "Sig"), guide=FALSE)+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Poca Region Max Temp")

###Poca-Rand ppt
df8<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df8_melt<-melt(df8, id.vars = "Month")
df8_melt$Month <- factor(df8_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

df8_sig<-data.frame(Month=coef.cal$month, sig_h=coef.cal$significant, sig_nh=coef.cal_nh$significant)
df8_sig_melt<-melt(df8_sig, id.vars = "Month")

poca_ppt<-ggplot(df8_melt, aes(x = Month, y=value, fill=variable, colour=df8_sig_melt$value))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"), guide=FALSE)+
  scale_colour_manual(values=c("white", "black"),name="",label=c("Not Sig", "Sig"), guide=FALSE)+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Poca Region Mean Precipitation")


###Final Plot
ggarrange(poca_tmean, poca_tmin, poca_tmax, poca_ppt, ncol=2, nrow=2)

#################################
###PDSI Data#####################
##Canaan Valley / Northern Region
df<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df_melt<-melt(df, id.vars = "Month")
df_melt$Month <- factor(df_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

df_sig<-data.frame(Month=coef.cal$month, sig_h=coef.cal$significant, sig_nh=coef.cal_nh$significant)
df_sig_melt<-melt(df_sig, id.vars = "Month")

cv_pdsi<-ggplot(df_melt, aes(x = Month, y=value, fill=variable, colour=df_sig_melt$value))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"), guide = FALSE)+
  scale_colour_manual(values=c("white", "black"),name="",label=c("Not Sig", "Sig"), guide=FALSE)+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("CV Region PDSI")

###Poca-Rand / Southern Region
df<-data.frame(Month=coef.cal$month, Coef_h=coef.cal$coef, Coef_nh=coef.cal_nh$coef)
df_melt<-melt(df, id.vars = "Month")
df_melt$Month <- factor(df_melt$Month,levels = 
                          c("May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

df_sig<-data.frame(Month=coef.cal$month, sig_h=coef.cal$significant, sig_nh=coef.cal_nh$significant)
df_sig_melt<-melt(df_sig, id.vars = "Month")

poca_pdsi<-ggplot(df_melt, aes(x = Month, y=value, fill=variable, colour=df_sig_melt$value))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values=c(cred, cblue),name="",label=c("Host Coef", "Nonhost Coef"), guide = FALSE)+
  scale_colour_manual(values=c("white", "black"),name="",label=c("Not Sig", "Sig"), guide=FALSE)+
  ylab("Coef")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Poca Region PDSI")


###Final Plot
ggarrange(cv_pdsi, poca_pdsi, ncol=2, nrow=1)



