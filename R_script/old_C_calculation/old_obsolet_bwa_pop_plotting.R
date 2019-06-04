###Obsolete BWA Population to % Rotholz plots.
#Kept for useful coding and plotting techniques


#fre_rot<-melt(fre_rec_rot,id.vars = "Year")
#fre_rec2<-read.csv("forest_service_data/FRE_rot_freq_by_pop_2015.csv")
#fre_rec3<-read.csv("forest_service_data/FRE_rot_freq_by_pop_2016.csv")
#fre_rec<-read.csv("forest_service_data/FRE_rot_freq_by_pop_all_time.csv")
#Old plot
#ggplot(fre_rec, aes(x = bwa_pop, y = rot_tree, fill = bwa_pop ))+
#  geom_bar(colour="black", stat="identity")+
#  geom_bar(aes(p1))+
#  ylim(0,70)
ggplot(fre_test, aes(fill=variable,x = bwa_pop, y = value))+
  geom_bar(position = "dodge", stat = "identity")+
  ylim(c(0,100))+
  ggtitle("FRE 2014 BWA Pop/% Rotholz")
#df$bwa_pop <- factor(df$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))
#df3<-melt(fre_rec3,id.vars = "bwa_pop")
#df$bwa_pop <- factor(df$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))

#Old stacked plot
#ggplot(df, aes(x = bwa_pop, y = value,fill=variable))+
#geom_bar(stat='identity')+
#scale_fill_manual(values=c("brown2", "steelblue4"),name="",label=c("% Rotholz", "# of Trees"))+
#scale_y_continuous(limits = c(0, 100),sec.axis = sec_axis(~.*1, name = "Sample Depth"))+
#ylab("% Rotholz")+
#xlab("BWA Population Size")+
#theme(legend.position=c(.85,.83))+
#ggtitle("Freeland Road BWA Population- % Rotholz (2014-16)")

fre_gsi<-read.csv("forest_service_data/FRE_inf_data.csv")

#ggplot(fre_gsi, aes(x=tree_id, y=tree_gsi))+
#geom_line()

###MAL
MAL_rec<-read.csv("forest_service_data/MAL_rot_freq_by_pop_all_time.csv")
#Old plot
#ggplot(fre_rec, aes(x = bwa_pop, y = rot_tree, fill = bwa_pop ))+
#  geom_bar(colour="black", stat="identity")+
#  geom_bar(aes(p1))+
#  ylim(0,70)

df<-melt(MAL_rec,id.vars = "bwa_pop")
df$bwa_pop <- factor(df$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))

ggplot(df, aes(x = bwa_pop, y = value,fill=variable))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=c("brown2", "steelblue4"),name="",label=c("% Rotholz", "# of Trees"))+
  scale_y_continuous(limits = c(0, 100),sec.axis = sec_axis(~.*1, name = "Sample Depth"))+
  ylab("% Rotholz")+
  xlab("BWA Population Size")+
  theme(legend.position=c(.85,.83))+
  ggtitle("Mallow Lake BWA Population- % Rotholz (2014-16)")

fre_gsi<-read.csv("forest_service_data/FRE_inf_data.csv")

#ggplot(fre_gsi, aes(x=tree_id, y=tree_gsi))+
#geom_line()

###MVT
MVT_rec<-read.csv("forest_service_data/MVT_rot_freq_by_pop_all_time.csv")
#Old plot
#ggplot(fre_rec, aes(x = bwa_pop, y = rot_tree, fill = bwa_pop ))+
#  geom_bar(colour="black", stat="identity")+
#  geom_bar(aes(p1))+
#  ylim(0,70)

df<-melt(MVT_rec,id.vars = "bwa_pop")
df$bwa_pop <- factor(df$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))

ggplot(df, aes(x = bwa_pop, y = value,fill=variable))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=c("brown2", "steelblue4"),name="",label=c("% Rotholz", "# of Trees"))+
  scale_y_continuous(limits = c(0, 100),sec.axis = sec_axis(~.*1, name = "Sample Depth"))+
  ylab("% Rotholz")+
  xlab("BWA Population Size")+
  theme(legend.position=c(.85,.83))+
  ggtitle("Middle Valley Trail BWA Population- % Rotholz (2014-16)")

fre_gsi<-read.csv("forest_service_data/FRE_inf_data.csv")

#ggplot(fre_gsi, aes(x=tree_id, y=tree_gsi))+
#geom_line()

###SEW
SEW_rec<-read.csv("forest_service_data/SEW_rot_freq_by_pop_all_time.csv")
#Old plot
#ggplot(fre_rec, aes(x = bwa_pop, y = rot_tree, fill = bwa_pop ))+
#  geom_bar(colour="black", stat="identity")+
#  geom_bar(aes(p1))+
#  ylim(0,70)

df<-melt(SEW_rec,id.vars = "bwa_pop")
df$bwa_pop <- factor(df$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))

ggplot(df, aes(x = bwa_pop, y = value,fill=variable))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=c("brown2", "steelblue4"),name="",label=c("% Rotholz", "# of Trees"))+
  scale_y_continuous(limits = c(0, 100),sec.axis = sec_axis(~.*1, name = "Sample Depth"))+
  ylab("% Rotholz")+
  xlab("BWA Population Size")+
  theme(legend.position=c(.85,.83))+
  ggtitle("CV Sewer Plant BWA Population- % Rotholz (2014-16)")

fre_gsi<-read.csv("forest_service_data/FRE_inf_data.csv")

#ggplot(fre_gsi, aes(x=tree_id, y=tree_gsi))+
#geom_line()

###VFD
VFD_rec<-read.csv("forest_service_data/VFD_rot_freq_by_pop_all_time.csv")
#Old plot
#ggplot(fre_rec, aes(x = bwa_pop, y = rot_tree, fill = bwa_pop ))+
#  geom_bar(colour="black", stat="identity")+
#  geom_bar(aes(p1))+
#  ylim(0,70)

df<-melt(VFD_rec,id.vars = "bwa_pop")
df$bwa_pop <- factor(df$bwa_pop,levels = c("None", "Light", "Moderate", "Heavy"))

ggplot(df, aes(x = bwa_pop, y = value,fill=variable))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=c("brown2", "steelblue4"),name="",label=c("% Rotholz", "# of Trees"))+
  scale_y_continuous(limits = c(0, 100),sec.axis = sec_axis(~.*1, name = "Sample Depth"))+
  ylab("% Rotholz")+
  xlab("BWA Population Size")+
  theme(legend.position=c(.85,.83))+
  ggtitle("CV Volunteer Fire Dept BWA Population- % Rotholz (2014-16)")

fre_gsi<-read.csv("forest_service_data/FRE_inf_data.csv")

#ggplot(fre_gsi, aes(x=tree_id, y=tree_gsi))+
#geom_line()

specie=c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition=rep(c("normal" , "stress" , "Nitrogen") , 4)
value=abs(rnorm(12 , 0 , 15))
data=data.frame(specie,condition,value)