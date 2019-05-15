sub_fre_gsi<-subset(ef_defol, year > 2015 & year < 2017)
#fre_gsi.txt<-write.table(sub_fre_gsi, file = "/Users/aleef/Desktop/fre_gis.txt", row.names = T, col.names = T)


fre_samp<-read.csv("forest_service_data/FRE_rot_freq_by_pop_samp_depth.csv")
fre_rec<-read.csv("forest_service_data/FRE_rot_freq_by_pop.csv")

ggplot(fre_rec, aes(x = bwa_pop, y = rot_tree, fill = bwa_pop ))+
  geom_bar(colour="black", stat="identity")+
  geom_line(y=fre_samp$num_tree)
  
#All FRE data from 2014-16  





