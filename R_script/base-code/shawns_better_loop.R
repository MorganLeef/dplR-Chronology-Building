rm(list=ls())
setwd("C:/Users/aleef/Desktop/COFECHA/All_testcode/dplR-Chronology-Building")

library(dplR)

#new addition
#library(dplyr)

## added full.names=TRUE so you don't have to change your working directory.
file_list_abba<-list.files(path = "data/", pattern = "*ABBA.rwl", full.names=TRUE)
file_list_pcru<-list.files(path = "data/", pattern = "*PCRU.rwl", full.names=TRUE)

#setwd("C:/Users/aleef/Desktop/COFECHA/All_testcode/dplR-Chronology-Building/data")

## I took your chronology building out of the graph loop for now.
## Start with the parts of the whole before creating the whole in a swift motion.


# Create an empty list to store your ABBA crns
crn.abba <- list()
for (index_abba in 1:length(file_list_abba)){
  # this can be done in one step.
  #a<-read.rwl(file_list_abba[index_abba])
  #b<-chron(detrend(a, method = "Mean"), m = substring(file_list_abba[index_abba], 1,3))
  
  # Substring changes because of the full.names command in list.files()...
  # Name the dataframe using substring and characters ".crn"; e.g. BDR.crn
  # for crn.abba[[...]] - [[]] puts it into a list of dataframes; [] just creates a list of lists
  crn.abba[[paste0(substring(file_list_abba[index_abba], 6, 8), ".crn")]] <- 
    chron(detrend(read.rwl(file_list_abba[index_abba]), method = "ModNegExp"), 
          prefix = substring(file_list_abba[index_abba], 6, 8))
  #Added a year column since working with row names later is a pain.
  crn.abba[[paste0(substring(file_list_abba[index_abba], 6, 8), ".crn")]]$Year <- 
    row.names(crn.abba[[paste0(substring(file_list_abba[index_abba], 6, 8), ".crn")]])
}

# Second verse, same as the first.... 
crn.pcru <- list()
  for (index_pcru in 1:length(file_list_pcru)){
    #c<-read.rwl(file_list_pcru[index_pcru])
    #d<-chron(detrend(c, method = "Mean"), m = substring(file_list_pcru[index_pcru], 1,3))
    crn.pcru[[paste0(substring(file_list_pcru[index_pcru], 6, 8), ".crn")]] <-
    chron(detrend(read.rwl(file_list_pcru[index_pcru]), method = "ModNegExp"), 
            prefix = substring(file_list_pcru[index_pcru], 6, 8))
    crn.pcru[[paste0(substring(file_list_pcru[index_pcru], 6, 8), ".crn")]]$Year <- 
      row.names(crn.pcru[[paste0(substring(file_list_pcru[index_pcru], 6, 8), ".crn")]])
    
    #a<-chron(detrend(read.rwl(file_list_pcru[index_pcru]), method = "Mean"), 
    #         m = substring(file_list_pcru[index_pcru], 6, 8))
  }

# New packages to use
library(reshape2)
library(ggplot2)

# Function to create a template to facet your graphs. 
# found here: https://stackoverflow.com/questions/18933575/easily-add-an-all-facet-to-facet-wrap-in-ggplot2
# Change line 61 and 62 if you don't want the title to be Comparison....
CreateAllFacet <- function(df, col){
  df$facet <- df[[col]]
  temp <- df
  temp$facet <- "Comparison"
  temp$facet = factor(temp$facet, levels=c("ABBA", "PCRU", "Comparison"))
  merged <-rbind(temp, df)
  
  # ensure the facet value is a factor
  merged[[col]] <- as.factor(merged[[col]])
  
  return(merged)
}

# Combines the crns from previously. 
# Ifelse is to put a stop to it if the names don't match up for some reason.
# This bit produces the graphs.
for (idx in 1:length(crn.abba)){
  ifelse(substring(names(crn.abba[idx]),1,3) != substring(names(crn.pcru[idx]), 1,3), 
         print("HOLY CRAP IT DIDN'T WORK!"),# setNames: give the columns an appropriate name for melting. Don't change unless you follow it through the rest of the code.  
         b <- setNames(merge(crn.abba[[idx]][,c(1,3)], crn.pcru[[idx]][,c(1,3)], by = "Year", all=TRUE), c("Year", "ABBA", "PCRU")))
  mlt.dfs <- melt( b , id.vars = "Year")

# Use online function for plotting
facet.df <- CreateAllFacet(mlt.dfs, 2)
# Get first year and end at 2015 - rounded to nearest 10 year. 
beg <- c(seq(signif(min(as.numeric(facet.df$Year)), digits=3), 2015, by=5))
#assign creates a ggplot variable by the name of the chronology.
assign(substring(names(crn.abba[idx]), 1,3),
  #set up the ggplot. [!is.na(facet.df$value),] tells it to only use rows that aren't NA. group is how to treat the data
  #For this it's the site + comparison
  ggplot(data = facet.df[!is.na(facet.df$value),], aes(x=as.numeric(as.character(Year)), y=value, group = variable))+
  # make it a line graph + tell it how to color the lines (you can change the red & blue to something else.)
  geom_line(aes(color=variable))+scale_color_manual(values=c("red","blue"))+
  #Creates an individual graph based on the facet column created by the function
  facet_wrap(~facet, ncol=1, scales="free_y" )+
  # Tinkering around with grids. I left the 10 grid marks to help align the graphs.
  theme(panel.grid.major.x = element_line(colour="grey", size=0.5),
        panel.grid.major.y= element_blank(),
        panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill="white"))+
  # Use the sequence from before to create x-axis labels
  scale_x_continuous(breaks=beg)+
  theme(axis.text.x=element_text(size=12))+
  # Give labels everywhere - if you want it blank copy and paste the element_blank() bit at the end.
  labs(title="This is a graph! Do you like it?", x="Years", y="Index")+#element_blank()
  # Uncomment this line if you want to get rid of the y-axis labels.
  #theme(axis.ticks.y=element_blank(), axis.text.y=element_blank())+
  # No legend!
  theme(legend.position="none")
)
rm(b, facet.df, mlt.dfs, beg, idx)
}

## Now each chronology has a graph. to show it, type its name
## EG: 
BDR
