#**Project Narrative** 
_Name this according to the actual purpose so others can find it.  Most of this needs to be written to the user, rather than to me! I started editing this file for better display but it needs significant work_

  In this project, I created three scripts that utilize dplR and base R plotting to create 14 figures from my own ring-width data. 
The "read_file_loop.R" in the R_script folder is the main script that is used to create the main plots, while the two scripts in the base_code 
folder are simplified functions that are designed to run on only one site, rather than looping over all 14 sites. I added them just to illustrate
what each individual sections of the loop in read_file_loop.R accomplish.

  Once the user opens the "dplR-Chronology-Building.Rproj" file, it should open the workspace with everything needed to run the read_file_loop.R
script. All commands have the paths of the data incorperated to the code, so the user should only have to set the working directory as soon as
they open the project (the working directory should be /dplR-Chronology-Building/). The data file contains all 28 files that are used in the
read_file_loop.R script. All 28 files are raw ring widths measurements. The file names are structured where a three letter abbreviation of the 
site makes up the first half and the species takes up the other half, separated by an underscore. So the ABBA file for the Courtland Road site 
would be called "COR_ABBA.rwl". This format is used so that each half of the site name can be called separately in the loop, since we will be 
looping slightly different code on each species. 

### Site Names List
After dplR has been loaded in, the next set of code creates two lists containing all the file names for each species. There should be 14 
files for each species since there are 14 sites and two species per site. We also have to change the working directory to the path of the files,
since the path could not be specified with the method I used to call the site names.

    file_list_abba<-list.files(path = "data/", pattern = "*ABBA.rwl")
    file_list_pcru<-list.files(path = "data/", pattern = "*PCRU.rwl")

    setwd("data")

## Loop Over Sites
Now that we have all the objects and dataframes ready to call, we can begin the loop. The loop is set up as a nested "for" loop, since we have to loop the code over two different sets of files. 

    for (index_abba in 1:length(file_list_abba)){
      a<-read.rwl(file_list_abba[index_abba])    ###reads in each tuscon format file
      b<-chron(detrend(a, method = "Mean"), m =     substring(file_list_abba[index_abba], 1,3))  ###creates a standardized index for each file, then a chronology

#####Line 28 contains the initial call to the ABBA file list, and creates the index_abba object that will be called throughout the loop. 
###In this combination, the file_list_abba[index_abba] object is just a character string of ABBA file names. Line 29 uses dplR's read.rwl 
function to read in each ABBA file, and creates a dataframe for each tuscon formatted file (object "a"). Line 30 runs two functions (nested function?)
on each "a" that is created for each ABBA file. This first is detrend(), which I used to standardize and detrend each file using the "Mean"
method, which creates an ring width index (.rwi) file for each .rwl "a" object. The second function is the "chron" function, which converts
each .rwi file into a chronology file (.crn). Now we have mean chronology objects (object b) for each ABBA file.

##### Lines 41 through 43 repeat the same code for each PCRU file in our file_list_pcru list.

  for (index_pcru in 1:length(file_list_pcru)){
    c<-read.rwl(file_list_pcru[index_pcru])   ###same as the last lines but for PCRU files
    d<-chron(detrend(c, method = "Mean"), m = substring(file_list_pcru[index_pcru], 1,3))
 
 #### The next section of code creates a callable "Years" rowname object for each species, since dplR uses column 0 for the years rownames. 
    
    years_ABBA<-as.numeric(rownames(b))  
    years_PCRU<-as.numeric(rownames(d))
    
  
  ### Next, to be able to plot these chronologies in base R, I needed to make them the same size. At this point, each file has a different number
  of samples that span a different lenght of time. R doesn't like to plot objects that have different lengths on the same axis. So, I subset
  each file to only include the data from 1980 to 2016, since this is the window we will be plotting from.
  
    sub_abba<-subset(b, years_ABBA > 1979 & years_ABBA < 2017) ###subsets each chronology to be the same length (1980-2016)
    sub_pcru<-subset(d, years_PCRU > 1979 & years_PCRU < 2017) ###not sure why I did this twice (too afraid to change)
    
  ### After all the files are subsetted, I could then perform a basic difference operation to simulate the difference in growth between the
  host (ABBA) and non-host (PCRU) species. This is a simplified version of that operation, so I just subtracted the values from the PCRU index
  from that of the ABBA index, and created the "diff" object.
  
    diff<-(sub_abba - sub_pcru) ###performs basic index subtraction operation to simulate host vs non-host model (extremely simplified)
  
  ### After that, I needed to create a callable "Years" rowname object for the "diff" dataframe for its plot.
  
    sub_year_abba<-as.numeric(rownames(sub_abba))  
 ### Finally, with the data ready to be called, I was able to start plotting. Since there will be four plots, it was logical to create a
 single figure with all graphs plotted in a 4 x 4 matrix, so I set those parameters here.
 
    par(mfrow=c(2,2))
    
### The first graph will plot the mean chronology for each ABBA file, where the x-axis is time (1980-2016), and the y-axis is the ring width
index (RWI usually from 0.1 - 2.0) where the mean is always one.
    
    plot(years_ABBA, b$xxxstd, type='l', axes=F, xlab='', ylab='', main= file_list_abba[index_abba], col="blue", xlim=c(1980, 2017)) ###ABBA chronology
    axis(1, seq(1980, 2020, by=5))
    mtext(side=1, "Year", line=2)
    axis(2, seq(-0.5, 2.0, by=0.5))
    mtext(side=2, "RWI", line=2)
    abline(h=1.0, col="black")
    
### The second graph plots the mean chronology for the PCRU files, exactly as they were plotted for the ABBA files.    
    
    plot(years_PCRU, d$xxxstd, type='l', col="red", axes=F, xlab='', main = file_list_pcru[index_pcru], ylab='', ylim = c(0.5,2.0), xlim=c(1980, 2017)) ###PCRU chronology
    axis(1, seq(1980, 2020, by=5))
    mtext(side=1, "Year", line=2)
    axis(2, seq(-0.5, 2.0, by=0.5))
    mtext(side=2, "RWI", line=2)
    abline(h=1.0, col="black")
    
### The third graph plots the difference index between the ABBA and PCRU files for each site (derived from the "diff" and sub_year_diff object)    
    
    plot(sub_year_abba, diff$xxxstd, type='l', axes=F, xlab='', ylab='', main = "ABBA/PCRU Difference", col="blue", ylim = c(-1.0,2.0),xlim=c(1980, 2017)) ###Difference plot
    axis(1, seq(1980, 2020, by=5))
    mtext(side=1, "Year", line=2)
    axis(2, seq(-1.5, 2.0, by=0.5))
    mtext(side=2, "RWI", line=2)
    abline(h=1.0, col="black")
    
### The fourth and final graph plots the mean chronologies from both species from each site on the same graph. This allows me to see how 
closely the two species growth patterns were through time.
    
    plot(years_ABBA, b$xxxstd, type='l', axes=F, xlab='', ylab='', main = "ABBA and PCRU", col="blue", ylim = c(0.5,2.0),xlim=c(1980, 2017))  ###Both plotted together
    axis(1, seq(1980, 2020, by=5))
    mtext(side=1, "Year", line=2)
    axis(2, seq(-0.5, 2.0, by=0.5))
    mtext(side=2, "RWI", line=2)
    abline(h=1.0, col="black")
    par(new=TRUE)
    plot(years_PCRU, d$xxxstd, type='l', col="red", axes=F, xlab='', ylab='', ylim = c(0.5,2.0), xlim=c(1980, 2017))
    
    legend(x="topright", legend=c("ABBA", "PCRU"),col= c("blue", "red"), lty=c(1,1), bty="n", xjust=-2, yjust = 4)
  }
}

### At the end of the script, there should be 14 different 4 x 4 figures. Thats's one figure per site, where each species from each site is
plotted in each individual site's figure.
