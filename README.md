# dplR-Chronology-Building and Base R Plotting
##**Proposal**

The purpose of this project is to use the dplR package to construct mean chronologies, and plot them in Rstudio. Andy Bunn's dplR package contains numerous functions and tools that allow analysts to read in tree-ring data in several formats, and reshape them based on the analysts desires. dplR also has its own plotting functions, that can plot everything from skeleton plots to mean chronologies. In this project, I will use dplR to standardize and detrend my tree-ring data, and create mean chronologies of two different tree species. I will then use dplR and base R plotting to create several different types of figures. I will accomplish all of the following in a single nested loop that runs over two lists of my files.

##**Objectives:**

  The loop will accomplish the following in order
    1.)Read in each file 
      There are files for each of two species at each of the 14 sites (28 files total)
    2.)Standardize and detrend each file using the "Mean" method
    3.)Plotting
      There will be four plots, one for an ABBA mean chronology, one for a PCRU chronology, one that plots chronologies for both species on one graph, and finally one that plots a difference index (ABBA index - PCRU index
      
##**Data-Source:**

  I will be using 28 .rwl files (included) that were collected from sites across West Virginia. 
  Each file will have a different number of trees that span different lengths in time.
  They are formatted as site-name-abbreviation_species.rwl (COR_ABBA.rwl for ABBA trees from Courtland Road site)
  
##**Implementation:**

  I have set up a Github repository that contains all the required files and scripts. The included R project file should open with the proper directory where the files and scipts are located. The scipts and data files are stored separately, but the scripts incorperate all the appropiate paths to load in the data when necessary. The main script will contain a loop that reads in the data and creates the plots.
  
##**Products:**

  Once the script's code is complete, there should be 14 sets of 4x4 plots (one 4x4 plot for each site)
