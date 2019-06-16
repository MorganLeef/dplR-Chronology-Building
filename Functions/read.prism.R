#read.prism.R
#Function to read PRISM MONTHLY time series data from a directory
#Rename the col headings, return a list of all prism files in directory
#read.prism()
read.prism <- function(site, tstep) {
    stopifnot(is.character(site), any(tstep == c('monthly','daily')))
    
    data.path <- "Data/climate_data/"
    glob.path <- paste0(data.path, site, "*", tstep, ".csv")   
    
    dataFiles <- lapply(Sys.glob(glob.path), read.csv, skip=11, head=F)
    prism.files <- basename(Sys.glob(glob.path))
    
    climvars <- c("pdate", "ppt", "tmin", "tmean", "tmax")
    dataFiles <- lapply(dataFiles, setNames, climvars)

    
    # remove the date column and save it
    dataFiles.num <- lapply(dataFiles, function(x) { x["pdate"] <- NULL; x })
    
    # Create a mean of the dataFrames list using abind
    dataFiles.mn <- apply(abind::abind(dataFiles.num, along = 3), 1:2, mean, na.rm=T) #returns matrix 

    pgrid.df <- data.frame(dataFiles.mn)
  
    if (tstep=='daily')
            pdate <- as.Date(dataFiles[[1]]$pdate)
   
            pdate <- as.Date(paste0(dataFiles[[1]]$pdate, "-01"))
    
    pgrid.df$pdate <- as.Date(pdate)
    pgrid.df <- pgrid.df[c(5, 1:4)]
    pgrid.output <- list (prism.files=prism.files, dataFiles=dataFiles, pgrid.df=pgrid.df)

}
