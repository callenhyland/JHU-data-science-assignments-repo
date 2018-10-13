pollutantmean <- function(directory, pollutant, id = 1:332){
    
    ## air polution data from 332 monitoring stations
    ## variables are the date of observation (YYYY-MM-DD), Sulfate, Nitrate
    ## pollutant tmean takes three arguments:
    ## "directory" is a character vector of length one specifying the path to the *.csv files
    ## "pollutant" is a character vector either "sulfate" or "nitrate"
    ## ID is an integer vector of all of the files to be used   
    
    #list of all files in the directory
    p <- dir(directory)
    #just the files specified in ID
    p_id <- p[id]
    #initialize empty vector
    vec <- vector()
    
    #loop through filenames, read in data
    for(f in p_id){
        #read in csv files as data frames
        d <- read.csv(paste(directory, "/", f, sep = ""))
        #take just the column with the desired pollutant
        pol <- d[,pollutant]
        #append the data to the vector of values
        vec <- c(vec, pol)
    }
    #take mean ignorning na's
    mn <- mean(vec, na.rm = TRUE)
}

