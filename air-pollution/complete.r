complete <- function(directory, id = 1:332){
    
    ## This function takes a directory filled with csv files 
    ## and returns the number of complete cases in each file
    ## A complete case means that both sulfate and nitrate is observed
    ## can be for a range of monitors specified by id
    ## default is all monitors
    
    # list of all files in the directory
    p <- dir(directory)
    # just the files specified in ID
    p_id <- p[id]
    # empty data frame of to hold results
    m = matrix(ncol = 2, nrow = length(p_id))
    
    # loop through files in subset
    i = 0
    for(f in p_id){
        i = i + 1
        #read in csv files as data frames
        d <- read.csv(paste(directory, "/", f, sep = ""))
        # add ID to first column of fth row
        m[i,1] <- d[1,4]
        
        # loop through row and test whether they are complete
        comp <- 0;
        for(t in 1:nrow(d)){
            if(!is.na(d[t,2]) && !is.na(d[t,3])){
                comp <- comp + 1
            }
        }
        m[i,2] <- comp
    }
    df = as.data.frame(m, colnames = c("ID", "nobs"))
}


