corr <- function(directory, threshold = 0){
    
    ## find correction between nitrate and sulfate values for all monitors
    ## that are over a threshold number of complete cases
    
    # list of all files in the directory
    p <- dir(directory)
    #empty matrix to hold correlation coefficients
    ids <- vector()
    corrs <- vector()
    
    for(f in p){
        #read in csv files as data frames
        d <- read.csv(paste(directory, "/", f, sep = ""))

        # here's a faster way to do it
        comp <- sum(!is.na(d[,2] * d[,3]))
        
        # if the number of complete cases is over the threshold
        if(comp > threshold){
            #calculate the correlation between nitrate and sulfate
            coef <- cor(d[,2], d[,3], use = "pairwise.complete.obs")
            ids <- c(ids, d[1,4])
            corrs <- c(corrs, coef)
        }
    }
    m = cbind(ids, corrs)
    df = as.data.frame(m, colnames = c("ID", "corr_coef"))
}