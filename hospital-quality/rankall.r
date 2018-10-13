rankall <- function(outcome, num){
    
    ## Take input of outcome = a specific outcome, 30 day mortality statistic
    ## for either heart attach, heart failure, or pneumonia, and the rank to report
    
    ## Read outcome data
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    conditions = c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if (!any(outcomes[7] ==state)){ stop("invalid state") }
    if (!any(conditions == outcome)){ stop("invalid outcome") }
    
    if (outcome == "heart attack"){ col_num = 11 }
    if (outcome == "heart failure"){ col_num = 17 }
    if (outcome == "pneumonia"){ col_num = 30 } 
    
    ## For each state, find the hospital of the given rank
    outcomes[, col_num] <- as.numeric(outcomes[, col_num])
    states_list <- unique(outcomes$State)
    names_list <- vector(mode = "character", length = length(states_list))
    
    for (state in states_list) {
        st <- outcomes[outcomes$State == state, col_num]
        hosp <- outcomes$Hospital.Name[outcomes$State == state]
        
        ## find the hospital with the rank that the user has requested
        # if the user specified best
        if (num == "best"){
            hosp_rank <- order(st, hosp, na.last = TRUE)
            hosp_num <- hosp[hosp_rank[1]]
        }
        #if the user has specified worst
        else if (num == "worst"){
            hosp_rank <- order(st, hosp, na.last = TRUE, decreasing = TRUE)
            hosp_num <- hosp[hosp_rank[1]]
        }
        #if the rank is larger than the number of non-NA scores
        else if (num > sum(!is.na(st))){
            hosp_num <- NA
        }
        else {
            hosp_rank <- order(st, hosp, na.last = TRUE)
            hosp_num <- hosp[hosp_rank[num]]
        }
        
        ## add the hospital name into names vector
        names_list[which(df$State == state)] <- hosp_num
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    df <- data.frame(names_list, states_list)
    colnames(df) <- c("Hospital.Name", "State")
    return(df)
}