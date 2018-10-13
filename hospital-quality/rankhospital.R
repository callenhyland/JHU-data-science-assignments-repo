rankhospital <- function(state, outcome, num) {
    
    ## Report hospital in a specified state having rank num in a given outcome
    
    ## Read outcome data
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    conditions = c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if (!any(outcomes[7]==state)){
        stop("invalid state")
    }
    
    if (!any(conditions == outcome)){
        stop("invalid outcome")
    }
    
    if (outcome == "heart attack"){
        outcomes[, 11] <- as.numeric(outcomes[, 11])
        st <- outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[outcomes$State == state]
    }    

    if (outcome == "heart failure"){
        outcomes[, 17] <- as.numeric(outcomes[, 17])
        st <- outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[outcomes$State == state]
    }  
    
    if (outcome == "pneumonia"){
        outcomes[, 30] <- as.numeric(outcomes[, 30])
        st <- outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[outcomes$State == state]
    }  
    
    hosp <- outcomes$Hospital.Name[outcomes$State == state]
    
    if (num == "best"){
        hosp_rank <- order(st, hosp, na.last = TRUE)
        out <- hosp[hosp_rank[1]]
    }
    else if (num == "worst"){
        hosp_rank <- order(st, hosp, na.last = TRUE, decreasing = TRUE)
        out <- hosp[hosp_rank[1]]
    }
    else if (num > sum(!is.na(st))){
        out <- NA
    }
    else {
        hosp_rank <- order(st, hosp, na.last = TRUE)
        out <- hosp[hosp_rank[num]]
    }
    
    return(out)
}