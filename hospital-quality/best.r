best <- function(state, outcome) {
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
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    if (outcome == "heart attack"){
        outcomes[, 11] <- as.numeric(outcomes[, 11])
        st <- outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[outcomes$State == state]
    }
    
    if (outcome == "heart failure"){
        outcomes[, 17] <- as.numeric(outcomes[, 17])
        st <- outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[outcomes$State == state]
    }
    
    if (outcome == "pneumonia"){
        outcomes[, 30] <- as.numeric(outcomes[, 30])
        st <- outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[outcomes$State == state]
    }
    
    hosp <- outcomes$Hospital.Name[outcomes$State == state]
    hospital <- hosp[which.min(st)]
    
    return(hospital)
}