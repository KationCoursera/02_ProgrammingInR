best <- function(state, outcome) {
    
    ## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    
    valid_outcome <- sum(outcome == c("heart attack", "heart failure", "pneumonia")) > 0
    
    if(!valid_outcome)
        stop("invalid outcome")
    
    valid_state <- sum(state == levels(factor(data$State))) > 0
    
    if(!valid_state)
        stop("invalid state")
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    if (outcome == "heart attack") {
        
        message("heart attack")

        data_subset <- subset(data, State == state)
        data_v <- data_subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        data_v <- as.numeric(levels(data_v))[data_v]
        indices <- which(data_v == min(data_v,na.rm = TRUE))
        
        best_hospital <- as.character(data_subset$Hospital.Name[indices])
        
    } else if (outcome == "heart failure") {
        
        message("heart failure")
        
        data_subset <- subset(data, State == state)
        data_v <- data_subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        data_v <- as.numeric(levels(data_v))[data_v]
        indices <- which(data_v == min(data_v,na.rm = TRUE))
        
        best_hospital <- as.character(data_subset$Hospital.Name[indices])
        
    } else {
        
        message("pneumonia")
        
        data_subset <- subset(data, State == state)
        data_v <- data_subset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        data_v <- as.numeric(levels(data_v))[data_v]
        indices <- which(data_v == min(data_v,na.rm = TRUE))
        
        best_hospital <- as.character(data_subset$Hospital.Name[indices])
        
    }
    
    #best_hospital <- sort(best_hospital)
    
    best_hospital
    
    
    
}

