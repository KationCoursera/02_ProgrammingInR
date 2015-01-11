rankhospital <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv", na.string = "Not Available")
    
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
        data_subset <- data[,c(2,7,11)]        
        
    } else if (outcome == "heart failure") {
        
        message("heart failure")
        data_subset <- data[,c(2,7,17)]
        
    } else {
        message("pneumonia")
        data_subset <- data[,c(2,7,23)]
        
    }
    
    data_subset_ordered <- data_subset[order(data_subset$State, data_subset[,3], data_subset$Hospital.Name, na.last=TRUE),]
    data_subset_ordered <- data_subset_ordered[data_subset_ordered$State == state,]
    data_subset_ordered <- data_subset_ordered[!is.na(data_subset_ordered[,3]),]
    
    
    if(num == "best") {
        num = 1
    } else if (num == "worst") {
        num = length(data_subset_ordered[,1])
    }
    
    as.character(data_subset_ordered[num,1])
    
}
