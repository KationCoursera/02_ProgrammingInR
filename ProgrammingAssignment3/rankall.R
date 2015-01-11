rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv", na.string = "Not Available")
    
    ## Check that state and outcome are valid
    
    valid_outcome <- sum(outcome == c("heart attack", "heart failure", "pneumonia")) > 0
    
    if(!valid_outcome)
        stop("invalid outcome")
    
    ## For each state, find the hospital of the given rank
    
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
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
    
    result <- data.frame(hospital=character(),
                         state=character())
    
    data_subset_ordered <- data_subset[order(data_subset$State, data_subset[,3], data_subset$Hospital.Name, na.last=TRUE),]    
    states <- levels(data_subset_ordered$State)
    
    for(i in 1:length(states)) {
        data_subset_ordered_state <- data_subset_ordered[data_subset_ordered$State == states[i],]
        data_subset_ordered_state <- data_subset_ordered_state[!is.na(data_subset_ordered_state[,3]),]
        
        if(num == "best") {
            num_i = 1
        } else if (num == "worst") {
            num_i = length(data_subset_ordered_state[,1])
        } 
               
        new_entry <- data.frame(hospital=as.character(data_subset_ordered_state[num_i,1]), state=as.character(states[i]))
        
        result <- rbind(result, new_entry)
    }
    
    result
    
}
