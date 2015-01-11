complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files <- list.files(directory, full.names=TRUE)
  data <- data.frame(ID=numeric(0), nobs=numeric(0))
  
  for (i in id) {
    data_i <- read.csv(files[i])
    temp <- data_i[!is.na(data_i$nitrate) & !is.na(data_i$sulfate),]
    temp_df <- data.frame(ID=i,nobs=nrow(temp))
    data <- rbind(data,temp_df)
  }
  
  data  
}