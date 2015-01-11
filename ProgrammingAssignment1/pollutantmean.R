pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
    
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  files <- list.files(directory, full.names=TRUE)
  data <- numeric()
  
  for (i in id) {
    monitor <- read.csv(files[i])
    data <- c(data,monitor[,pollutant])    
  }
  
  mean_data <- mean(data,na.rm=TRUE);
  
  
}