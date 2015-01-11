corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ## Find complete data sets
  
  files <- list.files(directory, full.names=TRUE)
  init <- read.csv(files[1])
  cr <- numeric(0)
  
  for (i in files) {
    data_i <- read.csv(i)
    data_complete_i <- data_i[!is.na(data_i$nitrate) & !is.na(data_i$sulfate),]
    if (nrow(data_complete_i) > threshold) {
      cr <- c(cr,cor(data_complete_i$sulfate,data_complete_i$nitrate))
    }
  }
  
  cr
  
}