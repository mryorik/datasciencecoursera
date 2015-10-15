pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV filesa
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  means <- c();
  for (i in id) {
    csv <- read.csv(paste0(directory, "/", formatC(i, width = 3, flag = "0"), ".csv"));
    csv[[pollutant]][is.na(csv[[pollutant]])] <- 0;
    means <- c(means, mean(csv[[pollutant]]));
  }
  mean(means)
}