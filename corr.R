corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  dfc = complete(directory);
  ids = dfc[dfc$nobs > threshold,]$id;
  
  cors <- c();
  for (id in ids) {
    csv <- read.csv(paste0(directory, "/", formatC(id, width = 3, flag = "0"), ".csv"));
    cors = c(cors, cor(csv[complete.cases(csv),]$sulfate, csv[complete.cases(csv),]$nitrate))
  }
  
  if (length(ids) > 0) {
    cors
  } else {
    numeric(0)
  }
  
}