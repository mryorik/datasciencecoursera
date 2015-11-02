rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = 'Not Available');
  
  ## Check that state and outcome are valid
  stateData <- data[data$State == state, ];
  if (nrow(stateData) == 0) {
    stop('invalid state');
  }
  
  if (outcome == 'heart attack') {
    colnum = 11;
  } else if (outcome == 'heart failure') {
    colnum = 17;
  } else if (outcome == 'pneumonia') {
    colnum = 23;
  } else {
    stop('invalid outcome');
  }
  
  dd = stateData[, c(2, colnum)];
  dd = dd[order(dd[,2], dd[,1]),];
  dd = dd[complete.cases(dd),];
  
  if (is.numeric(num)) {
    num = as.numeric(num);
    if (num <= nrow(stateData)) {
      return(dd[num, 1])
    }
  } else if (num == 'best') {
    return(dd[1, 1])
  } else if (num == 'worst') {
    return(dd[nrow(dd), 1])
  }
  
  return(NA);
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}