best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
  
  ## Check that state and outcome are valid
  stateData <- data[data$State == state, ];
  if (nrow(stateData) == 0) {
    stop('invalid state');
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if (outcome == 'heart attack') {
    dd = stateData[, c(2, 11)];
    dd = dd[complete.cases(dd),];
    if (nrow(dd) > 0) {
      ##return(head(dd[order(dd[, 2], dd[, 1]),]))
      return(dd[order(dd[, 2], dd[, 1]),][1, 1])
    }
  } else if (outcome == 'heart failure') {
    dd = stateData[, c(2, 17)];
    dd = dd[complete.cases(dd),];
    if (nrow(dd) > 0) {
      ##return(head(dd[order(dd[, 2], dd[, 1]),]))
      return(dd[order(as.numeric(dd[, 2]), dd[, 1]),][1, 1])
    }
  } else if (outcome == 'pneumonia') {
    dd = stateData[, c(2, 23)];
    dd = dd[complete.cases(dd),];
    if (nrow(dd) > 0) {
      ##return(dd[order(as.numeric(dd[, 2]), dd[, 1]),])
      return(dd[order(as.numeric(dd[, 2]), dd[, 1]),][1, 1])
    }
  }
  
  stop('invalid outcome');
}
