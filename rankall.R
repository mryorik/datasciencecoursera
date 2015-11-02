rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = 'Not Available', stringsAsFactors = FALSE);
  
  ## Check that state and outcome are valid
  if (outcome == 'heart attack') {
    colnum = 11;
  } else if (outcome == 'heart failure') {
    colnum = 17;
  } else if (outcome == 'pneumonia') {
    colnum = 23;
  } else {
    stop('invalid outcome');
  }
  
  ## For each state, find the hospital of the given rank
  dd = data[, c(2, 7, colnum)];
  dd = dd[complete.cases(dd),];
  X = split(dd, dd[, 2]);
  Y = data.frame(hospital = character(0), state = character(0));
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  for (i in seq_along(X)) {
    ddd <- X[[i]];
    ddd <- ddd[order(as.numeric(ddd[, 3]), ddd[, 1]),];
    if (num == 'best') {
      n = 1;
    } else if (num == 'worst') {
      n = nrow(ddd);
    } else {
      n = as.numeric(num);
    }
    Y = rbind(Y, data.frame(hospital = ddd[n,1], state = ddd[1,2]));
  }
  row.names(Y) <- Y[, 2];
  Y
}
