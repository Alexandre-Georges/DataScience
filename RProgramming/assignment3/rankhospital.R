rankhospital <- function(state, outcome, num = "best") {
  
  ## definition of useful indexes
  hospitalNameIndex <- 2
  stateIndex <- 7
  heartAttackIndex <- 11
  heartFailureIndex <- 17
  pneumoniaIndex <- 23
  
  ## Reading data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Getting states and rejecting the parameter state if unknown state
  states <- data[, stateIndex]
  if (!state %in% states) {
    stop("invalid state")
  }
  
  ## Filtering unwanted states
  data <- data[data[, stateIndex] == state, ]
  
  ## Getting the outcome column index from the string in parameter
  possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
  outcomeColumnIndexes <- c(heartAttackIndex, heartFailureIndex, pneumoniaIndex)
  if (!outcome %in% possibleOutcomes) {
    stop("invalid outcome")
  }
  outcomeColumnIndex <- outcomeColumnIndexes[match(outcome, possibleOutcomes)]
  
  ## Switching the data type to numeric
  data[, outcomeColumnIndex] <- as.numeric(data[, outcomeColumnIndex])
  
  ## Removing NaN values
  dataNoNan <- data[!is.na(data[outcomeColumnIndex]), ]
  
  ## Returning NA if the demanded rank is bigger than the number of results
  
  rank <- num
  if (num == "best") {
    rank <- 1
  } else if (num == "worst") {
    rank <- nrow(dataNoNan)
  } else  if (rank > nrow(dataNoNan)) {
    return(NA)
  }
  
  ## Getting ordered indexes by the selected outcome and by hospital name
  orderedData <- dataNoNan[order(dataNoNan[, outcomeColumnIndex], dataNoNan[, hospitalNameIndex]), ]
  
  ## Returning the hospital name
  orderedData[rank, hospitalNameIndex]
}