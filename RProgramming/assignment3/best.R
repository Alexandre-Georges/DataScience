best <- function(state, outcome) {
  
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
  
  ## Finding the min value index and the min value for this outcome
  minIndex <- which.min(dataNoNan[, outcomeColumnIndex])
  minValue <- dataNoNan[minIndex, outcomeColumnIndex]
  
  ## Getting all rows with that min value
  rowsWithMinValue <- dataNoNan[dataNoNan[outcomeColumnIndex] == minValue, ]
  
  ## Returning the hospital name
  rowsWithMinValue[order(rowsWithMinValue[, hospitalNameIndex]), hospitalNameIndex][1]
}