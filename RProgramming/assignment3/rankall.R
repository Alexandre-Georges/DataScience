rankall <- function(outcome, num = "best") {
  
  ## definition of useful indexes
  hospitalNameIndex <- 2
  stateIndex <- 7
  heartAttackIndex <- 11
  heartFailureIndex <- 17
  pneumoniaIndex <- 23
  
  ## Reading data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
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
  
  ## Getting ordered indexes by the selected outcome and by hospital name
  orderedData <- dataNoNan[order(dataNoNan[, outcomeColumnIndex], dataNoNan[, hospitalNameIndex]), ]
  
  ## Creating the result data frame
  result <- data.frame()
  
  ## Getting unique states by alphabetical order
  states <- unique(data[, stateIndex])
  states <- states[order(states)]
  
  ## Getting the result for each state
  for (state in states) {
    
    ## Setting the hospital name to NA if not found
    hostpitalName <- NA
    
    ## Filtering the data for the current state
    dataForState <- orderedData[orderedData[, stateIndex] == state, ]
    
    ## Getting the required rank
    rank <- num
    if (num == "best") {
      rank <- 1
    } else  if (num == "worst") {
      rank <- nrow(dataForState)
    }
    
    ## If the rank is an acceptable index we get its name
    if (nrow(dataForState) >= rank) {
      hostpitalName <- dataForState[rank, hospitalNameIndex]
    }
    
    ## Appending the result
    result <- rbind(result, data.frame(hospital = hostpitalName, state = state))
  }
  
  result
}