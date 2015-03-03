pollutantmean <- function(directory, pollutant, id = 1:332) {
	
	## Setting the pollutant column index, by default it is sulfate
	nPollutantIndex <- 2
	if (pollutant == "nitrate") {
		nPollutantIndex <- 3
	}
	
	## Data vector initialization, we will append the data later
	vData <- c()
		
	## For each file id we get its content and append the pollutant values to the data vector
	for (fileIndex in id) {
		
		## Contanenation of the directory and file name
		sFilePath <- paste(directory, paste(sprintf("%03i", fileIndex), ".csv", sep = ""), sep = "/")
		
		## Reading the file
		mFileData <- read.csv(sFilePath)
		
		## Removing NA values for the selected pollutant
		mFilteredData <- mFileData[!is.na(mFileData[nPollutantIndex]), ]
		
		## Removing IDs non included in the ID parameter vector
		mFilteredData <- mFilteredData[mFilteredData$ID %in% id, ]
		
		## Appending the new data
		vData <- c(vData, mFilteredData[, nPollutantIndex])
	}
	
	## Calculating the mean value of the pollutant
	meanValue <- mean(vData)
	
	meanValue
}