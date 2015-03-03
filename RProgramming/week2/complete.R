complete <- function(directory, id = 1:332) {
	
	## Intialization of the data matrix
	dfData <- data.frame(id = c(), nobs = c())
	
	## Getting file names from the directory
	vFiles <- dir(directory)
		
	## For each file we get its content and append the pollutant values to the data vector
	for (fileIndex in id) {
		
		## Contanenation of the directory and file name
		sFilePath <- paste(directory, paste(sprintf("%03i", fileIndex), ".csv", sep = ""), sep = "/")
		
		## Reading the file
		mFileData <- read.csv(sFilePath)
		
		## Removing rows with missing data
		nCompleteRows <- nrow(mFileData[!is.na(mFileData$sulfate) & !is.na(mFileData$nitrate),])
		
		## Appending the file result
		dfData <- rbind(dfData, data.frame(id = c(fileIndex), nobs = c(nCompleteRows)))
	}
	
	dfData
}