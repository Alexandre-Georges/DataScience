corr <- function(directory, threshold = 0) {
	
	## Initialization of the correlation vector
	vCorr <- c()
	
	## Getting the number of samples per file
	dfComplete <- complete(directory)
	
	## Removing files which do not contain enough samples
	dfFilteredComplete <- dfComplete[dfComplete$nobs > threshold, ]
	
	## Getting their IDs
	vIds <- dfFilteredComplete$id
	
	## For each file we get the correlation and append it to the vector
	for (fileIndex in vIds) {
		
		## Contanenation of the directory and file name
		sFilePath <- paste(directory, paste(sprintf("%03i", fileIndex), ".csv", sep = ""), sep = "/")
		## Reading the file
		mFileData <- read.csv(sFilePath)
		
		## Removing rows with missing data
		mCompleteRows <- mFileData[!is.na(mFileData$sulfate) & !is.na(mFileData$nitrate),]
		
		## Calculating the correlation between the 2 pollutants
		nCorr <- cor(mCompleteRows$sulfate, mCompleteRows$nitrate)
		
		## Appending the file result
		vCorr <- c(vCorr, nCorr)
	}
	vCorr
}