library(LaF)
library(ffbase)
library(plyr)
library(dplyr)

loadData <- function () {
  
  # Getting code/label files for activities and features
  activities <- read.table('UCI HAR Dataset/activity_labels.txt', sep = ' ', col.names = c('activityID', 'activityLabel'), comment.char = '')
  features <- read.table('UCI HAR Dataset/features.txt', sep = ' ', col.names = c('featureID', 'featureLabel'), comment.char = '')
  
  # Getting subject IDs files
  testSubjectIDs <- read.table('UCI HAR Dataset/test/subject_test.txt', sep = ' ', col.names = c('subjectID'), comment.char = '')
  trainSubjectIDs <- read.table('UCI HAR Dataset/train/subject_train.txt', sep = ' ', col.names = c('subjectID'), comment.char = '')
  
  # Same punishment for activities files
  testActivityIDs <- read.table('UCI HAR Dataset/test/y_test.txt', sep = ' ', col.names = c('activityID'), comment.char = '')
  trainActivityIDs <- read.table('UCI HAR Dataset/train/y_train.txt', sep = ' ', col.names = c('activityID'), comment.char = '')
  
  # X_*.txt files are using a fixed width format (16 characters per column and 561 columns of type 'double')
  columnWidths <- rep(16, 561)
  columnClasses <- rep('double', 561)
  
  # Removing () in feature names to have 'prettier' names
  columnNames <- gsub('\\(\\)', '', as.character(features$featureLabel))
  
  # Reading the X_*.txt files with a very fast library and then converting them to a data frame
  testFeaturesLaf <- laf_open_fwf('UCI HAR Dataset/test/X_test.txt', column_widths = columnWidths, column_types = columnClasses, column_names = columnNames, dec = '.')
  testFeatures <- as.data.frame(laf_to_ffdf(testFeaturesLaf))

  trainFeaturesLaf <- laf_open_fwf('UCI HAR Dataset/train/X_train.txt', column_widths = columnWidths, column_types = columnClasses, column_names = columnNames, dec = '.')
  trainFeatures <- as.data.frame(laf_to_ffdf(trainFeaturesLaf))
  
  # Adding the subject IDs and activities ID columns to the data frames
  testFeatures$subjectID <- testSubjectIDs$subjectID
  testFeatures$activityID <- testActivityIDs$activityID
  
  trainFeatures$subjectID <- trainSubjectIDs$subjectID
  trainFeatures$activityID <- trainActivityIDs$activityID
  
  # Appending the training data frame to the testing one resulting into one big data frame that contains both
  allFeatures <- plyr::rbind.fill(testFeatures, trainFeatures)
  
  # Getting indexes of columns that correspond to the mean or std of measurements
  meanStdColumnIndexes <- grep('(mean\\(\\)|std\\(\\))', features$featureLabel)
  
  # We still want to keep the activity IDs and subject IDs so we add the indexes to the previous list
  relevantColumnIndexes <- c(meanStdColumnIndexes, grep('activityID|subjectID', colnames(allFeatures)))
  
  # Keeping only these columns, the rest is removed
  allFeatures <- allFeatures[, relevantColumnIndexes]
  
  # Setting the activity label and removing the activity ID which is now useless
  allFeatures$activityLabel <- activities[allFeatures$activityID, ]$activityLabel
  allFeatures$activityID <- NULL
  
  # Exporting the features
  allFeatures <<- allFeatures
}

featureAvg <- function () {
  
  # Grouping the data by activity label and subject ID
  groupedFeatures <- group_by(allFeatures, activityLabel, subjectID)
  
  # Applying mean on every column of the grouped by data frame
  featuresSummary <<- summarise_each(groupedFeatures, funs(mean))
}

loadData()
featureAvg()

write.table(featuresSummary, file = 'tidy_dataset.txt', row.name = FALSE)