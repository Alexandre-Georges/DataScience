# Plots the data as a histogram
# For the development part, it was quicker to compute the data once and
# send it to the plot function instead of reloading the data every time
# so there is an optional parameter we can use as a shortcut
# to give the data to the plotting function
plot4 <- function (data = NULL, filePath = NULL) {
  
  # If the data is not provided, we will read it
  if (is.null(data)) {
    print('loading data')
    
    if (!is.null(filePath)) {
      data <- getData(filePath)
    } else {
      data <- getData()
    }
  }
  
  # Activation of the device
  png(file = 'plot4.png', width = 480, height = 480)
  
  # Drawing graphs
  par(mfrow = c(2, 2))
  plot(data$DateTime, data$Global_active_power, type = 'l', xlab = '', ylab = 'Global Active Power')
  plot(data$DateTime, data$Voltage, type = 'l', xlab = 'datetime', ylab = 'Voltage')
  plot(data$DateTime, data$Sub_metering_1, type = 'l', xlab = '', ylab = 'Energy sub metering', col = 'black')
  lines(data$DateTime, data$Sub_metering_2, type = 'l', col = 'red')
  lines(data$DateTime, data$Sub_metering_3, type = 'l', col = 'blue')
  legend('topright', pch = NA, lty = 1, col = c('black', 'red', 'blue'), legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), bty = 'n')
  plot(data$DateTime, data$Global_reactive_power, type = 'l', xlab = 'datetime', ylab = 'Global_reactive_power')
  
  # Closing it
  dev.off()
}

# Loads the data, filters it by date and convert dates/times to R formats
getData <- function (filePath = 'household_power_consumption.txt') {
  
  # Reading first lines to determine types
  initial <- read.table(filePath, sep = ';', header = TRUE, nrows = 10)
  
  # Determining classes for each column
  classes <- sapply(initial, class)
  
  # Reading the data file using classes found just above, '?' values will be converted to NA
  data <- read.table(filePath, sep = ';', header = TRUE, colClasses = classes, na.strings = c('?'))
  
  # Converting the Date column to R compatible date format
  data[, 'Date'] <- as.Date(data[, 'Date'], '%d/%m/%Y')
  
  # Filtering the data for the dates we are interested in
  filteredData <- data[data$Date >= '2007-02-01' & data$Date <= '2007-02-02', ]
  
  # Converting observation time and date to a POSIX date and time column
  filteredData[, 'DateTime'] <- as.POSIXct(paste(filteredData[, 'Date'], filteredData[, 'Time']), format = '%Y-%m-%d %H:%M:%S')
  
  # Removing the now useless columns containing date and time
  filteredData <- filteredData[, c(-1, -2)]
  
  filteredData
}