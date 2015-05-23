loadNEI <- function () {
  readRDS('summarySCC_PM25.rds')
}
loadSCC <- function () {
  readRDS('Source_Classification_Code.rds')
}

plot1 <- function () {
  
  library(dplyr)
  
  if (!exists('NEI')) {
    NEI <<- loadNEI()
  }
  if (!exists('SCC')) {
    SCC <<- loadSCC()
  }
  
  # There is only one type of pollutant: PM25-PRI / unique(NEI$Pollutant) so no need to filter
  # The data contains only years 1999, 2002, 2005 and 2008 so no need to filter by year as well
  
  # Grouping the data by year
  NEI_gb_year <- group_by(NEI, year)
  
  # Sum of the emissions by year, emissions are quite high so I divide by 10^6
  NEI_gb_year_sum <- dplyr::summarize(NEI_gb_year, emissions = sum(Emissions / 1000000))
  
  # PNG device
  png(file = 'plot1.png')
  
  # Plotting of the emissions by year with a bar plot
  barplot(NEI_gb_year_sum$emissions, names.arg = as.character(NEI_gb_year_sum$year), col = 'red', xlab = 'Year', ylab = expression('Emissions x ' ~ 10^{6}), main = 'Emissions by year')
  
  # Device off
  dev.off()
}