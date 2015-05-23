loadNEI <- function () {
  readRDS('summarySCC_PM25.rds')
}
loadSCC <- function () {
  readRDS('Source_Classification_Code.rds')
}

plot3 <- function () {
  
  library(ggplot2)
  library(dplyr)
  
  if (!exists('NEI')) {
    NEI <- loadNEI()
  }
  if (!exists('SCC')) {
    SCC <- loadSCC()
  }
  
  # There is only one type of pollutant: PM25-PRI / unique(NEI$Pollutant) so no need to filter
  # The data contains only years 1999, 2002, 2005 and 2008 so no need to filter by year as well
  
  # Filtering the data for Baltimore only fips 24510
  baltimore_NEI <- filter(NEI, fips == '24510')
  
  # Grouping the data by year
  NEI_gb_year_type <- group_by(baltimore_NEI, year, type)
  
  # Sum of the emissions by year, emissions are quite high so I divide by 10^3
  NEI_gb_year_type_sum <- dplyr::summarize(NEI_gb_year_type, emissions = sum(Emissions / 1000))
  
  # Switch to factor, year is a factor not a progressive value
  NEI_gb_year_type_sum$year <- factor(NEI_gb_year_type_sum$year)
  
  # PNG device
  png(file = 'plot3.png')
  
  # Plotting of the emissions by year with a bar plot
  p <- ggplot(NEI_gb_year_type_sum, aes(x = year, y = emissions, fill = type)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    labs(x = 'Year', y = expression('Emissions x ' ~ 10^{3}), title = 'Emissions by type in Baltimore', legend.title = 'Type', fill = 'Type')

  print(p)
  
  # Device off
  dev.off()
}