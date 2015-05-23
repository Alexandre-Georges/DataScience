loadNEI <- function () {
  readRDS('summarySCC_PM25.rds')
}
loadSCC <- function () {
  readRDS('Source_Classification_Code.rds')
}

plot6 <- function () {
  
  library(ggplot2)
  library(dplyr)
  
  if (!exists('NEI')) {
    NEI <- loadNEI()
  }
  if (!exists('SCC')) {
    SCC <- loadSCC()
  }
  # Joining NEI and SCC
  if (!exists('NEI_SCC')) {
    NEI_SCC <<- join(NEI, SCC, by = c('SCC'), type = 'inner')
  }
  
  # There is only one type of pollutant: PM25-PRI / unique(NEI$Pollutant) so no need to filter
  # The data contains only years 1999, 2002, 2005 and 2008 so no need to filter by year as well
  
  # Filtering the data for road sources in Baltimore and Los Angeles only (fips 24510/06037)
  road_baltimoreLosAngeles_NEI_SCC <- filter(NEI_SCC, Data.Category == 'Onroad' & (fips == '24510' | fips == '06037'))
  
  # Setting the city names
  road_baltimoreLosAngeles_NEI_SCC$fips <- factor(road_baltimoreLosAngeles_NEI_SCC$fips, levels = c('24510', '06037'), labels = c('Baltimore', 'Los Angeles'))
  
  # Grouping the data by year and fips
  NEI_NCC_gb_year_fips <- group_by(road_baltimoreLosAngeles_NEI_SCC, year, fips)
  
  # Sum of the emissions by year and fips, emissions are quite high so I divide by 10^3
  NEI_NCC_gb_year_fips_sum <- dplyr::summarize(NEI_NCC_gb_year_fips, emissions = sum(Emissions / 1000))
  
  # Switch to factor, year is a factor not a progressive value
  NEI_NCC_gb_year_fips_sum$year <- factor(NEI_NCC_gb_year_fips_sum$year)
  
  # PNG device
  png(file = 'plot6.png')
  
  # Plotting of the emissions
  p <- ggplot(NEI_NCC_gb_year_fips_sum, aes(x = year, y = emissions, fill = fips)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    labs(x = 'Year', y = expression('Emissions x ' ~ 10^{3}), title = 'Motor vehicles emissions by year', fill = 'City')
  
  print(p)
  
  # Device off
  dev.off()
}