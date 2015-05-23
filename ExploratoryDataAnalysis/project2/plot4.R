loadNEI <- function () {
  readRDS('summarySCC_PM25.rds')
}
loadSCC <- function () {
  readRDS('Source_Classification_Code.rds')
}

plot4 <- function() {
  
  library(ggplot2)
  library(plyr)
  
  if (!exists('NEI')) {
    NEI <<- loadNEI()
  }
  if (!exists('SCC')) {
    SCC <<- loadSCC()
  }
  # Joining NEI and SCC
  if (!exists('NEI_SCC')) {
    NEI_SCC <<- join(NEI, SCC, by = c('SCC'), type = 'inner')
  }
  
  # There is only one type of pollutant: PM25-PRI / unique(NEI$Pollutant) so no need to filter
  # The data contains only years 1999, 2002, 2005 and 2008 so no need to filter by year as well
  
  # Filtering the data for coal combustion
  coalCombustion_NEI <- filter(NEI_SCC,
    EI.Sector == 'Fuel Comb - Electric Generation - Coal'
    | EI.Sector == 'Fuel Comb - Industrial Boilers, ICEs - Coal'
    | EI.Sector == 'Fuel Comb - Comm/Institutional - Coal')
  
  # Grouping the data by year
  NEI_gb_year <- group_by(coalCombustion_NEI, year)
  
  # Sum of the emissions by year, emissions are quite high so I divide by 10^5
  NEI_gb_year_sum <- dplyr::summarize(NEI_gb_year, emissions = sum(Emissions / 100000))
  
  # Switch to factor, year is a factor not a progressive value
  NEI_gb_year_sum$year <- factor(NEI_gb_year_sum$year)
  
  # PNG device
  png(file = 'plot4.png')
  
  # Plotting of the emissions
  p <- ggplot(NEI_gb_year_sum, aes(x = year, y = emissions)) +
    geom_bar(stat = 'identity', position = 'dodge', fill = 'red') +
    labs(x = 'Year', y = expression('Emissions x ' ~ 10^{5}), title = 'Coal combustion emissions')
  
  print(p)
  
  # Device off
  dev.off()
}