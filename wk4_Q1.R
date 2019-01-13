library(tidyverse)

# Read RDS files containing data.
# NEI - PM2.5 emissions, 1999 to 2008
NEI <- readRDS("summarySCC_PM25.rds")
# SCC to interpret codes that map to source of pollutant
SCC <- readRDS("Source_Classification_Code.rds")

# total emissions by source for each year.
total_by_year <- NEI %>% group_by(year) %>% 
      summarise("total_emissions" = sum(Emissions))

#turning off scientific notation
options(scipen=999)

# let's plot the data.
# Question #1
with(total_by_year, 
     plot(x = year, y = round(total_emissions/1000000, 1), 
          ylab = "Total Annual Emissions (Millions of Tons)", 
          xlab = "Year",
          main = "Total PM2.5 emission, all sources. Years: 1999, 2002, 2005, 2008",
          pch = 17, col = "red", axes = FALSE, cex=2, lwd=3))
axis(side=1, at=c(1999, 2002, 2005, 2008))
axis(side=2, at=round(total_by_year$total_emissions/1000000, 1))

dev.copy(png,'plot1.png', width = 480, height = 480)
dev.off()

rm(list = ls(all = TRUE))
