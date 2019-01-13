library(tidyverse)

# Read RDS files containing data.
# NEI - PM2.5 emissions, 1999 to 2008
NEI <- readRDS("summarySCC_PM25.rds")
# SCC to interpret codes that map to source of pollutant
SCC <- readRDS("Source_Classification_Code.rds")

#turning off scientific notation
options(scipen=999)

# Q2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# fips=="24510" from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.
baltimore <- NEI %>% subset(fips=="24510")
baltimore_year <- baltimore %>% group_by(year) %>%
      summarise("total_emissions" = sum(Emissions))

#Plotting the data for Baltimore
with(baltimore_year, 
     plot(x = year, y = round(total_emissions/1000, 1), 
          ylab = "Annual Emissions (Thousands of Tons)", 
          xlab = "Year",
          main = "Baltimore PM2.5 emission, all sources. Years: 1999, 2002, 2005, 2008",
          pch = 17, col = "purple", axes = FALSE, cex=2, lwd=3))
axis(side=1, at=c(1999, 2002, 2005, 2008))
axis(side=2, at=round(baltimore_year$total_emissions/1000, 1))

dev.copy(png,'plot2.png', width = 480, height = 480)
dev.off()

rm(list = ls(all = TRUE))