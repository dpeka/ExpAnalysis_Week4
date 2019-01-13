library(tidyverse)

# Read RDS files containing data.
# NEI - PM2.5 emissions, 1999 to 2008
NEI <- readRDS("summarySCC_PM25.rds")
# SCC to interpret codes that map to source of pollutant
SCC <- readRDS("Source_Classification_Code.rds")

#turning off scientific notation
options(scipen=999)

# Q3 - which of the point, nonpoint, onroad, nonroad sources have seen decreases 
# in emissions from 1999–2008 
# for Baltimore City? Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question

# we will use the Baltimore already subsetted dataset and then group by type 
# and year

baltimore <- NEI %>% subset(fips=="24510")

balt_type_year <- baltimore %>% group_by(year, type) %>%
      summarise("tot_emissions" = sum(Emissions))
balt_type_year <- balt_type_year %>% transform(type = as.factor(type))

ggplot(data = balt_type_year, mapping = aes(year, tot_emissions, shape=type)) +
      geom_point(aes(colour=type), size = 5) +
      scale_x_discrete(name ="Year", limits=balt_type_year$year) +
      labs(title = "Total Emissions in Baltimore by Type", 
           y = "Total Emisisons in Tons") +
      theme_bw()
dev.copy(png,'plot3a.png', width = 480, height = 480)
dev.off()

# or we can use facets
ggplot(data = balt_type_year, mapping = aes(year, tot_emissions, shape=type)) +
      geom_point(aes(colour=type), size = 5) +
      scale_x_discrete(name ="Year", limits=balt_type_year$year) +
      labs(title = "Total Emissions in Baltimore by Type", 
           y = "Total Emisisons in Tons") +
      facet_grid(. ~ type) +
      theme_bw()
dev.copy(png,'plot3b.png', width = 480, height = 480)
dev.off()