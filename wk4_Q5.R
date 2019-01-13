library(tidyverse)

# Read RDS files containing data.
# NEI - PM2.5 emissions, 1999 to 2008
NEI <- readRDS("summarySCC_PM25.rds")
# SCC to interpret codes that map to source of pollutant
SCC <- readRDS("Source_Classification_Code.rds")

#turning off scientific notation
options(scipen=999)

# Q5 - How have emissions from motor vehicle sources changed from 1999–2008 
# in Baltimore City?
# We have to look at the SCC file and figure out based on the levels
# where we have vehicle as a source.
# We will use a regex: [Vv]ehicle or [Vv]eh.
# Note - I am looking at SCC short name as it has the most "hits" for
# vehicle.  Looking at the other fields, like EI.Sector would return a smaller
# number.
exp <- ".*([Vv]ehicle.*|[Vv]eh).*"

# subset SCC for vehicle first
veh_emi <- as.character(SCC[grepl(exp, SCC$Short.Name, ignore.case = T),1])

balt_ve_year <- NEI %>% subset(SCC %in% veh_emi & fips=="24510") %>%
      group_by(year, fips) %>% 
      summarise("tot_emissions" = sum(Emissions))
#plot total emissions from veh em in Baltimore for each year.
ggplot(data = balt_ve_year, mapping = aes(year, tot_emissions)) +
      geom_point(aes(colour=factor(year)), size = 5) +
      scale_x_discrete(name ="Year", limits= balt_ve_year$year) +
      labs(title = "Total Vehicle Emissions in Baltimore", 
           y = "Total Emisisons in Tons") +
      theme_bw()

dev.copy(png,'plot5.png', width = 480, height = 480)
dev.off()

rm(list = ls(all = TRUE))
