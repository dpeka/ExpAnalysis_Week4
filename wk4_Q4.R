library(tidyverse)

# Read RDS files containing data.
# NEI - PM2.5 emissions, 1999 to 2008
NEI <- readRDS("summarySCC_PM25.rds")
# SCC to interpret codes that map to source of pollutant
SCC <- readRDS("Source_Classification_Code.rds")

#turning off scientific notation
options(scipen=999)

# Q4 - Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

# We have to look at the SCC file and figure out based on the levels
# where we have coal combustion.
# We will use a regex: [Cc]oal & [Cc]omb
# Note - we get different results if we only look for coal (we get far more
# records).  I could not find clear documentation on knowing what are 
# Combustion / Coal only - so I am going conservative.
exp <- ".*([Cc]oal.*[Cc]omb|[Cc]omb.*[Cc]oal).*"

#grep(exp, SCC$Short.Name)
#grep(".*[Cc]oal.*", SCC$Short.Name)

# let's get the SCC IDs for the Coal Comb combinations
SCC_coal_comb <- as.character(SCC[grepl(exp, SCC$Short.Name, ignore.case = T),1])

#now we subset the NEI dataframe for the records we want
coal_NEI <- NEI %>% subset(SCC %in% SCC_coal_comb)

coal_year_NEI <- coal_NEI %>% group_by(year) %>%
      summarise("tot_emissions" = sum(Emissions))

#plot total emissions from coal combustion for each year.
ggplot(data = coal_year_NEI, mapping = aes(year, tot_emissions)) +
      geom_point(aes(colour=factor(year)), size = 5) +
      scale_x_discrete(name ="Year", limits= coal_year_NEI$year) +
      labs(title = "Total Emissions in USA from Coal Combustion sources", 
           y = "Total Emisisons in Tons") +
      theme_bw()

dev.copy(png,'plot4.png', width = 480, height = 480)
dev.off()

rm(list = ls(all = TRUE))