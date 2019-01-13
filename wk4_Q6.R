library(tidyverse)

# Read RDS files containing data.
# NEI - PM2.5 emissions, 1999 to 2008
NEI <- readRDS("summarySCC_PM25.rds")
# SCC to interpret codes that map to source of pollutant
SCC <- readRDS("Source_Classification_Code.rds")

#turning off scientific notation
options(scipen=999)

exp <- ".*([Vv]ehicle.*|[Vv]eh).*"
# subset SCC for vehicle first
veh_emi <- as.character(SCC[grepl(exp, SCC$Short.Name, ignore.case = T),1])

# Q6 - Compare emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, California 
# fips=="06037". Which city has seen greater changes over time in motor 
# vehicle emissions?
balt_la_ve_year <- NEI %>% 
      subset(SCC %in% veh_emi & (fips=="24510" | fips=="06037")) %>%
      group_by(year, fips) %>% 
      summarise("tot_emissions" = sum(Emissions))

balt_la_ve_year$fips <- factor(balt_la_ve_year$fips, 
                               levels = c("24510", "06037"),
                               labels = c("Baltimore", "L.A."))

#plot total emissions from veh em in Baltimore & LA for each year.
ggplot(data = balt_la_ve_year, mapping = aes(year, tot_emissions, shape=fips)) +
      geom_point(aes(colour=fips), size = 5) +
      scale_x_discrete(name ="Year", limits=balt_la_ve_year$year) +
      labs(title = "Total Vehicle Emissions - Baltimore vs. LA", 
           y = "Total Emisisons in Tons") +
      facet_grid(. ~ fips, labeller= label_value) +
      theme_bw() +
      theme(legend.title=element_blank())


dev.copy(png,'plot6.png', width = 480, height = 480)
dev.off()

rm(list = ls(all = TRUE))