## Code for analyzing air polution from the National Emissions Inventory
## for JHU Data Science exploratory data analysis course
## Callen Hyland, March 14, 2018

library(lattice)
library(ggplot2)
library(dplyr)

## read in source data from 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Address the following questions using a single plot each:

## Question 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

table(NEI$Pollutant)
#All are PM25-PRI, 6497651 entries

quantile(NEI$Emissions, probs = seq(0, 1, 0.1))

boxplot(NEI$Emissions ~ NEI$year, data = NEI, 
        xlab = "Year", 
        ylab = "Emissions (tons)", 
        ylim = c(0,0.7))

## A different view- just plot the median
med <- tapply(NEI$Emissions, as.factor(NEI$year), FUN = median)
plot(names(med), med, 
     ylab = "Median Emissions (tons)",
     xlab = "Year")

## What about the sum of all emissions from each year?
sm <- tapply(NEI$Emissions, as.factor(NEI$year), FUN = sum)
plot(names(sm), sm, 
     ylab = "Total Emissions (tons)",
     xlab = "Year")
## this also shows that total emissions have gone down

## ANSWER: Emissions have gone down from 1999 from 2008


## Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

nei = tbl_df(NEI)
balt <- filter(nei, nei$fips == "24510")

balt_sm <- tapply(balt$Emissions, as.factor(balt$year), FUN = sum)
plot(names(balt_sm), balt_sm, 
     ylab = "Total Emissions (tons)",
     xlab = "Year",
     main = "Total Emissions in Baltimore City (1999-2008)")

## Answer: Over the period of time between 1999 and 2008, emissions have gone down in Baltimore City


## Question 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

point <- tapply(balt$Emissions[balt$type == "POINT"], 
                as.factor(balt$year[balt$type == "POINT"]), sum)
nonpoint <- tapply(balt$Emissions[balt$type == "NONPOINT"], 
                as.factor(balt$year[balt$type == "NONPOINT"]), sum)
onroad <- tapply(balt$Emissions[balt$type == "ON-ROAD"], 
                 as.factor(balt$year[balt$type == "ON-ROAD"]), sum)
nonroad <- tapply(balt$Emissions[balt$type == "NON-ROAD"], 
                as.factor(balt$year[balt$type == "NON-ROAD"]), sum)

em_source <- data.frame(point, nonpoint, onroad, nonroad)
em_source$year <- row.names(em_source)

ggplot(data = em_source) + 
    geom_line(aes(x = em_source$year, y = em_source$point, group = 1, color = "point")) +
    geom_line(aes(x = em_source$year, y = em_source$nonpoint, group = 1, color = "non-point")) +
    geom_line(aes(x = em_source$year, y = em_source$onroad, group = 1, color = "on-road")) +
    geom_line(aes(x = em_source$year, y = em_source$nonroad, group = 1, color = "non-road")) +
    xlab("Year") + ylab("Emissions (tons)") + 
    ggtitle("Emissions in Baltimore City (1999-2008)")

## Answer: point sources have increased, while non-point, on-road, and non-road have decreased


## Question 4: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

codes <- SCC$SCC[grepl("Coal", SCC$EI.Sector, ignore.case = TRUE)]
coal <- filter(nei, is.element(nei$SCC, codes))

coal_sum <- tapply(coal$Emissions, as.factor(coal$year), FUN = sum)
par(mfrow = c(1,1))
plot(names(coal_sum), coal_sum,
     ylab = "Total Emissions (tons)",
     xlab = "Year",
     main = "Emissions from Coal Sources (1999-2008)")

## Answer: Coal emissions have dropped dramatically over the nine year period of the data. Most of the decrease happened between 2005 and 2008.


## Question 5: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

codes <- SCC$SCC[grepl("Vehicle", SCC$EI.Sector, ignore.case = TRUE)]
veh <- filter(balt, is.element(balt$SCC, codes))

veh_sum <- tapply(veh$Emissions, as.factor(veh$year), FUN = sum)
plot(names(veh_sum), veh_sum,
     ylab = "Total Emissions (tons)",
     xlab = "Year",
     main = "Emissions From Vehicle Sources in Baltimore City (1999-2008)")

## Answer: Vehicle emissions in Baltimore City have decreased over the period of 1999-2008, most of the decrease was between 1999 and 2002.


## Question 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

codes <- SCC$SCC[grepl("Vehicle", SCC$EI.Sector, ignore.case = TRUE)]
veh_balt <- filter(balt, is.element(balt$SCC, codes))
veh_sum_balt <- tapply(veh_balt$Emissions, as.factor(veh_balt$year), FUN = sum)

la <- filter(nei, nei$fips == "06037")
veh_la <- filter(la, is.element(la$SCC, codes))
veh_sum_la <- tapply(veh_la$Emissions, as.factor(veh_la$year), FUN = sum)

#put baltimore and la into one dataframe
em_veh <- data.frame(veh_sum_balt, veh_sum_la)
colnames(em_veh) <- c("balt", "la")

#normalize so that both are starting at one
em_veh$balt <- em_veh$balt / em_veh$balt[1]
em_veh$la <- em_veh$la / em_veh$la[1]
em_veh$year <- row.names(em_veh)

ggplot(data = em_veh) +
    geom_line(aes(x = em_veh$year, y = em_veh$balt, group = 1, color = "Baltimore")) +
    geom_line(aes(x = em_veh$year, y = em_veh$la, group = 1, color = "Los Angeles")) +
    xlab("Year") + ylab("Emissions (tons)") + 
    ggtitle("Vehicle Emissions in Baltimore and LA (1999-2008)")

## Answer: Between 1999 and 2008, there has been a greater proportional change in the vehicle emissions in Baltimore City than in Los Angeles.

