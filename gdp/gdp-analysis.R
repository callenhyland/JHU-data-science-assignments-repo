## Analysis of GDP and education for JHU Getting and Cleaning data, Quiz 3

library(dplyr)

## Load GDP data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url, "gdp.csv")
gdp <- read.csv("/home/callen/Documents/JHU-data-science-assignments/gdp/gdp.csv")

## Load education data
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url2, "education.csv")
edu <- read.csv("education.csv")
rm(url, url2)

## remove extra columns and rename remaining columns from GDP dataset
edu <- tbl_df(edu) 
gdp <- tbl_df(gdp)
gdp <- gdp[5:nrow(gdp),] 
gdp <- select(gdp, CountryCode = X, 
               Rank = Gross.domestic.product.2012, 
               Long.Name = X.2, GDP = X.3)
gdp <- filter(gdp, CountryCode != "", Rank != "")

gdp$GDP <- as.character(gdp$GDP) %>% trimws()
gdp$GDP <- as.numeric(gsub(",","", gdp$GDP))

## merge data sets based on the country code
mg <- merge(gdp, edu, by = "CountryCode") %>% tbl_df
mg <- mutate(mg, gdp_num = as.numeric(as.character(Rank)))
mg <- arrange(mg, desc(gdp_num))

## find average GDP for high income groups
income_groups_gdp <- group_by(mg, Income.Group) %>% summarize(mn = mean(GDP))

## find average rank for high income groups
income_group_rank <- group_by(mg, Income.Group) %>% summarize(mn = mean(gdp_num))


## break the ranking down by quantile, five groups
mg <- mutate(mg, qs = ntile(mg$gdp_num, 5))
quant_table <- table(mg$qs, mg$Income.Group)
