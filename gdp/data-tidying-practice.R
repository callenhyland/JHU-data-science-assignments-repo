
library(dplyr)

## load housing data
mydf <- read.csv("housing_data.csv")
hs <- tbl_df(mydf)
rm(mydf)

## split data on string "wgtp
splt <- strsplit(names(hs),"wgtp")
# find the 123rd element of the resulting vector
splt[123]

## load GDP data
setwd("/home/callen/Documents/JHU-data-science-assignments/gdp")
gdp <- read.csv("/home/callen/Documents/JHU-data-science-assignments/gdp/gdp.csv")
## remove the commas from the gdp numbers in millions and average them
gdp <- tbl_df(gdp)
gdp$Gross.domestic.product.2012 <- gdp$Gross.domestic.product.2012 %>% 
    as.character %>% as.numeric
gdp <- filter(gdp, !is.na(gdp$Gross.domestic.product.2012))

gdp$X.3 <- gdp$X.3 %>% as.character %>% trimws
gdp$X.3 <- gsub(",","", gdp$X.3) %>% as.numeric
mean(gdp$X.3)

## load education data
edu <- read.csv("education.csv") %>% tbl_df
## merge with gdp data on the short code
gdp <- rename(gdp, CountryCode = X)
mg <- merge(gdp, edu, by = "CountryCode") %>% tbl_df

## find all countries for the fiscal year ending in June
mg_fisc <- mg[grep("[Ff]iscal [Yy]ear", mg$Special.Notes),]
mg_fisc <- mg_fisc[grep("[Jj]une", mg_fisc$Special.Notes),]
nrow(mg_fisc)
