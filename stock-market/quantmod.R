
## load quantimod library
library(quantmod)
library(lubridate)

## get amazon stack quote
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

## how many samples were collected in 2012?
sampleTimes <- ymd(sampleTimes)
sum(year(sampleTimes)==2012)

## how many samples were collected on Mondays in 2012?
sum(year(sampleTimes)==2012 & weekdays(sampleTimes)== "Monday")
