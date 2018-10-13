## Questions for Week 3 quiz from JHU getting and cleaning data

library(jpeg)
library(dplyr)

## find all of the homes in Idaho that have over ten acres and produce > $10,000
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
file <- download.file(url, "housing_data.csv")
mydf <- read.csv("housing_data.csv")

## turn a data frame into dataframe tibble
hs <- tbl_df(mydf)
rm(mydf); rm(file)

# add logical vector for houses on greater than ten acres
hs <-mutate(hs, ten_acres = hs$ACR == 3)
# add logical vector for houses that sold more than $10,000
hs <- mutate(hs, bigbux = hs$AGS == 6)
# add another row that is all of the houses with BOTH
hs <- mutate(hs, bignrich = ten_acres & bigbux)
# find the first three rows of the resulting logical vector
first_three <- head(which(hs$bignrich),3)


## Read a JPEG image from the web
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(url, "jeff.jpg")
jeff <- readJPEG("jeff.jpg", native = TRUE)

#reshape into a one-dimensional vector
jeff_vect <- as.vector(jeff)

## find the 30th and 80th quantiles
q30 <- quantile(jeff_vect, probs = 0.30)
q80 <- quantile(jeff_vect, probs = 0.80)
