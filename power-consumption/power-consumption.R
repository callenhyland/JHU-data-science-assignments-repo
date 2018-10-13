
## Code for Class Project 1 for Exploratory Data Analysis

library(data.table)
## read in the large data table from text file
dt <- fread("household_power_consumption.txt", sep = ";")
## Y has to be capital to have four digit years
dt$Date <- as.Date(dt$Date, "%d/%m/%Y")
sub <- dt[Date == "2007-02-01" | Date == "2007-02-02"]
rm(dt); ## remove the big data table

## convert the Time column from string to time
sub <- mutate(sub, date_time = paste(Date, Time))
sub$date_time <- ymd_hms(sub$date_time)

## PLOT 1:
## make histogram of Global Active Power
hist(as.numeric(sub$Global_active_power), 
     xlab = "Global Active Power (kilowatts)", 
     ylab = "Frequency", 
     main = "Global Active Power",
     col = "red",
     breaks = 12)

## Plot 2:
## Make line graph of global active power thursday through saturday
## type = "l" makes it a line graph
plot(sub$date_time, sub$Global_active_power,
     xlab = "",
     ylab = "Global Active Power (kilowatts)",
     type = "l")

## Plot 3:
## plot energy submeterings
plot(sub$date_time, sub$Sub_metering_1, 
     type = "l",
     xlab = "",
     ylab = "Energy Sub-Metering",
     col = "black")

# add lines for other sub-meterings
lines(sub$date_time, sub$Sub_metering_2, type = "l", col = "red")
lines(sub$date_time, sub$Sub_metering_3, type = "l", col = "blue")

# add the legend
legend("topright", legend = c("sub-metering-1", 
                              "sub-metering-2", 
                              "sub-metering-3"), 
       col = c("black", "red", "blue"), lwd = 1)


## Plot 4:
## Plot global active power, Voltage, submetering, and global reactive power
par(mfrow = c(2,2), mar = c(5,5,2,3))

# SUBPLOT 1
plot(sub$date_time, sub$Global_active_power,
     xlab = "",
     ylab = "Global Active Power (kilowatts)",
     type = "l",
     col = "black")

# SUBPLOT 2
plot(sub$date_time, sub$Voltage,
     xlab = "",
     ylab = "Voltage (V)",
     type = "l",
     col = "black")

# SUBPLOT 3
plot(sub$date_time, sub$Sub_metering_1, 
     type = "l",
     xlab = "",
     ylab = "Energy Sub-Metering",
     col = "black")

# add lines for other sub-meterings
lines(sub$date_time, sub$Sub_metering_2, type = "l", col = "red")
lines(sub$date_time, sub$Sub_metering_3, type = "l", col = "blue")

# add the legend
legend("topright", legend = c("sub-metering-1", 
                              "sub-metering-2", 
                              "sub-metering-3"), 
       col = c("black", "red", "blue"), lwd = 1)

# SUBPLOT 4
plot(sub$date_time, sub$Global_reactive_power,
     xlab = "",
     ylab = "Global Reactive Power (kilowatts)",
     type = "l",
     col = "black")
