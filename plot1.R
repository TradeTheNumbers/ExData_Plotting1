
plot1 <- function(){

## read data in from .txt file
d <- read.table("./household_power_consumption.txt", header = TRUE, sep = ";") 

## Merge Date and Time columns into combined column called "DT", while also converting character/factor class ## to Date/Time 
##Class
together <- as.POSIXct(paste(d$Date, d$Time), format="%d/%m/%Y %H:%M:%S")

## Add new column, "DT", to data table with mutate() fxn.
new_d <- mutate(d, DT = as.POSIXct(paste(d$Date, d$Time), format="%d/%m/%Y %H:%M:%S"))

## Subset data table by target dates (i.e. 2007-02-01 and 2007-02-02)
sub <- new_d[new_d$DT >= as.POSIXct("2007-02-01") & new_d$DT < as.POSIXct("2007-02-03"),]

## Remove rows containing "NA", "<NA>" 
plot_ready <- na.omit(sub)

## Isolate column/variable of interest, i.e. "Global_active_power"
gap <- plot_ready$Global_active_power

## Convert to numeric class for use in histogram
new_gap <- as.numeric(as.vector(gap))

## Plot histogram, set x,y-labels, set color, set main title
hist(new_gap, xlab = "Global Active Power (kilowatts)", ylab = "Frequency", col = "red", main = "Global Active Power") 

## Save to png file
dev.copy(png, file = "plot1.png")
dev.off()

}

