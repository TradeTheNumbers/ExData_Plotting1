
plot4 <- function(){
  
  ## read data in from .txt file
  d <- read.table("./household_power_consumption.txt", header = TRUE, sep = ";") 
  
  ## Merge Date and Time columns into combined column called "DT", while also converting character/factor class 
  ## to Date/Time Class
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
  
  ## Open png graphics device (to allow for width/height control)
  
  png("plot4.png", width = 480, height = 480)
  
  ## Specify structure and positions of 4 charts on one page using mfrow
  
  par(mfrow = c(2,2))
  
  ## Plot1 - Global Active Power
  plot(plot_ready$DT, plot_ready$Global_active_power, type = "n", xlab = "", ylab = "Global Active Power")
  lines(plot_ready$DT, plot_ready$Global_active_power)
  
  ## Plot2 - Voltage
  plot(plot_ready$DT, as.numeric(as.character(plot_ready$Voltage)), type = "n", xlab = "datetime", ylab = "Voltage", yaxt = 
         "n")
  axis(2, at = seq(234,246,4))
  lines(plot_ready$DT, as.numeric(as.character(plot_ready$Voltage)))
  
  ## Plot3 - Sub_metering
  plot(plot_ready$DT, plot_ready$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering", yaxt = "n")
  axis(2, at = seq(0,30,10))
  lines(plot_ready$DT, as.numeric(as.character(plot_ready$Sub_metering_1)), col = "black")
  lines(plot_ready$DT, as.numeric(as.character(plot_ready$Sub_metering_2)), col = "red")
  lines(plot_ready$DT, as.numeric(as.character(plot_ready$Sub_metering_3)), col = "blue")
  
  ## Add and format the legend
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1,1,1), col = c("black", "red", 
                                                                                                               "blue"), bty = "n")
  
  ## Plot4 - Global_reactive_power
  plot(plot_ready$DT, as.numeric(as.character(plot_ready$Global_reactive_power)), type = "n", xlab = "datetime", ylab = 
         "Global_reactive_power", yaxt = "n")
  axis(2, at = seq(0.0, 0.5, 0.1))
  lines(plot_ready$DT, as.numeric(as.character(plot_ready$Global_reactive_power)))
  
  ## Close/end the png graphics device
  
  dev.off()
  
}
