
plot2 <- function(){
  
  ## read data in from .txt file
  d <- read.table("./household_power_consumption.txt", header = TRUE, sep = ";") 
  
  ## Merge Date and Time columns into combined column called "DT", while also converting character/factor class 
  ## to Date/Time 
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
  
  ## Open png graphics device (to allow for width/height control)
  
  png("plot2.png", width = 480, height = 480)
  
  ## Plot line graph of (x= Time) versus (y= Global_active_power) 
  plot(plot_ready$DT, as.numeric(as.character(plot_ready$Global_active_power)), type = "n", xlab = "", ylab ="Global Active Power (kilowatts)")
  lines(plot_ready$DT, as.numeric(as.character(plot_ready$Global_active_power)))
  
  ## Close/end the png graphics device
  
  dev.off()
  
}

