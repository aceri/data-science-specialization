# ----------------------------
# Downloading the file
# ----------------------------

      # First let's delete everything to avoid troubles
      rm(list=ls())
      # Destination file
      filename="exdata-data-household_power_consumption.zip"
      if(!file.exists(filename)) {
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",filename)
        file <- unzip(filename)
        unlink(filename)
      }

# ----------------------------
# Loading the csv data & getting only the rows we are going to use for speed
# ----------------------------
      
      imported<-read.table("household_power_consumption.txt",sep=";",header=T)
      formatted_date<-as.Date(imported$Date,format="%d/%m/%Y")
      imported$Date<-as.Date(imported$Date,format="%d/%m/%Y")

# ----------------------------
# Preformatting the data
# ----------------------------      
      # In order to work properly we convert to numeric
      # i had to be very careful here because my date is normally written different with month instead of day order
      # i was getting wrong data
      DATA <- imported[(imported$Date=="2007-02-01") | (imported$Date=="2007-02-02"),]
      DATA$Global_active_power <- as.numeric(as.character(DATA$Global_active_power))
      DATA$Global_reactive_power <- as.numeric(as.character(DATA$Global_reactive_power))
      DATA$Voltage <- as.numeric(as.character(DATA$Voltage))
      DATA <- transform(DATA, Datetime=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M")
      DATA$Sub_metering_1 <- as.numeric(as.character(DATA$Sub_metering_1))
      DATA$Sub_metering_2 <- as.numeric(as.character(DATA$Sub_metering_2))
      DATA$Sub_metering_3 <- as.numeric(as.character(DATA$Sub_metering_3))


# ----------------------------
# PLOT2
# ----------------------------      
      # Plot 1 should only have one chart
      par(mfrow=c(1,1))

      
      # Now i have a little problem, that the default date is not in english for me.
      # Not sure if this is acceptable
      
      # YOU MIGHT NOT NEED TO DO THIS. I DO. Notice that it depends on
      # what platform you use. "English" for Windows, "en_US" for mac, etc. 
      # You might want to just remove the line even if you don't have an enlish date in your system
      Sys.setlocale("LC_TIME","English") 

      # Now seems we need plot
      plot(DATA$Datetime,DATA$Global_active_power,type="l",col="black",xlab="",ylab="Global Active Power (kilowatts)")      
      
      #Seems now it is fine even with the english days.

      # Let's save it now to file
      dev.copy(png, file="plot2.png", width=480, height=480)
      dev.off()      
      
      
