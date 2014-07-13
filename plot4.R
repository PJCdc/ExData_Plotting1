library(data.table)
cp01.wd <- getwd()
fname <- "household_power_consumption.txt"
fimport <- paste(cp01.wd, "//", fname, sep = "")

## Use the fread function from the data.table library to decrease time
## to import into memory the raw data file.
power.data.raw <- fread(fimport, sep = ";", header = TRUE,
                        colClasses = c("character", "character","numeric",
                                       "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "numeric"),
                        na.strings = c("NA", "?"), stringsAsFactors = FALSE)

## Extract the rows for only Feb 1, 2007 and Feb 2, 2007
power.data <- power.data.raw[Date == "1/2/2007" | Date == "2/2/2007"]

## Clean up imported data:
##  Convert Date column to Date class; Time column to POSIXct Date-Time class;
##  Columns 3 to 9 to numeric class.
dt.c <- paste(power.data$Date,power.data$Time)
dt.lv <- as.POSIXlt(dt.c, format = "%d/%m/%Y %H:%M:%S")
dt.cv <- as.POSIXct(dt.c, format = "%d/%m/%Y %H:%M:%S")

power.data[,Date := as.Date(dt.cv)]
power.data[,Time := dt.cv]

for (i in 3:ncol(power.data)) {
    power.data[[i]] <- as.numeric(power.data[[i]])
}
## Save "cleaned" data table as R object.
save(power.data, file = "powerData.RDA")


## Create Plot 4
op <- par(mfcol = c(2,2), mex = 0.7, cex = 0.85) ##, mar = c(5.1,4.1,4.1,2.1))
cex.lab.val <- 0.75
cex.axis.val <- 0.60
## PLot r1-c1
## Line graph of Global Active Power(kilowatts) v TImeDate
plot(power.data[,Time], power.data[,Global_active_power],
     type = "l", xlab = "", ylab = "Global Active Power (kilowatt)",
     cex.lab = cex.lab.val, cex.axis = cex.axis.val)

## PLot r2-c1
## Line chart of Energy v TimeDate for 
## series Sub_metering_1, Sub_metering_2, Sub_metering_3
plot(power.data[,Time], power.data[,Sub_metering_1],
     type = "l", col = "black", xlab = "", ylab = "Energy sub metering",
     cex.lab = cex.lab.val, cex.axis = cex.axis.val)
lines(power.data[,Time], power.data[,Sub_metering_2], col = "red")
lines(power.data[,Time], power.data[,Sub_metering_3], col = "blue")
legend("topright", lty = 1, bty = "n", cex = 0.65,
       col = c("black", "blue", "red"), xjust = 0,
       legend = c("Sub_metering_1   ", "Sub_metering_2   ", "Sub_metering_3   "))


## PLot r1-c2
## Line of Voltage v TimeDate (panel r2-c1 in plot 4)
plot(power.data[,Time], power.data[,Voltage],
     type = "l", xlab = "datetime",ylab = "Voltage",
     cex.lab = cex.lab.val, cex.axis = cex.axis.val)

## PLot r2-c2
## Line chart of Global_reactive_power v TimeDate (panel r2-c2 in plot 4)
plot(power.data[,Time], power.data[,Global_reactive_power], type = "l",
     xlab = "datetime",
     ylab = names(power.data)[4],
     cex.lab = cex.lab.val, cex.axis = cex.axis.val)

par(op)

## dev.copy(png, file = "plot4.png")
## Save plot to a png file format
dev.copy(png, file = "plot4.png", width = 480, height = 480, units = "px")
dev.off()


## Alt method without displaying on screen:
##  Open png device; create plot; close device.
## png(file = "plot3.png", width = 480, height = 480, units = "px",
##      pointsize = 6)
## hist(power.data[,Global_active_power],
##     xlab = "Global Active Power (kilowatts)",
##     ylab = "Frequency", main = "Global Active Power",
##     col = "red")
## dev.off()
