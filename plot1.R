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

## Create Plot 1
## Histogtram of frequency of Global Active Power (kilowatts)
hist(power.data[,Global_active_power],
     xlab = "Global Active Power (kilowatts)",
     ylab = "Frequency", main = "Global Active Power",
     col = "red")

## Save plot to a png file format
dev.copy(png, file = "plot1.png", width = 480, height = 480, units = "px")
dev.off()

## Alt method without displaying on screen:
##  Open png device; create plot; close device.
## png(file = "plot1.png", width = 480, height = 480, units = "px")
## hist(power.data[,Global_active_power],
##     xlab = "Global Active Power (kilowatts)",
##     ylab = "Frequency", main = "Global Active Power",
##     col = "red")
## dev.off()
