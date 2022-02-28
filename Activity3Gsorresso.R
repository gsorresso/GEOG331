##Activity 3 Gabrielle Sorresso
##GS 2/21/2022
##set working directory 
setwd("Z://students/gsorresso/")

#Create Own Function Example
#Name of arguments is in (), everything in {} is run each time funct is run
assert <- function(statement, err.message, allgood.message){
  if(statement == FALSE){
    print(err.message)  
  }
  if(statement == TRUE){
    print(allgood.message)  
  }
}

#check how statement works by evaluating a false statement
assert(1==2, "error: unequal values", "equal values")
#evaluate a true statement
assert(2==2, "error: unequal values", "equal values")
#set assert to check if two vectors are the same length
a <- c(1, 2, 3, 4)
b <- c(8, 4, 5)
assert(length(a) == length(b), "different lengths", "same length")


##read in Bewkes Weather Data & install lubridate package  
##skip first three rows since there is additional column inf0 
##specify NA is designated differently 
datW <- read.csv("datafolder/bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header = FALSE)

##preview data 
print(datW[1,])

##get sensor info from files (data contains all relevant units)
sensorInfo <- read.csv("datafolder/bewkes_weather.csv", na.strings=c("#N/A"), nrows = 2)
print(sensorInfo)

#get column names from sensorInfo Table and set weather station colnames to be the same 
colnames(datW) <- colnames(sensorInfo)
#preview data 
print(datW[1,])

##install lubridate package with install.package 
#install.packages(c("lubridate"))
library(lubridate)

##convert dates to standardize formats - date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz = "America/New_York")
##calculate day of year 
datW$doy <- yday(dates)
#calculate hour in the day 
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year 
datW$DD <- datW$doy + (datW$hour/24)
#preview new date calculations 
datW[1,]


##Check for Missing Values 
#air temp 
length(which(is.na(datW$air.temperature)))

#wind speed 
length(which(is.na(datW$wind.speed)))

#precipitation
length(which(is.na(datW$precipitation)))

#soil temp
length(which(is.na(datW$soil.temp)))

#soil moisture
length(which(is.na(datW$soil.moisture)))

##More QA/QC
##make a plot with filled in points for soil moisture (using pch)
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year", 
     ylab = "Soil Moisture (cm3 water per cm3 soil)")

##make a plot with filled in points for air temp (using pch)
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year", 
     ylab = "Air Temp (Degrees C)")

##create new column for QA/QC with air temp using ifelse funct 
###In the ifelse funct argument is logical statement to be evaluated as true or false 
###second argument is value that new column will be given if statement is true 
###third argument is value new column will be given if statement is false 
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

##check extreme ranges of values and throughout percentiles for air temp
quantile(datW$air.temperature)

##look at days with really low air temp 
datW[datW$air.tempQ1 < 8,]
##look at days with really high air temp 
datW[datW$air.tempQ1 > 33,]

##plot precipitation and lightening strikes on the same plot 
###normalize lightening strikes to match precipitation 
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
##make plot with precipitation and lightning activity marked 
###start with empty and add features 
plot(datW$DD, datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & Lightening",
     type = "n")

##plot precipitation points only when there is precipitation & make points semi transparent 
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0], 
       col = rgb(95/255, 158/255, 160/255, .5), pch = 15)

##plot lightning points only when there is lightning 
points(datW$DD[lightscale > 0], lightscale[lightscale > 0 ],
       col = "tomato3", pch = 19)

##question 5
##use the assert function to show that the lightscale data frame has the same vector length as the percipitation vector in datW
assert(length(lightscale) == length(datW$precipitation), "different lengths", "same length")

##filter out storms in wind and air temp measurements
###filter all values with lightning that coincide with rainfall greater than 2mm or only rainfall over 5mm
###also create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy > 0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

##question 6
##check extreme ranges of values and throughout percentiles for wind speed
quantile(datW$wind.speed)
###filter out suspect wind measurements (when there are storms) and replace with NA
datW$wind.speedQ1 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy > 0, NA,
                            ifelse(datW$precipitation > 5, NA, datW$wind.speed))

##check that when there is lightening wind is set to NA 
assert(datW$lightning.acvitivy )

##plot new wind speed vector
plot(datW$DD, datW$wind.speedQ1, type = "o", xlab = "Day of Year", ylab = "Wind Speed (m/s)",
     main = "Wind Speed")



##question 7
##check that soil measurements are reliable in days leading up to outage
plot(x = datW$DD, y = datW$soil.moisture, xlab = "Day of Year", ylab = "Precipitation & Soil Moisture")

#question 8
#create a vector of the avg values of air temp, wind speed, soil moisture, and soil temp and the total obs for each
average.values.and.obs <- c(mean(datW$air.temperature, na.rm=TRUE),
                                 mean(datW$wind.speedQ1, na.rm = TRUE),
                                 mean(datW$soil.moisture, na.rm = TRUE),
                                 mean(datW$soil.temp, na.rm = TRUE),
                                 sum(datW$precipitation, na.rm = TRUE),
                                 length(datW$air.temperature[!is.na(datW$air.temperature)]),
                                 length(datW$wind.speedQ1[!is.na(datW$wind.speedQ1)]),
                                 length(datW$soil.moisture[!is.na(datW$soil.moisture)]),
                                 length(datW$soil.temp[!is.na(datW$soil.temp)]),
                                 length(datW$precipitation[!is.na(datW$precipitation)]))
#create an object (table1) to store matrix of avg values and total obs
table1 <- matrix (average.values.and.obs, nrow = 2, byrow = TRUE,
                       dimnames=list(value= c("Avg or Total Value", "Number of Obs"),
                                     Variable = c("Avg Air Temp", "Avg Wind Speed", "Avg Soil Mosisture", "Avg Soil Temp", "Total Percipitation")))
#print table1
table1

##find min and max day or year for each measurement 

     
#question 9
#four plots of soil moist, air temp, soil temp, and precipitation
##same x-axis range
par(mfrow=c(2,2))
plot(x = datW$DD, y = datW$soil.moisture, type = "l", xlab = "Day of Year", ylab = "Soil Moisture", main = "Soil Moisture")
plot(x = datW$DD, y = datW$air.temperature, type = "l", xlab = "Day of Year", ylab = "Air Tempature", main = "Air Tempature")
plot(x = datW$DD, y = datW$soil.temp, type = "l", xlab = "Day of Year", ylab = "Soil Tempature", main = "Soil Tempature")
plot(x = datW$DD, y = datW$precipitation, type = "l", xlab = "Day of Year", ylab = "Precipitation", main = "Precipitation")
     
     



