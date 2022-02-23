##Activity 3 Gabrielle Sorresso
##GS 2/21/2022
##set working directory 
setwd("Z://students/gsorresso/")

#Create Own Function Example
#Name of arguments is in (), everything in {} is run each time funct is run 
assert <- function(statement,err.message){
  if(statement == FALSE){
  print(err.message)  
  }
}

#check how statement works by evaluating a false statement 
assert(1==2, "error: unequal values")
#evaluate a true statement 
assert(2==2, "error: unequal values")
#set assert to check if two vectors are the same length 
a <- c(1, 2, 3, 4)
b <- c(8, 4, 5)
assert(length(a) == length(b), "error: unequal length")


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



##filter out storms in wind and air temp measurements
###filter all values with lightning that coincide with rainfall greater than 2mm or only rainfall over 5mm
###also create a new air temp column 
datW$air.tempQ2 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy > 0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))


##question 6 


##question 7 
##check that soil measurements are reliable in days leading up to outage 
##get mean of soil temp 
MeanSoilTemp <- aggregate(datW$soil.temp, FUN="mean", na.rm=TRUE)
MeanSoilTemp <- aggregate(datW$soil.temp, FUN="mean", na.rm=TRUE, if datW$doy >= 195)




