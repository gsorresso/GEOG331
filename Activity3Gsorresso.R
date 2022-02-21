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

