##Activity 5 Gabrielle Sorresso
##GS 3/23/2022
##set working directory 
setwd("Z://students/gsorresso/")

#load in lubridate
library(lubridate)

#read in stream flow data 
datH <- read.csv("datafolder/stream_flow_data.csv", na.strings=c("Eqp"))
#look at headers of datH 
head(datH)

#read in precipitation data 
##hourly precipitation data is in mm 
datP <- read.csv("datafolder/2049867.csv") 
#look at headers of datP 
head(datP)

#only use most reliable data 
datD <- datH[datH$discharge.flag == "A",]

### define time for streamflow ### 
#convert date and time 
datesD <- as.Date(datD$date, "%m/%d,%Y")
#get day of year 
datD$doy <- yday(datesD)
#calculate year 
datD$year <- year(datesD)
#define time 
timesD <- hm(datD$time)

### define time for precipitation ### 
#convert date and time 
dateP <- ymd_hm(datP$DATE)
#get day of year 
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

### get decimal formats ###
#convert time from a string to a more usabel format 
#with a decimal hour 
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal 
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year but account for leap years 
datD$decYear <- ifelse(leap_year(datD$year), datD$year + (datD$decDay/366), 
                       (datD$year) + (datD$decDay/365)) 

#calculate time for datP 
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal 
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year but account for leap years 
datP$decYear <- ifelse(leap_year(datP$year), datP$year + (datP$decDay/366), 
                       (datP$year) + (datP$decDay/365)) 

#plot discharge 
plot(datD$decYear, datD$discharge, type = "l" , xlab = "Year", ylab = expression(paste("Discharge ft"^"", "sec"^"-1")))")))










