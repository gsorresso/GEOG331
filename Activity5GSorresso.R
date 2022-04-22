##Activity 5 Gabrielle Sorresso
##GS 3/23/2022
##set working directory 
setwd("Z://students/gsorresso/")

#load in lubridate
library(lubridate)
library(ggplot2)
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

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))  
#q2 results of leap year function 
leap_year(datP$year)

#plot discharge 
plot(datD$decYear, datD$discharge, type = "l", xlab ="Year", ylab = expression(paste("Discharge ft"^"3 ", "sec"^"-1")))

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot window
#dev.new(width=8,height=8)
#create bigger margins
par(mai=c(1,1,1,1))

#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes


#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
#create a legend 
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#################################################################################
#################################################################################
##QUESTION 5 
#add a new line to show observations for 2017 on graph 
##change axis labels to show each month instead of year 
###make 2017 line a different color 

#get dataframe of only 2017 data 
#create a subset for dataframe for 2016 
datD17 <- datD[datD$year == 2017,]

#start new plot window
#dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Mid Point of Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(15,345, by=30), #tick intervals
     lab=seq(1,12, by=1)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2) #show ticks at 90 degree angle

#add a line to show 2017 data in red 
lines(datD17$discharge, type = "l", col = "red") 
#add a legend 
legend("topright", c("mean","1 standard deviation", "2017 observations"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "red"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#################################################################################
#################################################################################
##QUESTION 7 ##
#create a data frame that indicates what days have a full 24 hours of precipitation measurements 
##plot all discharge measures and symbolize days that have precipitation measures available 
library("dplyr")
#create a full day variable
datP$fullday <- paste(yday(dateP), year(dateP))
#create a new df of full days 
fulldays <- summarise(group_by(datP, fullday), sum(hour))
#name columns 
colnames(fulldays) <- c("FullDate", "TotalHours")



#################################################################################
#################################################################################
##practice hydro graph 
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]
##look at minimum hydro flow during period 
min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

#expand margins 
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#################################################################################
#################################################################################
##QUESTION 8
#make another hydro graph during the winter 
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 302 & datD$doy < 304 & datD$year == 2008,]
hydroP <- datP[datP$doy >= 302 & datP$doy < 304 & datP$year == 2008,]
##look at minimum hydro flow during period 
min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

#expand margins 
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#################################################################################
#################################################################################
#Box Plots and Violin Plots 
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin() 


#################################################################################
#################################################################################
##QUESTION 9
#make a violin plot by season for 2016 and 2017 separately 

#subset 2016 data 
#create a subset for dataframe for 2016 
datD16 <- datD[datD$year == 2016,]
#create a season categorical variable 
datD16$seasonC <- 0 
datD16$seasonC <- ifelse(datD16$doy >= 61 & datD16$doy <= 152, 1, datD16$season)
datD16$seasonC <- ifelse(datD16$doy >= 153 & datD16$doy <= 244, 2, datD16$season)
datD16$seasonC <- ifelse(datD16$doy >= 245 & datD16$doy <= 303, 3, datD16$season)

#create as factor 
datD16$season <- cut(datD16$seasonC, 4, labels = c('Winter', 'Spring', 'Summer', "Fall"))

#make a violin plot
ggplot(data= datD16, aes(season,discharge)) + 
  geom_violin() + 
  ggtitle("Discharge in 2016 by Season") 

#subset 2017 data 
datD17 <- datD[datD$year == 2017,]

#create a season categorical variable 
datD17$seasonC <- 0 
datD17$seasonC <- ifelse(datD17$doy >= 60 & datD17$doy <= 151, 1, datD17$season)
datD17$seasonC <- ifelse(datD17$doy >= 152 & datD17$doy <= 243, 2, datD17$season)
datD17$seasonC <- ifelse(datD17$doy >= 244 & datD17$doy <= 302, 3, datD17$season)

#create as factor 
datD17$season <- cut(datD17$seasonC, 4, labels = c('Winter', 'Spring', 'Summer', "Fall"))

#make a violin plot
ggplot(data= datD17, aes(season,discharge)) + 
  geom_violin() + 
  ggtitle("Discharge in 2017 by Season")

#################################################################################
#################################################################################



