##Activity 2 Gabrielle Sorresso
##GS 1/31/2022
##set working directory to my noaa data folder
setwd("Z://students/gsorresso/")

##################################VECTORS#######################################
##make a vector of tree heights in meters
heights <- c(30, 41, 20, 22)
##practice math with vectors
##convert to cm
heights_cm <- hieghts*100
heights_cm
##look at first tree heights
heights[1]

##look at second and third heights (: indicates cont. series of vectors)
heights[2:3]

##more vector practice
##list numbers 1 tru 99
1:99
#####################################MATRICES####################################
##get more info on the matrix function
help(matrix)

##set up a matrix with 2 columns and fill in by rows
Mat<- matrix(c(1,2,3,4,5,6), ncol=2, byrow = TRUE)
Mat
##set up a matrix with 2 columns and fill in by column
Mat.bycol<- matrix(c(1,2,3,4,5,6), ncol=2, byrow = FALSE)
Mat.bycol
##subset matrix to look at row1, column2
Mat.bycol[1,2]
##look at all values in row 1
Mat.bycol[1,]
##look at all values in column 2  
Mat.bycol[,2]

###############################QUESTION 2#######################################
##character vector 
char_vector <- c("colgate", "has", "no", "drinking", "water")
##numeric vector 
num_vector <- c(1.1, 2.2, 3.3, 4.4, 5.5)
##integer vecor 
int_vector <- c(1L, 2L, 3L, 4L, 5L)
##factor vector 
fac_vector <- factor(c("no drinking water", "drinking water", "no drinking water"))

unclass(fac_vector)

################################DATA FRAMES####################################
##Read in Data
datW <- read.csv("datafolder/2011124.csv", stringsAsFactors = T)

##see first row
datW[1,]

##get more infor about the metadata
str(datW)

##specify a column with a proper date format 
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
##create a date column by reformatting the date to only include years and indicate this is numeric data 
datW$year <- as.numeric(format(datW$dateF, "%Y"))

##descriptive statistics 
##find all unique site names 
unique(datW$NAME)

##look at mean max temp for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

##Na value means missing data
##ignore na/missing values 
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
#calculate avg daily temp  (halfway between min and max temp)
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
##length function 

#get mean across all sites 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm=TRUE)
averageTemp

##change auto output to be more meaningful 
colnames(averageTemp) <- c("NAME", "MAAT")
averageTemp

##convert level to number for factor data type 
datW$siteN <- as.numeric(datW$NAME)
##add code to run all four histograms together 
par(mfrow=c(2,2))
##make a histogram for the first site in our levels and save it as h1 
h1<- hist(datW$TAVE[datW$siteN == 1],
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily Temp (degrees C)",
     ylab = "Relative Frequency", 
     col = "grey50",
     border = "white")

#add mean line with red color and thickness 3 
abline(v = mean(datW$TAVE[datW$siteN == 1], na.rm=TRUE),
       col = "tomato3",
       lwd = 3)
# add sd line below mean with red color and thickness 3 
abline(v = mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE) - sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
# add sd line above mean with red color and thickness 3 
abline(v = mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE) + sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
##Answer Q 4 make three other histograms for sites daily averages 
#make histogram for the second site in our levels, livermore CA 
hist(datW$TAVE[datW$siteN == 2],
     freq = FALSE,
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily Temp (degrees C)",
     ylab = "Relative Frequency", 
     col = "azure3",
     border = "white")
#add mean line with red color and thickness 3 
abline(v = mean(datW$TAVE[datW$siteN == 2], na.rm=TRUE),
       col = "tomato3",
       lwd = 3)
# add sd line below mean with red color and thickness 3 
abline(v = mean(datW$TAVE[datW$siteN == 2], na.rm = TRUE) - sd(datW$TAVE[datW$siteN == 2], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
# add sd line above mean with red color and thickness 3 
abline(v = mean(datW$TAVE[datW$siteN == 2], na.rm = TRUE) + sd(datW$TAVE[datW$siteN == 2], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
#make histogram for the fourth site in our levels, Mormon Flat AZ 
hist(datW$TAVE[datW$siteN == 4],
     freq = FALSE,
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily Temp (degrees C)",
     ylab = "Relative Frequency", 
     col = "cornsilk1",
     border = "white")
#add mean line with red color and thickness 3 
abline(v = mean(datW$TAVE[datW$siteN == 4], na.rm=TRUE),
       col = "tomato3",
       lwd = 3)
# add sd line below mean with red color and thickness 3 
abline(v = mean(datW$TAVE[datW$siteN == 4], na.rm = TRUE) - sd(datW$TAVE[datW$siteN == 4], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
# add sd line above mean with red color and thickness 3 
abline(v = mean(datW$TAVE[datW$siteN == 4], na.rm = TRUE) + sd(datW$TAVE[datW$siteN == 4], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
#make histogram for the fifth site in our levels, morrisville 6 SW, NY 
hist(datW$TAVE[datW$siteN == 5],
     freq = FALSE,
     main = paste(levels(datW$NAME)[5]),
     xlab = "Average daily Temp (degrees C)",
     ylab = "Relative Frequency", 
     col = "mistyrose3",
     border = "white")
#add mean line with red color and thickness 3 
abline(v = mean(datW$TAVE[datW$siteN == 5], na.rm=TRUE),
       col = "tomato3",
       lwd = 3)
# add sd line below mean with red color and thickness 3 
abline(v = mean(datW$TAVE[datW$siteN == 5], na.rm = TRUE) - sd(datW$TAVE[datW$siteN == 5], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
# add sd line above mean with red color and thickness 3 
abline(v = mean(datW$TAVE[datW$siteN == 5], na.rm = TRUE) + sd(datW$TAVE[datW$siteN == 5], na.rm = TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)

##probability distributions 
#seq function generate a seq of numbers that we can use to plot the normal across the range of temperature values 
x.plot <- dnorm(seq(-10,30, length.out = 100),
      mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE), 
      sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE)) 

#the dnorm function will produce a variable the probability density based on a mean and standard dev. 
y.plot <- dnorm(seq(-10,30, length.out = 100),
    mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE), 
    sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE)) 
                
##create a density that is scaled to fit the plot since the density has a different range from the data density 
##helpful for putting things on the same plot to make both plots share a max 
y.scaled <- (max(h1$density)/max(y.plot))* y.plot 
                
## points function adds points of lines to a graph, the first two arguments are the x coordinates and y coordinators 
points(x.plot, 
    y.scaled, 
    type = "l", 
    col = "royalblue3", 
    lwd = 4,
    lty = 2)
               
## run documentation on dnorm to get more info on probability distributions 
help(dnorm) 
                
#probability of freezing temps 
# syntax: pnorm(value to evaluate at(& below), mean, sd) 
pnorm (0, 
    mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE), 
    sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE)) 
                
## probability of temps under 5 degrees 
pnorm (5, 
    mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE), 
    sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE)) 
                
##find probability of temps between 0 - 5 degrees by subtracting prob under 5 degrees from prob under zero degrees
pnorm (5, 
    mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE), 
    sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE))  - pnorm (0,
    mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE), 
    sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE)) 
                
##probability of a temperature above 20 degrees C 
## know that pnorm gives all values under 20 so we can subtract this from 1 
1 - pnorm (20, 
    mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE), 
    sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE))
                
## use qnorm to find all high temp values that happen with less than 5% probability 
qnorm (0.95, 
    mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE), 
    sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE))
                
##Q7 Histogram for Daily Precipitation in Aberdeen 
hist(datW$PRCP[datW$siteN == 1],
    freq = FALSE,
    main = paste(levels(datW$NAME)[1]),
    xlab = "Daily Percipitation (cm) ",
    ylab = "Relative Frequency",
    col = "azure3",
    border = "white")

##Q8 Get the precipitation for each year and site and then make a histogram for one site of annual precipitation
AnnualP <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum", na.rm=TRUE)
AnnualP
colnames(AnnualP) <- c("NAME", "YEAR", "Percip")

##Q7 Histogram for Annual Percip in Livermore CA 
hist(AnnualP$Percip[AnnualP$NAME == "LIVERMORE, CA US"],
     freq = FALSE,
     main = "Annual Percipitation in Livermore CA",
     xlab = "Annual Percipitation (cm) ",
     ylab = "Relative Frequency",
     col = "azure3",
     border = "white")

##Q9 Mean annual Precipitation for all sites 
MeanAnnualPercip <- aggregate(AnnualP$Percip, by=list(AnnualP$NAME), FUN ="mean", na.rm = TRUE)
colnames(MeanAnnualPercip) <- c("NAME", "MeanPercip")
##look at the average percipitation and temp at all sites 
MeanAnnualPercip
averageTemp


                    