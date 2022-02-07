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
datW <- read.csv("datafolder/2011124.csv")

##set first row
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

##look at mean max tempp for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

##Na value means missing data
##ignore na/missing values 
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
#calculate avg daily temp  (halfway between min and max temp)
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get mean across all sites 
averageTemp <- aggregate(datW$TAVE, by=list(datW$Name), FUN="mean", na.rm=TRUE)
averageTemp

##change auto output to be more meaningful 
colnames(averageTemp) <- c("NAME", "MAAT")
averageTemp 