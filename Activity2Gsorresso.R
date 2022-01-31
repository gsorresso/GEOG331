##Activity 2 Gabrielle Sorresso 
##GS 1/31/2022

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

################################DATA FRAMES####################################
##Read in Data
datW <- read.csv("Z:\\GEOG331_S22\data\noaa\_weather\2011124.csv")












