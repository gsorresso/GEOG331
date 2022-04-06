#GS 4/6 Participation 

#load terra package 
library(terra)

##set working directory 
setwd("Z://students/gsorresso/")
##read rasta from file 
p <- rast("Z:/data/rs_data/20190706_002918_101b_3B_AnalyticMS_SR.tif")

#plot the rasta 
plot(p)

#plot and rbg rendering of the data
plotRGB(p, r = 3, g = 2, b = 1,
        scale = 65535,
        stretch = "hist") 


