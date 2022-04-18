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

#read file with field observations of canopy cover 
tree <- read.csv("Z:/data/rs_data/siberia_stand_data.csv",
                 header = T)

#convert to vector object using terra package 
gtree <- vect(tree, geom = c("Long", "Lat"), "epsg:4326")

#min and max long and lat
ext(gtree)
ext(p)

#project the data to match the cordinate system of the raster layer 
gtree2 <- project(gtree,p)
ext(gtree2)

#create a polygon from the extent of the points 
b <- as.lines(ext(gtree), "epsg:4326")

#reproject the polygons to the same projection as our raster
b2 <- project(b,crs(p))

#buffer the extent by 200m 
b3 <- buffer(b2, width = 200)

#crop raster layer to see study area 
p2 <- crop(p,b3,
           filename = "20190706_SR_crop.tif",
           overwrite = TRUE)

#make a plot
plotRGB(p2, r =3, g=2, b = 1, 
        scale = 65535,
        stretch = "lin")

#add points to plot
points(gtree2, col = "blue", cex=gtree2$cc.pct/100)

#caculate NDVI 
ndvi <- (p2[[4]]-p2[[3]])/(p2[[4]]+p2[[3]])

#set the layer name to ndvi 
names(ndvi) <- "ndvi"

#create a plot of the ndvi map with sample points on top 
png(filename = "nadvi_map.png", 
    width = 6, height = 4, units = "in", res = 300)
plot (ndvi)
points(gtree2, cex = gtree$cc.pct/50, col = "blue")

##turn graphics device off
dev.off()

#extract NDVI values for each point \
nt <- terra:extract(nvigtree2,fun = mean, method = "bilinear")

#plot ndvi v. canopy cover 
plot(nt$ndvi,gtree2$cc.pct,
     pch = 16, col = "blue")

#plot again but fix y-axis
plot(nt$ndvi,gtree2$cc.pct,
     pch = 16, col = "blue",
     xlim = c(0,1))



