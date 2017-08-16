
# remove list
# install libraries
rm(list=ls())
library(rgdal)
library(raster)

#########################Disturbance index calculation########################################

# read raster image and forest mask
img_full <- brick("C:\\image.tif") # use brick function to read all bands in a raster image
forestmask <- shapefile("C:\\forestmask.shp") 

# use forest mask to remove all unwanted class for example water, peatland
img <- mask(img_full, forestmask)


# Calculation of Tasseled cap coefficient for Rapideye image.
#Reference: Derivation of tasseled cap coefficients for RapidEye data, 
#Author(s): Maurice Schönert; Horst Weichelt; Erik Zillmann; Carsten Jürgens, 
#Proc. SPIE 9245, Earth Resources and Environmental Remote Sensing/GIS Applications V, 92450Q (23 October 2014); doi: 10.1117/12.2066842

# Tasseled Cap coefficents 
Coe_B <- c(0.2435,	0.3448,	0.4881,	0.493,	0.5835)
Coe_G <- c(-0.2216,	-0.2319,	-0.4622,	-0.2154,	0.7981)
Coe_W <- c(-0.7564,	-0.3916,	0.5049,	0.14,	0.0064)

#Brightness, yelloness and greennes index calculation
Brightness <- Coe_B[1]*img[[1]] +  Coe_B[2]*img[[2]] + Coe_B[3]*img[[3]] + Coe_B[4]*img[[4]] + Coe_B[5]*img[[5]]
Greenness <- Coe_G[1]*img[[1]] +  Coe_G[2]*img[[2]] + Coe_G[3]*img[[3]] + Coe_G[4]*img[[4]] + Coe_G[5]*img[[5]]
Yellowness <- Coe_W[1]*img[[1]] +  Coe_W[2]*img[[2]] + Coe_W[3]*img[[3]] + Coe_W[4]*img[[4]] + Coe_W[5]*img[[5]]

mean_B <- cellStats(Brightness, stat="mean") # mean brightness
sd_B <- cellStats(Brightness, stat="sd") # standard deviation of brightness
Brightness <- (Brightness - mean_B)/sd_B # Brightness

mean_G <- cellStats(Greenness, stat="mean") # mean greennes
sd_G <- cellStats(Greenness, stat="sd") # standard deviation of greennes
Greenness <- (Greenness - mean_G)/sd_G  # Greennes

mean_Y <- cellStats(Yellowness, stat="mean") # mean yelloness
sd_Y <- cellStats(Yellowness, stat="sd") # standard deviation of yelloness
Yellowness <- (Yellowness - mean_Y)/sd_Y #Yelloness

#Disturbance feature calcuation such as DI, TCD, TCA
DI <-  Brightness - (Greenness + Yellowness) #DI = Disturbance index
DI_TCD <- sqrt(Brightness^2 + Greenness^2) #TCD = Tasseled Cap distance
DI_TCA <- atan(Greenness/Brightness) #TCA = Tasseled Cap angle

#write rasters 
writeRaster(DI, filename = "C:\\DI_year.tif", format="GTiff", overwrite = TRUE) #write DI = Disturbance index
writeRaster(DI_TCD, filename = "C:\\DI_TCD_year.tif", format="GTiff", overwrite = TRUE) #write TCD = Tasseled Cap distance
writeRaster(DI_TCA, filename = "C:\\DI_TCA__year.tif", format="GTiff", overwrite = TRUE) #write TCA = Tasseled Cap angle

###############################################################################################