#Install necessary libraries
rm(list=ls())
library(rgdal)
library(raster)

# Load image using brick function. Image is cloud free, and urban area masked
img <- brick("C:\\Image_cloud_urban.tif") 

# Read five individual bands from images
BLUE <- raster(img, layer=1) # Blue band
GREEN <- raster(img, layer=2) # Green band
RED <- raster(img, layer=3) # Red band
RED_E <- raster(img, layer=4) # Red edge band of RapidEye image
NIR <- raster(img, layer=5) # NIR band

#Start calculating 30 features such as
# # RVI, NIR_r1, NIR_r2, NIR_r3, NIR_r4, B_G,B_R,B_RE,B_NIR, G_R, G_RE ,G_NIR ,
# # NDVI_RE, GDVI,GNDVI,DVI,SGI,TVI,CTVI,TTVI,NRVI,MSR,IPVI,GEMI,SAVI,MSAVI,ARVI,SARVI,GARI

#Simple Ratio index
RVI <- NIR/RED #Ratio vegetation index, NIR/RED, Reference:Pearson and Miller 1972
NIR_r1 <- NIR/(BLUE+GREEN) # Infrared and visible band ratio, Reference:Gibson and Power 2010
NIR_r2 <- NIR/(BLUE+GREEN+RED) # Infrared and visible band ratio, Reference:Gibson and Power 2010
NIR_r3 <- RED_E/(BLUE+GREEN) # Infrared and visible band ratio, Reference:Gibson and Power 2010
NIR_r4 <- RED_E/(BLUE+GREEN+RED) # Infrared and visible band ratio, Reference:Gibson and Power 2010
B_G <- BLUE/GREEN # Simple Band ratio
B_R <- BLUE/RED   # Simple Band ratio
B_RE <- BLUE/RED_E # Simple Band ratio
B_NIR <-BLUE/NIR # Simple Band ratio
G_R <-GREEN/RED # Simple Band ratio
G_RE <-  GREEN/RED_E # Simple Band ratio
G_NIR <- GREEN/NIR # Simple Band ratio

#Vegetation index calcuation
NDVI_NIR <- (NIR - RED)/(NIR + RED) # Normalized Difference Vegetation Index (NDVI_NIR)
NDVI_RE <- (RED_E - RED)/(RED_E + RED) #  RedEdge Normalized Difference Vegetation Index (NDVI_RE)
GDVI <- NIR-GREEN #Green Difference Vegetation Index (GDVI)
GNDVI <- (NIR-GREEN)/ (NIR+GREEN) # Green Normalized Difference Vegetation Index (GNDVI)
DVI <- NIR-RED #Difference Vegetation Index (DVI)
SGI <- (RED+GREEN)/2 # Sum Green Index (SGI)
TVI <- (((NIR-RED)/(NIR+RED))^0.5)+0.5 #Transformed Vegetation Index (TVI)
CTVI <- ((NDVI_NIR+0.5) /abs(NDVI_NIR+0.5)) * sqrt(abs(NDVI_NIR+0.5)) #Corrected Transformed Vegetation Index (CTVI)
TTVI <- sqrt(abs(NDVI_NIR+0.5)) # Thiam's Transformed Vegetation Index (TTVI)
NRVI <- RVI-1/RVI+1 #Normalized Ratio Vegetation Index (NRVI)
MSR <- ((NIR/RED)-1) / (sqrt(NIR/RED)+1) #Modified Simple Ratio (MSR)
IPVI <- NDVI_NIR/2 # Infrared Percentage Vegetation Index (IPVI)
eta <- 2*(NIR^2 -RED^2)+(1.5*NIR)+(0.5*RED)/(NIR+RED+0.5) #for GEMI calculation
GEMI <- (eta * (1-0.25*eta)) - ((RED-0.125)/(1-RED)) # Global Environmental Monitoring Index (GEMI)
L <- 0.5 # Low vegetation, L = 1, intermediate, 0.5, and high 0.25
SAVI <- (NIR-RED)/(NIR+RED+L)*(1+L) # Soil-Adjusted Vegetation Index (SAVI)
MSAVI <- (2*NIR+1-sqrt((2*NIR+1)^2-(8*(NIR-RED)))) # Modified Soil-Adjusted Vegetation Index (MSAVI)
gam <- 1 # to correct the radiance in the red band, The gamma constant is a weighting function that depends on aerosol type.
Prb <- NIR - (gam*(BLUE-RED)) # to calculate ARVI and SARVI
ARVI <- (NIR - Prb)/(NIR+Prb) # Atmospherically Resistant Vegetation Index (ARVI)
SARVI <- ((1+L)*(NIR-Prb))/(NIR+Prb+L) #Soil and Atmospherically Resistant Vegetation Index (SARVI)
Prb_2 <-  GREEN - (gam*(BLUE-RED)) # to calculate GARI
GARI <- (NIR - Prb_2)/(NIR + Prb_2) # Green Atmospherically Resistant Index (GARI)

img_merge <- stack(img, NDVI_NIR, RVI, NIR_r1, NIR_r2, NIR_r3, NIR_r4, B_G, B_R, B_RE, B_NIR, G_R, G_RE, G_NIR ,
                   NDVI_RE, GDVI, GNDVI, DVI, SGI, TVI, CTVI, TTVI, NRVI, MSR, IPVI, GEMI, SAVI, MSAVI, ARVI, SARVI, GARI) # merge raster layers
writeRaster(img_merge, filename = "C:\\RapidEye_year.tif", format="GTiff") # write raster image