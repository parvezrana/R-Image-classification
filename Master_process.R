##############################################################################################################
# USER INPUTS NEEDED
##############################################################################################################

# install libraries
rm(list=ls())
library(rgdal)
library(raster)
library(lattice)
library(ggplot2)
library(caret)
library(snow)
library(randomForest)
library(e1071)
library(kernlab)
library(SDMTools)
library(landsat)
library(glcm)
library(rJava)
library(extraTrees)

## STEP 1
## Step 1 calculates all the features differences between two years
# At first, we need to load the output raster using using Feature_calculation.R 
#Load images using brick function
img_merge_full_year1 <- brick( "C:\\RapidEye_year1.tif")
img_merge_full_year2 <- brick( "C:\\RapidEye_year2.tif")

forestmask <- shapefile("C:\\forestmask.shp") # forest mask
img_merge_year1 <- mask(img_merge_full_year1, forestmask) # # use forest mask to remove all unwanted class such as water, grassland, cropland and peatland
img_merge_year2 <-  mask(img_merge_full_year2, forestmask) # use forest mask to remove all unwanted class such as water, grassland, cropland and peatland

# 35 Features difference calculation. # Raster image should have same extent. #To see feature name, please have to look at the Feature_calculation.R
B1_dif <- img_merge_year1[[1]] - img_merge_year2[[1]] # difference
B2_dif <- img_merge_year1[[2]] - img_merge_year2[[2]] # difference
B3_dif <- img_merge_year1[[3]] - img_merge_year2[[3]] # difference
B4_dif <- img_merge_year1[[4]] - img_merge_year2[[4]] # difference
B5_dif <- img_merge_year1[[5]] - img_merge_year2[[5]] # difference
NDVI_dif <- img_merge_year1[[6]] - img_merge_year2[[6]] # difference
RVI_dif <- img_merge_year1[[7]] - img_merge_year2[[7]] # difference
NIR_r1_dif <- img_merge_year1[[8]] - img_merge_year2[[8]] # difference
NIR_r2_dif <- img_merge_year1[[9]] - img_merge_year2[[9]] # difference
NIR_r3_dif <- img_merge_year1[[10]] - img_merge_year2[[10]] # difference
NIR_r4_dif <- img_merge_year1[[11]] - img_merge_year2[[11]] # difference
B_G_dif <- img_merge_year1[[12]] - img_merge_year2[[12]] # difference
B_R_dif <- img_merge_year1[[13]] - img_merge_year2[[13]] # difference
B_RE_dif <- img_merge_year1[[14]] - img_merge_year2[[14]] # difference
B_NIR_dif <- img_merge_year1[[15]] - img_merge_year2[[15]] # difference
G_R_dif <- img_merge_year1[[16]] - img_merge_year2[[16]] # difference
G_RE_dif <- img_merge_year1[[17]] - img_merge_year2[[17]] # difference
G_NIR_dif <- img_merge_year1[[18]] - img_merge_year2[[18]] # difference
NDVI_RE_dif <- img_merge_year1[[19]] - img_merge_year2[[19]] # difference
GDVI_dif <- img_merge_year1[[20]] - img_merge_year2[[20]] # difference
GNDVI_dif <- img_merge_year1[[21]] - img_merge_year2[[21]] # difference
DVI_dif <- img_merge_year1[[22]] - img_merge_year2[[22]] # difference
SGI_dif <- img_merge_year1[[23]] - img_merge_year2[[23]] # difference
TVI_dif <- img_merge_year1[[24]] - img_merge_year2[[24]] # difference
CTVI_dif <- img_merge_year1[[25]] - img_merge_year2[[25]] # difference
TTVI_dif <- img_merge_year1[[26]] - img_merge_year2[[26]] # difference
NRVI_dif <- img_merge_year1[[27]] - img_merge_year2[[27]] # difference
MSR_dif <- img_merge_year1[[28]] - img_merge_year2[[28]] # difference
IPVI_dif <- img_merge_year1[[29]] - img_merge_year2[[29]] # difference
GEMI_dif <- img_merge_year1[[30]] - img_merge_year2[[30]] # difference
SAVI_dif <- img_merge_year1[[31]] - img_merge_year2[[31]] # difference
MSAVI_dif <- img_merge_year1[[32]] - img_merge_year2[[32]] # difference
ARVI_dif <- img_merge_year1[[33]] - img_merge_year2[[33]] # difference
SARVI_dif <- img_merge_year1[[34]] - img_merge_year2[[34]] # difference
GARI_dif <- img_merge_year1[[35]] - img_merge_year2[[35]] # difference

img_merge_dis <- stack(B1_dif, B2_dif, B3_dif, B4_dif, B5_dif, NDVI_dif,
                   RVI_dif, NIR_r1_dif, NIR_r2_dif, NIR_r3_dif, NIR_r4_dif,
                   B_G_dif, B_R_dif, B_RE_dif, B_NIR_dif, G_R_dif, G_RE_dif, G_NIR_dif ,
                  NDVI_RE_dif, GDVI_dif, GNDVI_dif, DVI_dif, SGI_dif, TVI_dif, CTVI_dif, TTVI_dif, NRVI_dif,
                  MSR_dif, IPVI_dif, GEMI_dif, SAVI_dif, MSAVI_dif, ARVI_dif, SARVI_dif, GARI_dif) # stack all raster layers

writeRaster(img_merge_dis, filename = "C:\\D_year1year2_diff.tif", format="GTiff") # write raster image

# read raster images of Disturbance indeces features such as DI, TCD, TCA
DI_year1 = raster("C:\\DI_year1.tif") #DI = Disturbance index
DI_year1_TCD = raster("C:\\DI_year1_TCD.tif") #TCD = Tasseled Cap distance
DI_year1_TCA = raster("C:\\DI_year1_TCA.tif") #TCA = Tasseled Cap angle

DI_year2 = raster("C:\\DI_year2.tif") #DI = Disturbance index
DI_year2_TCD = raster("C:\\DI_year2_TCD.tif") #TCD = Tasseled Cap distance
DI_year2_TCA = raster("C:\\DI_year2_TCA.tif")# TCA = Tasseled Cap angle

# Raster image should have same extent.
DI_year1year2 <- DI_year1 - DI_year2 # difference DI
DI_year1year2_TCD <- DI_year1_TCD - DI_year2_TCD # difference TCD
DI_year1year2_TCA <- DI_year1_TCA - DI_year2_TCA # difference TCA

DI_all_year1year2 <- stack(DI_year1year2, DI_year1year2_TCD, DI_year1year2_TCA)  # stack all raster layers
writeRaster(DI_all_year1year2, filename = "C:\\DI_year1year2_diff.tif", format="GTiff")  # write raster image


##########################################################################################################################################
### STEP 2
### Step 2 involves the modeling and classification using 3 machine learning algorithms

# load rasters 
D_year1year2_diff = brick("C:\\D_year1year2_diff.tif")
DI_year1year2_diff = brick("C:\\DI_year1year2_diff.tif")

img_merge_dis <- stack(D_year1year2_diff, DI_year1year2_diff) # stack rasters

# in the next five lines, we will change the layers names.
names(img_merge_dis)[1:5] <- c(paste0("B", 1:5, coll = "")) # rename raster layer
names(img_merge_dis)[6] <- paste("NDVI") # rename raster layer
names(img_merge_dis)[7:35] <- paste0(c("RVI", "NIR_r1", "NIR_r2", "NIR_r3", "NIR_r4", "B_G", "B_R", "B_RE", "B_NIR", "G_R", "G_RE", "G_NIR" ,
                                       "NDVI_RE", "GDVI", "GNDVI", "DVI", "SGI", "TVI", "CTVI", "TTVI", "NRVI", "MSR", "IPVI", "GEMI", "SAVI", "MSAVI", "ARVI", "SARVI", "GARI")) # rename raster layer
names(img_merge_dis)[36:38] <- paste0(c("DI_20102015", "DI_20102015_TCD", "DI_20102015_TCA"))
names(img_merge_dis)

# Load the training area
trainData_dis <- shapefile("C:\\Training_area.shp")
names(trainData_dis)
responseCol <- "Classvalue" # land use type ID
head(trainData_dis, 5) # see the first five rows in the training area 

#Extracting training pixels values
DF_Trian = data.frame(matrix(vector(), nrow = 0, ncol = length(names(img_merge_dis)) + 6))
for (i in 1:length(unique(trainData_dis[[responseCol]]))){
  category <- unique(trainData_dis[[responseCol]])[i]
  categorymap <- trainData_dis[trainData_dis[[responseCol]] == category,]
  dataSet <- extract(img_merge_dis, categorymap, sp=TRUE)
  dataSet <- dataSet@data
  dataSet <- cbind(dataSet, class = as.numeric(category))
  DF_Trian <- rbind(DF_Trian, dataSet)
}

set.seed(123)
# Divide training data into a train-subset and a test-subset
train <- sample(nrow(DF_Trian), round((nrow(DF_Trian) - 1) / 2, 0))
test <- c(1:nrow(DF_Trian))[!(c(1:nrow(DF_Trian)) %in% train)]
trainset_df <- DF_Trian[train,]
testset_df <- DF_Trian[test,]
trainset_df <- na.omit(trainset_df)
testset_df <- na.omit(testset_df)
write.table(trainset_df, "C:\\trainset_df.txt", sep="\t") # write training set
write.table(testset_df, "C:\\testset_df.txt", sep="\t") # write test set

# remove unnecessary column from the data frame for further processing
sdfAll_train <- trainset_df[, -(1:5)] # Final training set
sdfAll_vali <-  testset_df[, -(1:5)] # Final test set

#################### Model development using Random Forest algorithm #########################################
#variable importance using randomforest pacakge
rf.model_train <- randomForest(as.factor(class) ~ ., data=sdfAll_train, importance=TRUE, ntree=5000) # total 38 variable
VI_F=importance(rf.model_train)
barplot((VI_F[,6]/sum(VI_F[,6])), xlab="Feature", ylab="Importance", main="Monitoring period Year1 Year2", col=rainbow(20), las=2) #if MeanDecreaseGini
varImpPlot(rf.model_train, sort=T, main='Monitoring period Year1 Year2')

#all features/variables used in training
rf.model_train <- train(as.factor(class) ~ ., method = "rf", data = sdfAll_train)
#prediction
rm.pred <- predict(rf.model_train, sdfAll_vali[,-39])

## compute confusion matrix using caret package
results <- confusionMatrix(data = rm.pred, sdfAll_vali[,39])
# User accuracy (UA) and Producer accuracy (PA)
as.table(results)
mat <- as.matrix(results)
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")
UA <- diag(mat) /rowSums(mat) # User accuracy (UA)
PA <- diag(mat)/colSums(mat) # Producer accuracy (PA)


#################### Model development using Extremely Randomized Trees algorithm (ERT) ################################
#38 variables used in training
ert.model_train <- train(as.factor(class) ~ ., method =  "extraTrees", data = sdfAll_train)
#prediction
ERT.pred <- predict(ert.model_train, sdfAll_vali[,-39])

## compute confusion matrix using caret package
results <- confusionMatrix(data = ERT.pred, sdfAll_vali[,39])
# User accuracy (UA) and Producer accuracy (PA)
as.table(results)
mat <- as.matrix(results)
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")
UA <- diag(mat) /rowSums(mat) # User accuracy (UA)
PA <- diag(mat)/colSums(mat) # Producer accuracy (PA)


#######################  Model development using Support Vector Machine algorithm (SVM) ####################################
# # 38 varialbes used in training
svm.model_train <- train(as.factor(class) ~ ., method = "svmPoly", data = sdfAll_train)
svmPoly.pred <- predict(svm.model_train, sdfAll_vali[,-39])

## compute confusion matrix using caret package
results <- confusionMatrix(data = svmPoly.pred, sdfAll_vali[,39])
# User accuracy (UA) and Producer accuracy (PA)
as.table(results)
mat <- as.matrix(results)
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")
UA <- diag(mat) /rowSums(mat) # User accuracy (UA)
PA <- diag(mat)/colSums(mat) # Producer accuracy (PA)


######################################
# In the next step, we will do the classification using any of the above algorithms such as RF or ERT or SVM
# Image class Prediction using Random Forest (RF) algorithm
pr.rf <- predict(img_merge_dis, rf.model_train, na.rm=TRUE)
writeRaster(pr.rf, filename = "C:\\Disturbance_map_year1year2_rf.tif", format="GTiff", overwrite=TRUE)

# Filter
Disturbance_map_year1year2_rf <- focal(pr.rf, w=matrix(1,7,7), fun=modal) # use focal function to reclassify the isolated pixels 
writeRaster(Disturbance_map_year1year2_rf, filename = "C:\\Disturbance_map_year1year2_rf_filter.tif", format="GTiff", overwrite=TRUE)

######################################################################################################################



