# Date: 8th April 2022
# Name: Yue Zhao
# Project: Exploration Image Classification in Python and ArcGIS (R version) #nolint

library(sp)
library(raster)
library(rgdal)
library(sf)

getwd()
setwd('C:\\Users\\zhaoy6429\\Documents\\ESRI exploration') #nolint

LC_B01_path<-'.\\04ArcGISProject\\Landsat9ImageClaasification\\LC09_band1.tif' #nolint
LC_B02_path<-'.\\04ArcGISProject\\Landsat9ImageClaasification\\LC09_band2.tif' #nolint
LC_B03_path<-'.\\04ArcGISProject\\Landsat9ImageClaasification\\LC09_band3.tif' #nolint
LC_B04_path<-'.\\04ArcGISProject\\Landsat9ImageClaasification\\LC09_band4.tif' #nolint
LC_B05_path<-'.\\04ArcGISProject\\Landsat9ImageClaasification\\LC09_band5.tif' #nolint
LC_B06_path<-'.\\04ArcGISProject\\Landsat9ImageClaasification\\LC09_band6.tif' #nolint
LC_B07_path<-'.\\04ArcGISProject\\Landsat9ImageClaasification\\LC09_band7.tif' #nolint

LC_B01 <- raster(LC_B01_path)
LC_B02 <- raster(LC_B02_path)
LC_B03 <- raster(LC_B03_path)
LC_B04 <- raster(LC_B04_path)
LC_B05 <- raster(LC_B05_path)
LC_B06 <- raster(LC_B06_path)
LC_B07 <- raster(LC_B07_path)

plot(LC_B07)
summary(LC_B01)

raterstack <- addLayer(LC_B01,LC_B02,LC_B03,LC_B04,LC_B05,LC_B06,LC_B07)
raterstack

## Some basic statistics using cellStats() from the raster package
cellStats(raterstack$LC09_band2, stat = max)
hist(raterstack)

pairs(raterstack, maxpixels  =1000)

par(mfrow = c(1, 1)) # reset plotting window
pal <- colorRampPalette(c("blue", "red", "green"))
ndvi <- overlay(raterstack$LC09_band5, raterstack$LC09_band4, fun=function(x,y){(x-y)/(x+y)})
plot(ndvi, col = pal(50))

Gelderland_recal <- calc(raterstack, fun  = function(x) x / 10000)
plot(Gelderland_recal)
plot(raterstack)

all_layer <- stack(Gelderland_recal, ndvi)
plot(all_layer)

names(all_layer) <- c(names(all_layer), "NDVI")
names(all_layer)

training_polygon <- st_read('.\\02Data\\processed\\training_set.csv')
training_polygon <- read_sf('.\\02Data\\processed\\Traning_set.shp')
summary(training_polygon$Classname)
training_polygon$Classname <- as.factor(training_polygon$Classname)
summary(training_polygon$Classname)

## Plotting
# Define a colour scale for the classes (as above)
# corresponding to: cropland, forest, wetland
cols <- c("grey", "green", "yellow", "blue")
### Superimpose training polygons (colour by class) onto NDVI plot
plot(ndvi,col = pal(50))
plot(training_polygon["Classname"], add = TRUE, pal=cols)
## Add a customised legend
legend("topright", legend=c("Builtup", "Forest", "Planted / Cultivated","Water"), fill=cols, bg="white")


## Extract pixel values below the polygons
trainingData<-extract(all_layer, st_zm(training_polygon))
# Check structure
str(trainingData)

# Convert each matrix to a data.frame and add the class column from the polygons
valuetable <- lapply(1:length(trainingData), function(polygonID) cbind.data.frame(trainingData[[polygonID]], Class=training_polygon$Classname[polygonID]))

# Unlist into one long data.frame
valuetable <- do.call(rbind, valuetable) # pixel level
head(valuetable, n = 10)
tail(valuetable, n = 10)

val_builtup <- subset(valuetable, Class == "Builtup")
val_forest <- subset(valuetable, Class == "Forest")
val_planted_ciltivated <- subset(valuetable, Class == "Planted / Cultivated")
val_water <- subset(valuetable, Class == "Water")

#NDVI
par(mfrow = c(4, 1))
hist(val_builtup$layer, main = "Builtup", xlab = "NDVI", xlim = c(0, 1), col = "grey")
hist(val_forest$layer, main = "Forest", xlab = "NDVI", xlim = c(0, 1),  col = "green")
hist(val_planted_ciltivated$layer, main = "Planted / Cultivated", xlab = "NDVI", xlim = c(0, 1), col = "yellow")
hist(val_water$layer, main = "Water", xlab = "NDVI", xlim = c(0, 1),  col = "blue")

#Construct a random forest model
library(ranger)
modelRF <- ranger(x=valuetable[, 1:ncol(valuetable)-1], y=valuetable$Class,
                  importance = "permutation", seed=0xfedbeef)

modelRF
class(modelRF)
str(modelRF)
names(modelRF)
## Inspect the confusion matrix of the OOB error assessment
modelRF$confusion.matrix
importance(modelRF)

# double the name of all_layer and valuetable
names(all_layer)
names(valuetable)

## Predict land cover using the RF model
predLC <-raster::predict(all_layer, modelRF, fun=function(...) predict(...)$predictions)

## Plot the results
par(mfrow = c(1, 1))
cols <- c("grey", "green", "yellow", "blue")
plot(predLC, col=cols, legend=FALSE)
legend("bottomright",
       legend=c("Builtup", "Forest", "Planted / Cultivated","Water"),
       fill=cols, bg="white")

print(modelRF)
