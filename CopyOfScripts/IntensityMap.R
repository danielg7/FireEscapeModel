# Copyright notice: ----
# This script is provided with a Creative Commons - Attribution license, as defined on:
# http://creativecommons.org/licenses/by/3.0/us/
#
#
# Author Contact:
# Daniel Godwin
# danielg7@gmail.com
# Savanna Ecology Lab
# Division of Biological Sciences
# University of Missouri - Columbia
#
# Script Intent: ---
# This script models fire intensity across Kruger National Park.
#
# Completeness: Incomplete
#
# Inputs: ----
# 
#
#
# Outputs: ----
# 
# 
#
# TODO:  ----
#        
#        
#
#        
# 
# Load required packages ----
library(ggplot2)
library(raster)
library(rgdal)
library(lubridate)

crs.k <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

krugerAvgTmin_UTM <- raster(x="/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/krugerAvgTmin_UTM")
krugerMAP_UTM <- raster(x="/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/krugerMAP_UTM")
minRH_KNP_UTM <- raster(x='/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/minRH_KNP_UTM')

krugerFRI <- readOGR(dsn="Data/FRI/",layer = "fire return interval_1941to2006")
krugerFRI_UTM <- spTransform(krugerFRI,crs.k)
krugerFRI_UTM <- subset(krugerFRI_UTM,MFRI <= 6)

krugerFRIRaster <- raster(krugerMAP_UTM)

source("Scripts/GMED.R")

krugerMAP_UTM <- resample(x=gmap_ZA_KNP,y=krugerFRIRaster)

minRH_KNP_UTM <- resample(x = minRH_KNP_UTM,y = krugerFRIRaster)
krugerFRIRaster <- rasterize(krugerFRI_UTM,krugerFRIRaster,field=krugerFRI_UTM$MFRI)



krugerBrick <- brick(krugerMAP_UTM,krugerFRIRaster,minRH_KNP_UTM)
names(krugerBrick) <- c("MAP","MFRI","RH")


krugerFuelLoad <- 382.9 + 3.3 * krugerBrick$MAP + 979.4 * krugerBrick$MFRI - 0.001 * krugerBrick$MFRI^2 + 0.37*krugerBrick$MAP*krugerBrick$MFRI - 161.8*krugerBrick$MFRI^2

RelativeHumidity <- c(4.2,36.6,82) # Range values taken from Trollope 2002
FuelMoisture <- c(7.5,32.1,68.8) # Fuels assumed to take the value of RH immediately
WindSpeed <- c(6.7,2.6,0.3)

krugerFirelineIntensity <- 2729 + 0.8684*krugerFuelLoad - 530*sqrt(FuelMoisture[1]) - 0.1907*krugerBrick$RH^2 - 5961/WindSpeed[1]
krugerFirelineIntensity$Average <- 2729 + 0.8684*krugerFuelLoad - 530*sqrt(FuelMoisture[2]) - 0.1907*RelativeHumidity[2]^2 - 5961/WindSpeed[2]

names(krugerFirelineIntensity)[1] <- "High"

krugerWoodyCover <- raster(x="Data/WoodyCover/wcp_map_fin.tif")


krugerWoodyCover_UTM <- projectRaster(krugerWoodyCover,krugerAvgTmin_UTM)
names(krugerWoodyCover_UTM) <- "WoodyCover"

krugerWoodyCover_newExtent <- resample(krugerWoodyCover_UTM,krugerFirelineIntensity)
krugerGlyRaster_newExtent <- resample(krugerGlyRaster,krugerFirelineIntensity)


krugerIntensityInvestigation <- brick(krugerWoodyCover_newExtent,krugerFirelineIntensity,krugerGlyRaster_newExtent,krugerMAP_UTM)

krugerIntensityInvestigationDF <- na.omit(as.data.frame(krugerIntensityInvestigation))
names(krugerIntensityInvestigationDF) <- c("WoodyCover","FirelineIntensity_High","FirelineIntensity_Average","Geology","MAP")

krugerIntensityInvestigationDF$Geology <- as.factor(krugerIntensityInvestigationDF$Geology)
levels(krugerIntensityInvestigationDF$Geology) <- c("Granitic","Basaltic")

