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
# This script downloads and assembles a series of useful raster datasets for Kruger National Park
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
library(raster)
library(weatherData)
# Define working directories ----
DataFolder <- "/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data"
ScratchData <- "/Users/danielgodwin/Documents/"
# Functions ----
projectAndCrop <- function(image,crs,maskOutline){
  projection(image) <- crs
  
  scratch <- crop(image,extent(maskOutline))
  scratch <- mask(scratch,maskOutline)
  
  return(scratch)
}
# Read in data ----
krugerPrec1 <- getData(name="worldclim",var="prec",res=.5,lat=-24,lon=28)
krugerPrec2 <- getData(name="worldclim",var="prec",res=.5,lat=-24,lon=31)
krugertmin1 <- getData(name="worldclim",var="tmin",res=.5,lat=-24,lon=28)
krugertmin2 <- getData(name="worldclim",var="tmin",res=.5,lat=-24,lon=31)
# Mosaic data ----
krugerPrec <- mosaic(krugerPrec1,krugerPrec2,fun=max)
krugertmin <- mosaic(krugertmin1,krugertmin2,fun=max)

crs.k <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs.m <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

krugerOutline <- readOGR(dsn="/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/",layer="boundary_kruger")


krugerOutline_UTM <- spTransform(x=krugerOutline, CRSobj=crs.k)

krugerPrec_sub <- projectRaster(krugerPrec_sub,crs=crs.k)

krugerPrec_sub <- projectAndCrop(krugerPrec_sub,crs.k,krugerOutline_UTM)
krugerMAP <- sum(krugerPrec_sub$layer.1,krugerPrec_sub$layer.2,krugerPrec_sub$layer.3,krugerPrec_sub$layer.4,krugerPrec_sub$layer.5,krugerPrec_sub$layer.6,krugerPrec_sub$layer.7,krugerPrec_sub$layer.8,krugerPrec_sub$layer.9,krugerPrec_sub$layer.10,krugerPrec_sub$layer.11,krugerPrec_sub$layer.12)

# Clean up ----
rm(krugerPrec_sub)
rm(krugerPrec)
rm(krugerPrec1)
rm(krugerPrec2)

