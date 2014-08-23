## @knitr loadEverything

library(ggplot2)
library(raster)
library(lattice)
library(rgdal)
library(lubridate)

crs.k <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

krugerMAP_UTM <- raster(x="/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/krugerMAP_UTM")
krugerGly <- readOGR(dsn="/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/",layer="KNP_GraniticAndBasaltic")
krugerGly_UTM <- spTransform(krugerGly,crs.k)

krugerOverlayRaster <- raster(krugerMAP_UTM)
krugerGlyRaster <- rasterize(krugerGly_UTM,krugerOverlayRaster)

krugerFRI <- readOGR(dsn="Data/FRI/",layer = "fire return interval_1941to2006")
krugerFRI_UTM <- spTransform(krugerFRI,crs.k)
rm(krugerFRI)

krugerFRIRaster <- raster(krugerMAP_UTM)

krugerFRIRaster <- rasterize(krugerFRI_UTM,krugerFRIRaster)

krugerFRIBrick <- brick(krugerFRIRaster,krugerMAP_UTM,krugerGlyRaster)

krugerFRI_df <- as.data.frame(krugerFRIBrick)
names(krugerFRI_df) <- c("ID","Count","MFRI","STDEV_FRI","MAP","Geology")
krugerFRI_df <- na.omit(krugerFRI_df)

krugerFRI_df$Geology

plot(MFRI ~ MAP,krugerFRI_df)
MFRI_MAP_lm <- lm(MFRI ~ MAP,krugerFRI_df)
