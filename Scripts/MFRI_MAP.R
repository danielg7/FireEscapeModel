## @knitr loadEverything

library(ggplot2)
library(raster)
library(lattice)
library(rgdal)
library(lubridate)

projectAndCrop <- function(image,crs,maskOutline){
  projection(image) <- crs
  
  scratch <- crop(image,extent(maskOutline))
  scratch <- mask(scratch,maskOutline)
  
  return(scratch)
}

crs.k <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

krugerMAP_UTM <- raster(x="Data/Sites/Spatial/krugerMAP")
krugerOverlayRaster <- raster(krugerMAP_UTM)

krugerOutline <- readOGR(dsn="Data/Sites/Spatial/",layer="boundary_kruger")
krugerOutline_UTM <- spTransform(x=krugerOutline, CRSobj=crs.k)

#krugerMAP_UTM <- projectAndCrop(krugerMAP_UTM,crs.k,krugerOutline_UTM)
krugerMAP_UTM <- projectRaster(from = krugerMAP_UTM,crs = crs.k)




krugerFRI <- readOGR(dsn="Data/Sites/Spatial/FRI/",layer = "fire return interval_1941to2006")
krugerFRI_UTM <- spTransform(krugerFRI,crs.k)
#rm(krugerFRI)

krugerFRIRaster <- raster(krugerMAP_UTM)
krugerFRIRaster <- rasterize(x = krugerFRI_UTM,y = krugerFRIRaster,field=krugerFRI_UTM$MFRI)

krugerFRIRaster <- projectAndCrop(krugerFRIRaster,crs.k,krugerOutline_UTM)
krugerMAP_UTM <- projectAndCrop(krugerMAP_UTM,crs.k,krugerOutline_UTM)

krugerFRIBrick <- brick(krugerMAP_UTM,krugerFRIRaster)

krugerFRI_df <- na.omit(as.data.frame(krugerFRIBrick))
names(krugerFRI_df) <- c("MAP","MFRI")
row.names(krugerFRI_df) <- NULL

write.csv(krugerFRI_df,"Data/krugerFRIMAP.csv")

rm(krugerFRIBrick)
rm(krugerMAP_UTM)
rm(krugerFRIRaster)
rm(krugerFRI)
rm(krugerFRI_UTM)
rm(krugerMAP)
