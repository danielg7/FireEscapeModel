## @knitr loadEverything

library(ggplot2)
library(raster)
library(lattice)
library(plyr)
library(rgdal)
library(lubridate)
library(boot)

crs.k <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

krugerWoodyCover <- raster(x="Data/WoodyCover/wcp_map_fin.tif")
krugerMAP_UTM <- raster(x="Data/krugerMAP_UTM")
#krugerGly <- readOGR(dsn="Data/",layer="KNP_GraniticAndBasaltic")
#krugerGly_UTM <- spTransform(krugerGly,crs.k)
krugerWoodyCover_UTM <- projectRaster(krugerWoodyCover,krugerMAP_UTM)

krugerOverlayRaster <- raster(krugerMAP_UTM)
krugerGlyRaster <- rasterize(krugerGly_UTM,krugerOverlayRaster)

#krugerFRI <- readOGR(dsn="Data/FRI/",layer = "fire return interval_1941to2006")
#krugerFRI_UTM <- spTransform(krugerFRI,crs.k)
#rm(krugerFRI)

#krugerFRIRaster <- raster(krugerMAP_UTM)

#krugerFRIRaster <- rasterize(krugerFRI_UTM,krugerFRIRaster)


krugerFRIBrick <- brick(krugerMAP_UTM,krugerGlyRaster,krugerWoodyCover_UTM)

krugerFRI_df <- as.data.frame(krugerFRIBrick)


names(krugerFRI_df) <- c("MAP","Geology","WoodyCover")
krugerFRI_df <- na.omit(krugerFRI_df)



krugerFRI_df$MAP_cut <- cut(krugerFRI_df$MAP,breaks=seq(400,950,50))
levels(krugerFRI_df$MAP_cut) <- seq(400,950,50)
krugerFRI_df$MAP_cut <- as.character(krugerFRI_df$MAP_cut)
krugerFRI_df$MAP_cut <- as.numeric(krugerFRI_df$MAP_cut)
krugerFRI_df <- subset(krugerFRI_df,WoodyCover >= 0)
krugerMAP_agg <- ddply(krugerFRI_df,.(MAP_cut),summarize,
                       WC = mean(WoodyCover,na.rm=FALSE),
                       WC_SE = sd(WoodyCover)/sqrt(length(WoodyCover)))
names(krugerMAP_agg)[2] <- "WoodyCover"

xyplot(WoodyCover ~ MAP_cut,krugerMAP_agg)

WCMap <- lm(WoodyCover ~ MAP_cut,krugerMAP_agg)

plot(WoodyCover ~ MAP_cut,krugerMAP_agg)
abline(lm(WoodyCover ~ MAP_cut,krugerMAP_agg))
