## @knitr loadEverything

library(ggplot2)
library(raster)
library(rgdal)
library(lubridate)
library(plyr)

crs.k <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs.m <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

FIRMS <- readOGR(dsn="/Users/danielg7/Documents/FIRMS/",layer="firms138621406915198_MCD14ML")
krugerOutline <- readOGR(dsn="/Users/danielg7/Documents/LandsatImagery/Boundary/",layer="boundary_kruger")
krugerAvgTmin_UTM <- raster(x="/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/krugerAvgTmin_UTM")
krugerMAP_UTM <- raster(x="/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/krugerMAP_UTM")
krugerFirelineIntensity_UTM <- raster(x="/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/krugerFireLineIntensity_UTM")
krugerGly <- readOGR(dsn="Data/",layer="KNP_GraniticAndBasaltic")

krugerWoodyCover <- raster(x="Data/WoodyCover/wcp_map_fin.tif")

krugerMAP_UTM <- resample(krugerMAP_UTM,krugerAvgTmin_UTM)
krugerGly_UTM <- spTransform(krugerGly,crs.k)
krugerFirelineIntensity_UTM <- resample(krugerFirelineIntensity_UTM,krugerAvgTmin_UTM)
krugerWoodyCover_UTM <- projectRaster(krugerWoodyCover,krugerAvgTmin_UTM)
krugerOverlayRaster <- raster(krugerWoodyCover_UTM)
krugerGly_UTM <- spTransform(krugerGly,crs.k)

krugerGlyRaster <- rasterize(krugerGly_UTM,krugerOverlayRaster)

names(krugerMAP_UTM) <- "MAP"
names(krugerFirelineIntensity_UTM) <- "Fireline_Intensity"
names(krugerWoodyCover_UTM) <- "WoodyCover"

FIRMS <- spTransform(FIRMS,crs.k)
krugerOutline <- spTransform(krugerOutline,crs.k)




## @knitr SubsetAndExtract

# Subset fire detections to just those of 95% confidence or more.
FIRMS_highConfidence <- subset(FIRMS,CONFIDENCE >= 95)

# Divide dates into wet and dry season.
FIRMS_highConfidence$ACQ_DATE <- ymd(as.character(FIRMS_highConfidence$ACQ_DATE))
FIRMS_highConfidence$Month <- month(FIRMS_highConfidence$ACQ_DATE)
FIRMS_Kruger_DrySeason <- subset(FIRMS_highConfidence,Month >= 7 & Month < 10)
FIRMS_Kruger_WetSeason <- subset(FIRMS_highConfidence,Month < 7 | Month > 10)

krugerBrick <- brick(krugerMAP_UTM,krugerAvgTmin_UTM,krugerFirelineIntensity_UTM,krugerWoodyCover_UTM,krugerGlyRaster$layer)
names(krugerBrick)[5] <- "Geology"

DrySeasonMAP <- raster::extract(krugerBrick,FIRMS_Kruger_DrySeason,method='bilinear',df=TRUE,sp=TRUE)
WetSeasonMAP <- raster::extract(krugerBrick,FIRMS_Kruger_WetSeason,method='bilinear',df=TRUE,sp=TRUE)

DrySeasonMAP <- as.data.frame(DrySeasonMAP)
WetSeasonMAP <- as.data.frame(WetSeasonMAP)

DrySeasonMAP$Season <- "Dry"
WetSeasonMAP$Season <- "Wet"

FRP_Variables <- rbind(DrySeasonMAP,WetSeasonMAP)
FRP_Variables$Geology <- as.factor(FRP_Variables$Geology)
levels(FRP_Variables$Geology)[9] <- "Basaltic"
levels(FRP_Variables$Geology)[1] <- "Granitic"
levels(FRP_Variables$Geology)[c(2,3,4,5,6,7,8)] <- "Other"

FRP_Variables$MAP_cut <- cut(FRP_Variables$MAP,breaks=seq(400,950,50))
levels(FRP_Variables$MAP_cut) <- seq(400,950,50)
FRP_Variables$MAP_cut <- as.character(FRP_Variables$MAP_cut)
FRP_Variables$MAP_cut <- as.numeric(FRP_Variables$MAP_cut)
FRP_Variables_agg <- ddply(FRP_Variables,.(MAP_cut),summarize,
                       WC = mean(WoodyCover,na.rm=TRUE),
                       WC_SE = sd(WoodyCover)/sqrt(length(WoodyCover)),
                       Intensity = mean(FRP,na.rm=TRUE),
                       Intensity_SE = sd(FRP)/sqrt(length(FRP)))
