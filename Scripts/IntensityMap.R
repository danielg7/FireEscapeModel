library(gstat)
library(ggplot2)

library(raster)
library(lattice)
library(rgdal)
library(lubridate)

crs.k <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs.m <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

FIRMS <- readOGR(dsn="/Users/danielg7/Documents/FIRMS/",layer="firms138621406915198_MCD14ML")
krugerOutline <- readOGR(dsn="/Users/danielg7/Documents/LandsatImagery/Boundary/",layer="boundary_kruger")
krugerAvgTmin_UTM <- raster(x="/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/krugerAvgTmin_UTM")
krugerMAP_UTM <- raster(x="/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/krugerMAP_UTM")
krugerFirelineIntensity_UTM <- raster(x="/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/krugerFireLineIntensity_UTM")

krugerMAP_UTM <- resample(krugerMAP_UTM,krugerAvgTmin_UTM)
krugerFirelineIntensity_UTM <- resample(krugerFirelineIntensity_UTM,krugerAvgTmin_UTM)
names(krugerMAP_UTM) <- "MAP"
names(krugerFirelineIntensity_UTM) <- "Fireline_Intensity"

FIRMS <- spTransform(FIRMS,crs.k)
krugerOutline <- spTransform(krugerOutline,crs.k)

FIRMS_highConfidence <- subset(FIRMS,CONFIDENCE >= 95)

#FIRMS_Kruger_inter <- over(FIRMS_highConfidence,krugerOutline)
#FIRMS_Kruger <- subset(FIRMS_highConfidence,!is.na(FIRMS_Kruger_inter[,1]))

FIRMS_highConfidence$ACQ_DATE <- ymd(as.character(FIRMS_highConfidence$ACQ_DATE))
FIRMS_highConfidence$Month <- month(FIRMS_highConfidence$ACQ_DATE)
FIRMS_Kruger_DrySeason <- subset(FIRMS_highConfidence,Month >= 7 & Month < 10)
FIRMS_Kruger_WetSeason <- subset(FIRMS_highConfidence,Month < 7 | Month > 10)

krugerBrick <- brick(krugerMAP_UTM,krugerAvgTmin_UTM,krugerFirelineIntensity_UTM)

DrySeasonMAP <- extract(krugerBrick,FIRMS_Kruger_DrySeason,method='bilinear',df=TRUE,sp=TRUE)
WetSeasonMAP <- extract(krugerBrick,FIRMS_Kruger_WetSeason,method='bilinear',df=TRUE,sp=TRUE)

DrySeasonMAP <- as.data.frame(DrySeasonMAP)
WetSeasonMAP <- as.data.frame(WetSeasonMAP)

DrySeasonMAP$Season <- "Dry"
WetSeasonMAP$Season <- "Wet"

FRP_Variables <- rbind(DrySeasonMAP,WetSeasonMAP)

head(FRP_Variables)

FRP_Variables$FRP_kwm <- FRP_Variables$FRP / 1000

FRP_Variables$ConvertedIntensity <- predict(Intensity_LM,list(ReactionIntensity=FRP_Variables$FRP))
xyplot(FRP ~ MAP | Season,FRP_Variables)

bwplot(FRP ~ Season,FRP_Variables)

kruskal.test(formula=FRP~Season,FRP_Variables)
summary(subset(FRP_Variables,Season == "Dry")$FRP)
summary(subset(FRP_Variables,Season == "Wet")$FRP)

fireSeasonMap <- ggplot(data=FRP_Variables,aes(x=MAP,y=FRP,color=Season))
fireSeasonMap+
  myTheme+
  ylab("Fire Radiative Power (MW / km^2)")+
  xlab("Mean Annual Precipitation (mm / yr)")+
  scale_color_colorblind()+
  geom_point(alpha=.75)

FRP_Cut <- FRP_Variables
FRP_Cut$MAPRange <- cut(FRP_Variables$MAP,breaks=seq(0,1000,by=50),ordered_result=TRUE)
FRP_Cut <- na.omit(FRP_Cut)

fireSeasonMapCut <- ggplot(data=FRP_Cut,aes(x=MAPRange,y=FRP,fill=Season,factor=Season))
fireSeasonMapCut+
  myTheme+
  ylab("Fire Radiative Power (MW / km^2)")+
  xlab("Mean Annual Precipitation (mm / yr)")+
  scale_x_discrete(labels=c("400 - 450","450 - 500","500 - 550","550 - 600","600 - 650","650 - 700","700 - 750","750 - 800","800 - 850","850 - 900"))+
  scale_color_colorblind()+
  geom_boxplot(position="dodge")