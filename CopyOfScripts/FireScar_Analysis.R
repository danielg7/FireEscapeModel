library(rgdal)
library(lubridate)

source("Scripts/WeatherProcessor.R")

Aggregated_FireScars <- readOGR(dsn="Data/BurnScars/", layer="agg2")
crs.k <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

Aggregated_FireScars$STARTDATE <- ymd(as.character(Aggregated_FireScars$STARTDATE))
Aggregated_FireScars$DATE_START <- ymd(as.character(Aggregated_FireScars$DATE_START))
Aggregated_FireScars$DATE_END <- ymd(as.character(Aggregated_FireScars$DATE_END))
Aggregated_FireScars <- spTransform(Aggregated_FireScars,crs.k)

FireScars_WoodyImpact <- subset(Aggregated_FireScars,!is.na(WOODYIMPAC))

krugerMAP_UTM <- raster(x="/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/krugerMAP_UTM")
krugerWoodyCover <- raster(x="Data/WoodyCover/wcp_map_fin.tif")
krugerWoodyCover_UTM <- projectRaster(krugerWoodyCover,krugerMAP_UTM)
names(krugerWoodyCover_UTM) <- "WoodyCover"


krugerFireScarAnalysis_brick <- brick(krugerMAP_UTM,krugerWoodyCover_UTM)



extractedFireScars <- raster::extract(x = krugerFireScarAnalysis_brick,
                              y = FireScars_WoodyImpact,
                              fun = mean,
                              sp = TRUE)

extractedFireScars_df <- as.data.frame(extractedFireScars)

extractedFireScars_df <- subset(extractedFireScars_df,WoodyCover >= 0)

extractedFireScarsAgg_df <- ddply(extractedFireScars_df,.(FIREID),transform,
                                  IGNITIONSE = IGNITIONSE,
                                  STARTDATE = STARTDATE,
                                  HERBACEOUS = HERBACEOUS,
                                  GENERALOBS = GENERALOBS,
                                  AREA_HA = sum(AREA_HA),
                                  MAP = mean(layer,na.rm = TRUE),
                                  WoodyCover = mean(WoodyCover,na.rm = TRUE))

extractedFireScarsAgg_df <-subset(extractedFireScarsAgg_df, !is.na(layer))
extractedFireScarsAgg_df$layer <- NULL
extractedFireScarsAgg_df <- extractedFireScarsAgg_df[!duplicated(extractedFireScarsAgg_df),]

extractedFireScars_wx_df <- merge(extractedFireScarsAgg_df,Kruger_Wx_Combined,by.x = "IGNITIONSE", by.y = "Station_Long")

extractedFireScars_wx_df$Year <- year(as.Date(as.character(extractedFireScars_wx_df$Year),format="%Y"))


testPlied <- ddply(extractedFireScars_wx_df,
                   .(FIREID),
                   mutate,
                   PreviousYears = year(STARTDATE) - 2)

testPlied <- ddply(testPlied,
                   .(FIREID),
                   subset,
                   Year > PreviousYears & Year <= year(STARTDATE)
                  )
aggPlied <- ddply(testPlied,"FIREID",summarise,
                   PreviousMAP = mean(AnnualPrecip))

firescar_final <- merge(extractedFireScarsAgg_df,aggPlied,by="FIREID")
firescar_final$Year <- year(as.Date(as.character(firescar_final$STARTDATE),format="%Y"))


firescar_final$WOODYIMPAC <- factor(firescar_final$WOODYIMPAC,levels(firescar_final$WOODYIMPAC)[c(2,4,1,3,5)])
levels(firescar_final$HERBACEOUS) <- c("Clean","Clean","Moderately clean","Patchy","Very patchy")
#firescar_final$HERBACEOUS <- factor(firescar_final$HERBACEOUS,levels(firescar_final$HERBACEOUS)[c(2,4,1,3,5)])

firescar_final_dry <- subset(firescar_final,month(STARTDATE) <= 7 & month(STARTDATE) < 10)
firescar_final_wet <- subset(firescar_final,month(STARTDATE) >= 7 | month(STARTDATE) > 10)

firescar_final_dry$Season <- "Dry"
firescar_final_wet$Season <- "Wet"

firescar_final <- rbind(firescar_final_dry,firescar_final_wet)
firescar_final$Season <- as.factor(firescar_final$Season)



firescar_final$WoodyImpactLevel <- firescar_final$WOODYIMPAC
levels(firescar_final$WoodyImpactLevel) <- c(0,1,2,3,4)
firescar_final$WoodyImpactLevel <- as.numeric(firescar_final$WoodyImpactLevel)

Impact_null <- glm(WoodyImpactLevel ~ 1,data=firescar_final)
Impact_season <- glm(WoodyImpactLevel ~ Season, data = firescar_final)
Impact_MAP <- glm(WoodyImpactLevel ~ MAP, data = firescar_final)
Impact_CAUSE <- glm(WoodyImpactLevel ~ CAUSE, data = firescar_final)
Impact_WC <- glm(WoodyImpactLevel ~ WoodyCover, data = firescar_final)

rm(firescar_final_dry)
rm(firescar_final_wet)
(aggPlied)
(testPlied)
rm(extractedFireScars)
rm(extractedFireScars_df)
(extractedFireScars_wx_df)
rm(FireScars_WoodyImpact)
rm(krugerFireScarAnalysis_brick)


library(ggplot2)
library(ggthemes)

myTheme_big <- theme_tufte() +
  theme(
    text = element_text(family="sans",size=17),
    axis.line = element_line(size = .3)
  )

tileTable <- ddply(.data=firescar_final,
                            .(HERBACEOUS,WOODYIMPAC,CAUSE),
                            summarize,
                   Area = sum(AREA_HA),
                            Count = length(WOODYIMPAC))

tileTable$CountPercent = tileTable$Count/sum(tileTable$Count)
tileTable$AreaPercent = tileTable$Area/sum(tileTable$Area)

Tile <- ggplot(aes(x=HERBACEOUS,y=WOODYIMPAC,fill=AreaPercent),data=tileTable)
  Tile+
  scale_fill_continuous("Percent",high = "red",low="darkblue")+
  ylab("Impact on Woody Cover")+
  xlab("\nImpact on Herbaceous Material")+
  geom_tile()+
  myTheme_big

TileCause <- ggplot(aes(x=CAUSE,y=WOODYIMPAC,fill=AreaPercent),data=tileTable)
TileCause+
  scale_fill_continuous("Percent",high = "darkred",low="darkblue")+
  ylab("Impact on Woody Cover")+
  xlab("\nCause")+
  geom_tile()+
  myTheme_big


yearTable <- ddply(.data=firescar_final,
                   .(HERBACEOUS,WOODYIMPAC,Year),
                   summarize,
                   WoodyCount = length(WOODYIMPAC),
                   HerbCount = length(HERBACEOUS))