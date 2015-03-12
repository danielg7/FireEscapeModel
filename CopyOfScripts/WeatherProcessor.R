library(reshape2)
library(plyr)
library(ggplot2)
library(lattice)
sitesWx <- read.csv("Data/Weather/Kruger_Stations1980_2008.csv")

sitesWx_long <- melt(sitesWx,id.vars="Year",variable_name="Station")
names(sitesWx_long)[3] <- "AnnualPrecip"
names(sitesWx_long)[2] <- "Station"

newWx <- read.csv("Data/Weather/monthly_rainfall_2006_2010_paf-moo-sat-pre.txt")
newWx_annual <- ddply(newWx,.(YEAR,STATION),summarize,AnnualPrecip = sum(SumOfMM))
names(newWx_annual) <- c("Year","Station","AnnualPrecip")

newWx_annual_subset <- subset(newWx_annual,Year >= 2009)

Kruger_Wx_Combined <- rbind(sitesWx_long,newWx_annual_subset)

lookupStation <- data.frame(Station = unique(Kruger_Wx_Combined$Station),Station_Long = c("Satara","Berg En Dal","Byamiti","Houtboschrand","Kingfisherspruit","Crocodile Bridge","Letaba","Mahlangeni","Malelane","Mooiplaas","Mopani","Nwanetsi","Olifants",
                                                                                          "Lower Sabie","Pafuri","Pafuri (Wenela)","Phalaborwa","Pretoriuskop","Punda Maria","Shangoni","Shingwedzi","Shimuwini","Sirheni","Skukuza","Stolznek","Talamati",
                                                                                          "Tshokwane","Vlakteplaas","Woodlands"))

Kruger_Wx_Combined <- merge(Kruger_Wx_Combined,lookupStation,by="Station")

rm(lookupStation)
rm(newWx_annual)
rm(newWx_annual_subset)
rm(sitesWx)
rm(sitesWx_long)


for(i in unique(Kruger_Wx_Combined$Station)){
  k <- subset(Kruger_Wx_Combined,Station == i)
  print(i)
  k <- na.omit(k)
  print(mean(k$AnnualPrecip))
  print(sd(k$AnnualPrecip))
}