library(reshape)
library(plyr)
library(ggplot2)
library(lattice)

sitesWx <- read.csv("Data/Weather/Kruger_Stations1980_2008.csv")

sitesWx_long <- melt(sitesWx,id.vars="Year",variable_name="Station")
names(sitesWx_long)[3] <- "AnnualPrecip"

newWx <- read.csv("Data/Weather/monthly_rainfall_2006_2010_paf-moo-sat-pre.txt")
newWx_annual <- ddply(newWx,.(YEAR,STATION),summarize,AnnualPrecip = sum(SumOfMM))
names(newWx_annual) <- c("Year","Station","AnnualPrecip")

newWx_annual_subset <- subset(newWx_annual,Year >= 2009)

Kruger_Wx_Combined <- rbind(sitesWx_long,newWx_annual_subset)


MAP_STANDEV <- ddply(Kruger_Wx_Combined,.(Station),summarize,MAP = mean(AnnualPrecip,na.rm=TRUE),MAP_SD = sd(AnnualPrecip,na.rm=TRUE))


summary(lm(MAP_SD ~ MAP,MAP_STANDEV ))
