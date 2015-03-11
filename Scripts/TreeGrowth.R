
myTheme <- theme_tufte() +
  theme(
    text = element_text(family="Arial", size=16),
    axis.line = element_line(size = .3)
  )



Melon_Sites <- read.csv("Data/Sites/Mellon_Tree.csv")
Melon_Species <- read.csv("Data/MeasuredGrowth/MellonGrant/MellonGrant_Species.csv")
Melon_Growth <- read.csv("Data/MeasuredGrowth/MellonGrant/MellonGrant_TreeGrowth_LongForm.csv")
source("Scripts/WeatherProcessor.R")
PRET2011_RINGW_LongForm <- read.csv("Data/MeasuredGrowth/TrialProject/PRET2011_RINGW_LongForm.csv")


names(Melon_Sites)[2] <- "SAMPLE"

Melon_Master <- merge(Melon_Sites,Melon_Species, by="SAMPLE")
Melon_Master$Species <- NULL

Melon_Master <- merge(Melon_Master,Melon_Growth,by="SAMPLE")
Melon_Master$Diam <- NULL
Melon_Master$TTT <- NULL

PRET2011_RINGW_LongForm$Site <- "PRE"
PRET2011_RINGW_LongForm$SPECIES <-"Terminalia sericea"

Melon_Master <- rbind(Melon_Master,PRET2011_RINGW_LongForm)

Melon_Master$RINGW <- Melon_Master$RINGW/1000

Melon_Master <- Melon_Master[order(Melon_Master$SAMPLE,Melon_Master$YEAR),]

Melon_Master <- ddply(Melon_Master,.(SAMPLE),mutate,Growth = cumsum(RINGW))

Melon_Master$EstCrossSection <- (Melon_Master$Growth)^2 * pi
Melon_Master$EstBasalDiameter <- (Melon_Master$Growth) * 2

Melon_Master <- ddply(Melon_Master,.(SAMPLE),mutate,YearOfGrowth = YEAR - min(YEAR)+1)

Melon_AllWx <- merge(Melon_Master,Kruger_Wx_Combined,by.x=c("YEAR","Site"),by.y=c("Year","Station"))

MAP <- read.csv("Data/Weather/Kruger_MAP.csv")
Melon_AllWx <- merge(Melon_AllWx,MAP,by.x="Site",by.y="Station")

naive_growth <- ddply(Melon_AllWx,
                      .(SPECIES,Site,SAMPLE,MAP),
                      summarize,
                      TotalSize = sum(EstBasalDiameter),
                      Age = max(YearOfGrowth),
                      TotalLifeRain = sum(AnnualPrecip),
                      MeanLifeRain = mean(AnnualPrecip))

naive_growth$NaiveRate <- naive_growth$TotalSize/naive_growth$Age



COMO_naive_DF <- subset(naive_growth,SPECIES=="Colophospermum mopane")
COAP_naive_DF <- subset(naive_growth,SPECIES=="Combretum apiculatum")
TESE_naive_DF <- subset(naive_growth,SPECIES=="Terminalia sericea")

COMO_naiveGrowth <- lm(log(TotalSize) ~ 0 + log(Age),data=COMO_naive_DF)
TESE_naiveGrowth <- lm(log(TotalSize) ~ 0 + log(Age),data=TESE_naive_DF)
COAP_naiveGrowth <- lm(log(TotalSize) ~ 0 + log(Age),data=COAP_naive_DF)                       

#COMO_nls <- nls(TotalSize ~ (a + Age / b + Age), data = COMO_naive_DF, start=c(a = 100, b = 8))
#TESE_SS <- nls(TotalSize ~ SSasympOrig(input = TESE_naive_DF$Age,Asym = 20,lrc = 1),data = TESE_naive_DF)

COMO_plotDF <- expand.grid(Age=seq(0,20))
COMO_plotDF$TotalSize <- exp(predict(COMO_naiveGrowth,COMO_plotDF))

COAP_plotDF <- expand.grid(Age=seq(0,20))
COAP_plotDF$TotalSize <- exp(predict(COAP_naiveGrowth,COAP_plotDF))

TESE_plotDF <- expand.grid(Age=seq(0,20))
TESE_plotDF$TotalSize <- exp(predict(TESE_naiveGrowth,TESE_plotDF))

TESE_plotDF$TotalSize_SS <- predict(TESE_SS,TESE_plotDF)

TESE_plotDF$SPECIES <- "Terminalia sericea"
COMO_plotDF$SPECIES <- "Colophospermum mopane"
COAP_plotDF$SPECIES <- "Combretum apiculatum"


displayPlotted <- rbind(TESE_plotDF,COMO_plotDF)
displayPlotted <- rbind(displayPlotted,COAP_plotDF)

displayPlotted$Species <- as.factor(displayPlotted$SPECIES)
names(naive_growth)[1] <- "Species"
levels(naive_growth$Species)<- c("C. mopane","C. apiculatum","T. sericea")
levels(displayPlotted$Species)<- c("C. mopane","C. apiculatum","T. sericea")

displayPlotted_plot <- ggplot(data = displayPlotted, aes(x = Age, y=TotalSize,colour=Species))
displayPlotted_plot+
  myTheme+
  ylab("Ring Width (um)")+
  geom_point(data=naive_growth,aes(x = Age,y = TotalSize,color=Species))+
  geom_line()