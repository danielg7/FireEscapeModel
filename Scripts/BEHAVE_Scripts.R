library(reshape)
library(lattice)

BEHAVE <- read.csv("Data/OutputsFromBehave_KNP.csv")
BEHAVE[7,3] <- 11602

names(BEHAVE) <- c("FuelLoad","ROS","FirelineIntensity","FlameLength","ReactionIntensity","ResidenceTime")

BEHAVE$ResidenceTime <- BEHAVE$ResidenceTime * 60

#BEHAVE <- melt(BEHAVE)
#names(BEHAVE) <- "F"

plot(FirelineIntensity ~ ReactionIntensity,BEHAVE)
Intensity_LM <- lm(FirelineIntensity ~ ReactionIntensity,BEHAVE)
abline(Intensity_LM)

qplot(data=BEHAVE, x=ResidenceTime,y=FirelineIntensity,fill=FuelLoad)

residenceTimePlot <- ggplot(data=BEHAVE,aes(x=ResidenceTime,y=FirelineIntensity,size=ROS,color=FuelLoad))
residenceTimePlot+
  myTheme+
  scale_color_continuous(high="red",low="green")+
  geom_point()