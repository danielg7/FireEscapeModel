library(ggplot2)
library(ggthemes)
library(boot)
library(gridExtra)

myThemeBig <- theme_tufte() +
  theme(
    text = element_text(family="sans",size=32),
    axis.line = element_line(size = .3)
  )

myThemeMedium <- theme_tufte() +
  theme(
    text = element_text(family="sans",size=25),
    axis.line = element_line(size = .3)
  )

count <- 2000




# WC/FI relationship
FI_m <- -20/750
FI_m <- rnorm(n = 1,mean = FI_m,sd = .005)
FI_b <- 50

# WC/FRI relationship
FF_m <- 10/5 
FF_m <- rnorm(n = 1,mean = FF_m,sd = .005)

FF_b <- 2
FF_b <- rnorm(n = 1,mean = FF_b,sd = .005)

dummyDF <- data.frame(FI = rnorm(count,1000,sd = 500), FF = rnorm(count,5,5))
dummyDF$WC_FI <- FI_m * dummyDF$FI + FI_b
dummyDF$WC_FI <- dummyDF$WC_FI + rnorm(n = count,mean = 0,sd = 5)

dummyDF$WC_FF <- (FF_m * dummyDF$FF + FF_b)
dummyDF$WC_FF <- dummyDF$WC_FF + rnorm(n = count,mean = 0,sd = 5)


a_FI_pwc <- ggplot()+
  myThemeBig+
  theme(plot.title = element_text(size=12,
                                  hjust="0",
                                  family = "Arial",
                                  face="plain"),
        legend.position=c(.8,.9),legend.direction="vertical")+
  ggtitle("A.")+
  
  xlab("Average Fire Intensity")+
  ylab("Tree Cover")+
  theme(axis.ticks = element_blank(), axis.text = element_blank())+
  ylim(0,50)+
  xlim(0,2000)+
  #geom_point(data = dummyDF, size = 4, alpha = .10, aes(x = FI, y = WC_FI))+
  geom_abline(colour = "red", size = 2, slope = FI_m, intercept = FI_b)



b_FI_pwc <- ggplot()+
  myThemeBig+
  theme(plot.title = element_text(size=12,
                                  hjust="0",
                                  family = "Arial",
                                  face="plain"),
        legend.position=c(.8,.9),legend.direction="vertical")+
  ggtitle("B.")+
  theme(axis.ticks = element_blank(), axis.text = element_blank())+
  xlab("Fire Return Interval")+
  ylab("Tree Cover")+
  ylim(0,50)+
  xlim(0,20)+
 # geom_point(data = dummyDF, size = 4, alpha = .10, aes(x = FF, y = WC_FF))+
  geom_abline(colour="red", size = 2, slope = FF_m, intercept = FF_b)

grid.arrange(a_FI_pwc,b_FI_pwc,nrow=1)



MAP <- c(450,550,650,750,850)
MFRI <- c(5.0,5.3,5.2,2.8,2.1)
MFRI_MAP_df <- data.frame(MAP,MFRI)
lm_MAP <- lm(MFRI~MAP,MFRI_MAP_df)

coefficients(lm_MAP)

MFRI_MAP <- ggplot()+
  myThemeBig+
 # ggtitle("Fire Return Interval Decreases with Rainfall")+
  xlim(400,900)+
  xlab("\n")+
  ylim(2,10)+
  theme(axis.ticks = element_blank(), axis.text = element_blank())+
  ylab("Fire Frequency")+
 # geom_point()+
  geom_abline(colour="red", size = 2, slope = 2/250,
              intercept = 1)



Intensity_MAP <- ggplot()+
  myThemeBig+
 # ggtitle("Fire Intensity Increases with Rainfall")+
  xlim(400,900)+
  xlab("\nMean Annual Rainfall")+
  ylim(2,10)+
  ylab("Intensity")+
  theme(axis.ticks = element_blank(), axis.text = element_blank())+
  # geom_point()+
  geom_abline(colour="red", size = 2, slope = 2/250,
              intercept = 1)


Growth_MAP <- ggplot()+
  myThemeBig+
  #ggtitle("Tree Grow\nIncreases with Rainfall")+
  xlim(400,900)+
  xlab("\nRainfall")+
  ylim(2,10)+
  theme(axis.ticks = element_blank(), axis.text = element_blank())+
  ylab("Tree Growth Rates")+
  # geom_point()+
  geom_abline(colour="red", size = 2, slope = 2/250,
              intercept = 1)

grid.arrange(MFRI_MAP,Intensity_MAP,Growth_MAP, nrow = 1)

ProbTop_MAP <- ggplot()+
  myThemeBig+
  #ggtitle("Tree Grow\nIncreases with Rainfall")+
  xlim(400,900)+
  xlab("\n Rainfall")+
  ylim(2,10)+
  theme(axis.ticks = element_blank(), axis.text = element_blank())+
  ylab("Probability of Topkill")+
  # geom_point()+
  geom_abline(colour="red", size = 2, slope = 1/250,
              intercept = 1)

grid.arrange(Growth_MAP,ProbTop_MAP, nrow = 1)

