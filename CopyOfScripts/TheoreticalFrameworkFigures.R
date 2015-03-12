library(ggplot2)
library(ggthemes)
library(gridExtra)
library(boot)


library(ggthemes)
myTheme <- theme_tufte() +
  theme(
    text = element_text(family="sans",size=10),
    axis.line = element_line(size = .3)
  )

a_map_pwc <- ggplot()+
  myTheme+
  coord_cartesian()+
  theme(plot.title = element_text(size=12,
                                  hjust="0",
                                  family = "Arial",
                                  face="plain"),
        legend.position=c(.8,.9),legend.direction="vertical")+
  ggtitle("A.")+
  xlab("Mean Annual Precipitation")+
  ylab("Woody Cover (%)")+
  ylim(0,50)+
  xlim(0,1000)+
  geom_point(data=krugerMAP_agg,aes(y = WoodyCover, x = MAP_cut), size=1)+
  geom_abline(slope = coef(WCMap)[2], intercept = coef(WCMap)[1])

b_map_fuel_accumulation <- ggplot()+
  myTheme+
  coord_cartesian()+
  theme(plot.title = element_text(size=12,
                                  hjust="0",
                                  family = "Arial",
                                  face="plain"),
        legend.position=c(.8,.9),legend.direction="vertical")+
  ggtitle("B.")+
  xlab("Mean Annual Precipitation")+
  ylab(expression(paste("Fuel Load (kg ha",{}^{-1},")")))+
  ylim(0,6000)+
  xlim(0,1000)+
  geom_abline(slope = 6000/1250, intercept = 0)

c_intensity_fuel_accumulation <- ggplot()+
  myTheme+
  coord_cartesian()+
  theme(plot.title = element_text(size=12,
                                  hjust="0",
                                  family = "Arial",
                                  face="plain"),
        legend.position=c(.8,.9),legend.direction="vertical")+
  ggtitle("C.")+
  xlab(expression(paste("Fuel Load (kg ha",{}^{-1},")")))+
  ylab("Fire Intensity")+
  ylim(0,3000)+
  xlim(0,6000)+
 # geom_abline(slope = 16890 * .1 * .04 / 1000, intercept = 0,linetype="dashed")+
  geom_abline(slope = 16890 * .1 * .38 / 1000, intercept = 0,linetype="solid")
 # +geom_abline(slope = 16890 * .1 * 1.22 / 1000, intercept = 0,linetype="dashed")
  

d_map_intensity <- ggplot()+
  myTheme+
  coord_cartesian()+
  theme(plot.title = element_text(size=12,
                                  hjust="0",
                                  family = "Arial",
                                  face="plain"),
        legend.position=c(.8,.9),legend.direction="vertical")+
  ggtitle("D.")+
  xlab("Mean Annual Precipitation")+
  ylab(expression(paste("Fire Intensity (kW ","m",{}^{-1},")")))+
  ylim(0,3000)+
  xlim(0,1000)+
  geom_abline(slope = 4.13, intercept = -558.22)

dummyDF <- data.frame(MAP = seq(0,5000,10))
dummyDF$Intensity <- dummyDF$MAP * 4.13 - 558.22
dummyDF$ProbTop <- inv.logit(-3.9 * log(2) + .05 * sqrt(dummyDF$Intensity) + .3 * 1)


e_map_intensity <- ggplot()+
  myTheme+
  coord_cartesian()+
  theme(plot.title = element_text(size=12,
                                  hjust="0",
                                  family = "Arial",
                                  face="plain"),
        legend.position=c(.8,.9),legend.direction="vertical")+
  ggtitle("E.")+
  xlab("Mean Annual Precipitation")+
  ylab("Probability of Top Kill")+
  ylim(0,1)+
  xlim(0,1000)+
  geom_line(data = dummyDF, aes(x = MAP, y = ProbTop))

grid.arrange(a_map_pwc,b_map_fuel_accumulation,c_intensity_fuel_accumulation,d_map_intensity,e_map_intensity,nrow=3)
  