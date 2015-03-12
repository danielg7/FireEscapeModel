library(ggplot2)
library(ggthemes)

myTheme <- theme_tufte() +
  theme(
    text = element_text(family="sans",size=17),
    axis.line = element_line(size = .3)
  )

otherDF <- expand.grid(Intensity = c(20,430,1010,1630,2332,3158,4232,5760,8486,20000),W = seq(1,6500,1))

otherDF$TopKillClass <- as.factor(otherDF$Intensity)

levels(otherDF$TopKillClass) <- c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9",".99")

otherDF$RoS <- (otherDF$Intensity / 16890) / otherDF$W

Isoclines <- ggplot(data = otherDF, aes(x = log(RoS), y = log(W), group = Intensity, color=Intensity))
Isoclines+
  scale_color_gradient(expression(paste("Fireline Intensity (kW ","m",{}^{-1},")")),low="blue",high="red")+
  ylab("log Mass of Material Combusted (g)")+
  xlab(expression(paste("log Rate of Spread (m s",{}^{-1},")")))+
  geom_line()+
  #xlim(0,0.1)+
  #ylim(0,500)+
  myTheme
