intermediateA1 <- krugerProbTop_High
intermediateB1 <- pEscape 

names(intermediateA1)[1] <- "MAP"
intermediateA2 <- intermediateA1[c("MAP","ProbTop")]
intermediateA2$Model <- "Objective1_Approach"
intermediateA2$ProbEscape <- 1 - intermediateA2$ProbTop
intermediateA2$ProbTop <- NULL

names(intermediateB1) <- c("MAP","ProbEscape")
intermediateB1$Model <- "Objective3_Approach"

comparePlotDF <- rbind(intermediateA2,intermediateB1)


escapePlotCompare <- ggplot(data = comparePlotDF, aes(x = MAP, y = ProbEscape, factor = Model, colour = Model))
escapePlotCompare +
  myTheme+
  theme(text = element_text(size = 35),
        legend.background = element_rect(),
        legend.position="bottom",legend.direction="vertical")+
  ylab("Probabilty of Escape")+
  scale_color_colorblind()+
  xlab("Mean Annual Precipitation (mm/yr)")+
  geom_point()