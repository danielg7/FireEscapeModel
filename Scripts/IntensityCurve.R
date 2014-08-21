# Values reproduced from Figure 9.8 in Veld Burning in Ved Management in South Africa, W.S.W Trollope

TopKill <- c(.45,.50,.52,.58,.60,.65)
Intensity <- c(500,1000,2000,3000,4000,5000)

IntensityTopkillDF <- data.frame(TopKill,Intensity)

IntensityTopkill_lm <- lm(TopKill ~ Intensity, IntensityTopkillDF)
summary(IntensityTopkill_lm)

