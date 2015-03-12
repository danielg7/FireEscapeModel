# Copyright notice: ----
# This script is provided with a Creative Commons - Attribution license, as defined on:
# http://creativecommons.org/licenses/by/3.0/us/
#
#
# Author Contact:
# Daniel Godwin
# danielg7@gmail.com
# Savanna Ecology Lab
# Division of Biological Sciences
# University of Missouri - Columbia
#
# Script Intent: ---
# This model simulates the growth of three common savanna tree species
#
# Completeness: Incomplete
#
# Inputs: ----
# Scripts:  TreeHeight.R
#           TreeGrowth.R
#           
# Data:     krugerFRI_df (A product of MFRI_MAP.df)
#
# Outputs: ----
# 
# 
#
# TODO:  ----
#        
#        
#
#        
# 
# Load required packages ----

library(ggplot2)
library(ggthemes)
library(boot)
library(scales)

myTheme <- theme_tufte() +
  theme(
    text = element_text(family="Arial",size=18),
    axis.line = element_line(size = .3)
  )

#
# Run ancillary analyses ----

setwd("/Users/danielgodwin/Dropbox/Graduate School/Dissertation/FireEscapeModel_Git/")
source('Scripts/TreeHeight.R', echo=FALSE)
source('Scripts/TreeGrowth.R', echo=FALSE)
#source('Scripts/MFRI_MAP.R',echo=FALSE)
krugerFRI_df <- read.csv("Data/krugerFRIMAP.csv")
krugerFRI_df$X <- NULL

# Create empty dataframes ----
  
  treeHeightList <- numeric(0)
  fireIntensityList <- numeric(0)
  treeHeightsByMAP <- list(0)
  intensityByMAP <- list(0)

# Define start conditions ----

  timeLimit <- 10
  treeNumbers <- 1000
  runs <- 5000

# Model ----

  for(u in 1:runs){
    
  randomLine <- sample(x = nrow(krugerFRI_df),size = 1, replace = FALSE)
  
  FireFrequency <- 1/krugerFRI_df[randomLine,2]
  
  TESE_vector_height <- rep(.1,treeNumbers)
  TESE_vector_age <- rep(1,treeNumbers)
  MAP_seed <- krugerFRI_df[randomLine,1]
  
  TimeSinceFire <- 0
 
  for(timeSkip in 1:timeLimit){
    

         MAP <- rnorm(1,mean=MAP_seed,sd=MAP_seed * 0.22294 + 107.76283)
         
         if(MAP <= 0){ 
           while(MAP <= 0) 
             MAP <- rnorm(1,mean=MAP_seed,sd=MAP_seed * 0.22294 + 107.76283)}
    
    
    
    TESE_vector_basalDiameter <- unlist(exp(predict(object = TESE_naiveGrowth,newdata = list(Age = TESE_vector_age))))
    #TESE_vector_basalDiameter <- exp((summary(TESE_naiveGrowth)$coef[1] + rnorm(length(TESE_vector_age),mean = unlist(exp(predict(object = TESE_naiveGrowth,newdata = list(Age = TESE_vector_age)))), summary(TESE_naiveGrowth)$coef[2])) * TESE_vector_age) 
    
    TESE_vector_height <- exp(0.63 * log(TESE_vector_basalDiameter) - 0.02)
    
    TESE_vector_height[which(TESE_vector_height <= 0)]


    burnTest <- rbinom(n = 1,
                       size = 1,
                       prob = FireFrequency) 
    if(burnTest == 0)
    {
      TimeSinceFire <- TimeSinceFire + 1
    }
    
    if(burnTest == 1)
    {
      
      FuelLoad <- 382.9 + 3.3 * MAP + 979.4 * TimeSinceFire - 0.001 * MAP^2 + 0.37*MAP*TimeSinceFire - 161.8*TimeSinceFire^2
      # Fuel loads calculated from Govender et al. 2006 Fig. 1
      
  #    RateOfSpread <- .38
  #    RelativeHumidity <- sample(seq(4,82,1),1) # Range values taken from Trollope 2002
  #    FuelMoisture <- 32 # Fuels assumed to take the value of RH immediately
  #    WindSpeed <- sample(seq(.3,6.7,.1),1) #(mean wind speed)
      
      RateOfSpread <- .38
      RelativeHumidity <- 4.2
      FuelMoisture <- 7.5
      WindSpeed <- 6.7
      
      FireIntensity = 2729 + 0.8684*FuelLoad - 530*sqrt(FuelMoisture) - 0.1907*RelativeHumidity^2 - 5961/WindSpeed # Trollope 2002?
         
      if(FireIntensity <= 0) FireIntensity <- 100
      
      fireIntensityList <- append(fireIntensityList,FireIntensity)
      # Fire Intensity calculated as I = Hwr, where I is fire intensity (kW/m),
      # H is heat yield (kJ/g), w is the mass of fuel combused (g/m^2))
      # and r is the rate of spread (m/s)
      
      # Heat yield is derived from Govender et al. 2006
      # r is estimated from values seen in Govender et al. 2006
      TESE_vector_topkills <- rbinom(n=length(TESE_vector_height),size=1,prob=(1 - (inv.logit(-3.9 * log(TESE_vector_height) + .05 * sqrt(FireIntensity) + .3 * 1))))
      TESE_vector_height <- TESE_vector_height * TESE_vector_topkills
      TESE_vector_height <- sort(TESE_vector_height)
      
      TESE_vector_age0 <- rep.int(0,length(TESE_vector_height[which(TESE_vector_height == 0 )]))
      TESE_vector_age1 <- rep.int(1,length(TESE_vector_age)-length(TESE_vector_age0))
      TESE_vector_ageMod <- append(TESE_vector_age0,TESE_vector_age1)
      TESE_vector_age <- TESE_vector_age * TESE_vector_ageMod
      
      TimeSinceFire <- 0
      
      
    }
    TESE_vector_age <- TESE_vector_age + 1
  }
  treeHeightList <- append(treeHeightList,TESE_vector_height)
  
  MAP_for_list <- as.character(MAP_seed)
  
  treeHeightsByMAP[[MAP_for_list]] <-treeHeightList
  intensityByMAP[[MAP_for_list]] <-fireIntensityList
  
  treeHeightList <- numeric(0)
  }
  



# Clean up output data ----

treeHeightsByMAP_df <- as.data.frame(treeHeightsByMAP)
treeHeightsByMAP_df$X0 <- NULL

treeHeightsByMAP_df_long <- melt(treeHeightsByMAP_df,variable_name = "MAP")
names(treeHeightsByMAP_df_long) <- c("MAP","Height")
treeHeightsByMAP_df_long$MAP <- as.numeric(as.character(substring(treeHeightsByMAP_df_long$MAP, 2)))

treeHeightsByMAP_df_long$Height_Cuts <- cut(x = treeHeightsByMAP_df_long$Height,
                                            breaks = seq(0,
                                                        ceiling(max(treeHeightsByMAP_df_long$Height)),
                                                         1),
                                            labels = FALSE)

treeHeightsByMAP_df_long$MAP_Cuts <- cut(x = treeHeightsByMAP_df_long$MAP,
                                         right = TRUE,
                                            breaks = seq(300,
                                                         900,
                                                         50))
levels(treeHeightsByMAP_df_long$MAP_Cuts) <- seq(300,900,50)
treeHeightsByMAP_df_long$MAP_Cuts <- as.character(treeHeightsByMAP_df_long$MAP_Cuts)
treeHeightsByMAP_df_long$MAP_Cuts <- as.numeric(treeHeightsByMAP_df_long$MAP_Cuts)


treeHeightsByMAP_df_long_summary <- ddply(.data = treeHeightsByMAP_df_long,.(MAP_Cuts,Height_Cuts),summarize,Count = length(Height_Cuts))
treeHeightsByMAP_df_long_summary <- ddply(.data = treeHeightsByMAP_df_long_summary,.(MAP_Cuts),mutate,Proportion = Count / sum(Count))

#treeHeightsByMAP_df_long_summary$MAP <- as.numeric(as.character(treeHeightsByMAP_df_long_summary$MAP))
#treeHeightsByMAP_df_long_summary <- na.omit(treeHeightsByMAP_df_long_summary)

intensityByMAP_df <- as.data.frame(melt(intensityByMAP))
intensityByMAP_df <- intensityByMAP_df[-1,]
names(intensityByMAP_df) <- c("intensity","MAP")
intensityByMAP_df$MAP <- as.numeric(intensityByMAP_df$MAP)

intensityByMAP_df$MAP_Cuts <- cut(x = intensityByMAP_df$MAP,
                                         right = TRUE,
                                         breaks = seq(300,
                                                      900,
                                                      50))
levels(intensityByMAP_df$MAP_Cuts) <- seq(300,900,50)
intensityByMAP_df$MAP_Cuts <- as.character(intensityByMAP_df$MAP_Cuts)
intensityByMAP_df$MAP_Cuts <- as.numeric(intensityByMAP_df$MAP_Cuts)

intensityByMAP_df$intensity_Cuts <- cut(x = intensityByMAP_df$intensity,
                                  right = TRUE,
                                  breaks = seq(0,
                                               7000,
                                               100))
levels(intensityByMAP_df$intensity_Cuts) <- seq(0,7000,100)
intensityByMAP_df$intensity_Cuts <- as.character(intensityByMAP_df$intensity_Cuts)
intensityByMAP_df$intensity_Cuts <- as.numeric(intensityByMAP_df$intensity_Cuts)


intensityByMAP_summary <- ddply(.data = intensityByMAP_df,.(MAP_Cuts,intensity_Cuts),summarize,Count = length(intensity_Cuts))
intensityByMAP_summary <- ddply(.data = intensityByMAP_summary,.(MAP_Cuts),mutate,Proportion = Count / sum(Count))

intensityByMAP_summary$MAP_Cuts <- as.factor(as.character(intensityByMAP_summary$MAP_Cuts))
intensityByMAP_summary$intensity_Cuts <- as.factor(as.character(intensityByMAP_summary$intensity_Cuts))



pEscape <- ddply(.data = treeHeightsByMAP_df_long,
                 .(MAP_Cuts),
                 summarise,
                 pResult = length(which(Height > 1)) / length(Height)
)

pEscape$MAP <- as.numeric(as.character(pEscape$MAP))

# Create descriptive figures ----

distributionsPlot <- ggplot(data=treeHeightsByMAP_df_long_summary,aes(x=as.numeric(as.character(Height_Cuts)),y = Proportion, fill=as.factor(MAP_Cuts),group=MAP_Cuts))
distributionsPlot+
  myTheme+
  scale_fill_discrete("Mean Annual Precipitation (mm/yr)")+
  theme(text = element_text(size = 35),
        legend.background = element_rect(),
        legend.position="bottom",legend.direction="vertical")+
  guides(col = guide_legend(nrow = 8))+
  ylab("Proportion")+
  xlab("Height (m)")+
  geom_bar(stat="identity",binwidth=1, position="dodge")

intensityPlot <- ggplot(data=intensityByMAP_summary,aes(x=intensity_Cuts, y = Proportion, group = MAP_Cuts,fill=MAP_Cuts))
intensityPlot+
  myTheme+
  scale_fill_colorblind("Mean Annual Precipitation (mm/yr)")+
  theme(text = element_text(size = 35),
        legend.background = element_rect(),
        legend.position=c(.8,.9),legend.direction="vertical")+
  ylab("Proportion")+
  xlab(expression(paste("Fireline Intensity (kJ ","s",{}^{-1},"m",{}^{-1},")")))+
  geom_bar(stat="identity", binwidth = 1, position="dodge")

intensityPlot <- ggplot(data=intensityByMAP_df,aes(y=intensity, x=as.factor(MAP_Cuts)))
intensityPlot+
  myTheme+
  coord_flip()+
  theme(text = element_text(size = 35),
        legend.background = element_rect(),
        legend.position=c(.8,.9),legend.direction="vertical")+
  xlab("Mean Annual Precipitation (mm/yr)")+
  ylab(expression(paste("Fireline Intensity (kJ ","s",{}^{-1},"m",{}^{-1},")")))+
  geom_boxplot(position="dodge")

escapePlot <- ggplot(data=pEscape,aes(x=MAP_Cuts, y=pResult))
escapePlot+
  myTheme+
  theme(text = element_text(size = 35),
        legend.background = element_rect(),
        legend.position=c(.8,.9),legend.direction="vertical")+
  ylab("Probabilty of Escape")+
  xlab("Mean Annual Precipitation (mm/yr)")+
  geom_point(size = 3)

 # geom_line()

