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
# This script loads biomass data from the VCA biomass data and combines it.
#
# Completeness: Incomplete
#
# Inputs: ----
# Sitedescriptions.txt
# 2010_2012Positions.txt
#
# Outputs: ----
# 
# 
#
# TODO:  ----
# Figure out WTH this data is all about. Srsly.

# Load Packages -----------------------------------------------------------

# Load Site Description ---------------------------------------------------

Biomass_SiteDesc <- read.delim(file = "/Users/danielg7/Downloads/Biomassdata89-2012/Sitedescriptions.txt",
                               header = FALSE)

names(Biomass_SiteDesc) <- c("SITE_NUMBER","SECTION","Desc","Easting","Northing","Longitude","Latitude")

Biomass_SiteDesc_20102012 <- read.delim(file = "/Users/danielg7/Downloads/Biomassdata89-2012/2010-2012Positions.txt",
                                        header = TRUE)

names(Biomass_SiteDesc_20102012)[3] <- "SITE_NUMBER"


# Load Biomass Data -------------------------------------------------------

Biomass_2007_2012 <- read.delim(file = "/Users/danielg7/Downloads/Biomassdata89-2012/2007-2012Biomass.txt",
                                        header = TRUE)

names(Biomass_2007_2012)[4] <- "SITE_NUMBER"

Biomass89_99 <- read.delim(file = "/Users/danielg7/Downloads/Biomassdata89-2012/Biomass89-99.txt",
                           header = FALSE)
names(Biomass89_99) <- c("StandingCrop","SITE_NUMBER")

