gmednrfl1 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmednrfl1")
gmednrfl2 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmednrfl2")
gmednrfl3 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmednrfl3")
gmednrfl4 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmednrfl4")
gmednrfl5 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmednrfl5")
gmednrfl6 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmednrfl6")
gmednrfl7 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmednrfl7")
gmednrfl8 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmednrfl8")
gmednrfl9 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmednrfl9")
gmednrfl10 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmednrfl10")
gmednrfl11 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmednrfl11")
gmednrfl12 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmednrfl12")

krugerBoundary <- readOGR(dsn = "/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/",layer = "boundary_kruger")

gmed_ZA <- stack(x = c(gmednrfl1,gmednrfl2,gmednrfl3,gmednrfl4,gmednrfl5,gmednrfl6,gmednrfl7,gmednrfl8,gmednrfl9,gmednrfl10,gmednrfl11,gmednrfl12))

gmed_ZA_KNP <- crop(gmed_ZA,extent(krugerBoundary))
gmed_ZA_KNP <- mask(gmed_ZA_KNP,krugerBoundary)
sum_gmed_ZA_KNP <- sum(gmed_ZA_KNP)


gmap <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/gmap")
gmap_ZA_KNP <- crop(gmap,extent(krugerBoundary))
gmap_ZA_KNP <- mask(gmap_ZA_KNP,krugerBoundary)

gmap_ZA_KNP <- projectRaster(gmap_ZA_KNP,crs=crs.k)
