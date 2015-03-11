minRH01 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/minrh_d01")
minRH02 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/minrh_d02")
minRH03 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/minrh_d03")
minRH04 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/minrh_d04")
minRH05 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/minrh_d05")
minRH06 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/minrh_d06")
minRH07 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/minrh_d07")
minRH08 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/minrh_d08")
minRH09 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/minrh_d09")
minRH10 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/minrh_d10")
minRH11 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/minrh_d11")
minRH12 <- raster("/Users/danielg7/Documents/SA Atlas data Schulze et al 2008/South African Atlas of Climatology and Agrohydrology/Raster grids/minrh_d12")

krugerBoundary <- readOGR(dsn = "/Users/danielgodwin/Dropbox/Graduate School/Dissertation/Chapter 2 - Ignition Variation/SpatialDriversofTopKill/Data/",layer = "boundary_kruger")

minRH_ZA <- stack(x = c(minRH01,minRH02,minRH03,minRH04,minRH05,minRH06,minRH07,minRH08,minRH09,minRH10,minRH11,minRH12))

minRH_KNP <- crop(minRH_ZA,extent(krugerBoundary))
minRH_KNP <- mask(minRH_KNP,krugerBoundary)
minRH_KNP <- min(minRH_KNP)

crs.k <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
minRH_KNP_UTM <- projectRaster(from = minRH_KNP,crs = crs.k)



writeRaster(x = avg_minRH_KNP_UTM,filename = "Data/minRH_KNP_UTM",overwrite=TRUE)
