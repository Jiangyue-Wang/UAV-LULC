### load libraries-------
library(terra)
library(sf)
library(tidyverse)

### read in files-----
suli <- sf::read_sf("downloaded-datasets/Suli/suli.kml")

dem_dir <- "downloaded-datasets/ALOS_dem/"
dem_files <- dir(dem_dir)


for(i in 1:length(dem_files)){
  assign(paste0("dem",str_pad(i,width = 2, side = "left", pad = "0")), rast(paste0(dem_dir, dem_files[i])))
}

### merge tiles and mask to --------
alos_dem <- merge(dem01, dem02, dem03, dem04, dem05, dem06, dem07, dem08, dem09, dem10, dem11, dem12, dem13)
alos_dem # 12.5m, projection UTM47N EPSG:32647
plot(alos_dem)
alos_dem_proj <- terra::project(alos_dem, suli)
suli_dem <- terra::mask(alos_dem_proj, suli)
plot(suli_dem)
writeRaster(suli_dem, "downloaded-datasets/DEM/suli_dem.tif")
