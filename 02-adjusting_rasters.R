library(tidyverse)
library(raster)
library(sf)

resample.rasters <- function(features){
        
        dir.create("data/features/adjusted/", showWarnings = F)
        path="data/features/adjusted/"
        control <- raster::raster("data/features/Presence/coldcorals.tif")
        r <- raster::raster(features)
        crs(r) <- "+proj=longlat +datum=WGS84 +no_defs"
        r_adj <- raster::resample(r, control)
        

                
  raster::writeRaster(r_adj, paste0( str_remove( paste0(path, str_remove(features, "data/features/Presence/")), ".tif"),
                                                   "_adj.tif"),
                                    overwrite=TRUE)
        
}










normalize.rasters <- function(features){
        dir.create("data/features/adjusted/", showWarnings = F)
        path="data/features/adjusted/"
        control <- raster::raster("data/features/Presence/coldcorals.tif")
        r <- raster::raster(features)
        crs(r) <- "+proj=longlat +datum=WGS84 +no_defs"
        r_adj <- raster::resample(r, control)
          r <- round(r_adj, 4)
                norm <- (r- r@data@min)/ (r@data@max- r@data@min )
                
                raster::writeRaster(norm, paste0( str_remove( paste0(path, str_remove(features, "data/features/Indexes/")), ".tif"),
                                                  "_adj.tif"),
                                    overwrite=TRUE)
        
}




features <- list.files("data/features/Presence/" , pattern = ".tif", recursive = T, full.names = T)

lapply(features, resample.rasters)




features <- list.files("data/features/Indexes/" , pattern = ".tif", recursive = T, full.names = T)

lapply(features, normalize.rasters)








