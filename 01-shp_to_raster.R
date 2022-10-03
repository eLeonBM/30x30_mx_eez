library(tidyverse)
library(sf)
library(raster)

        #Raster shp
        pu <- read_sf("shp/pu_lockedin.shp") %>% 
                st_transform(., crs=4326)
        
        
        ext <- raster::extent(pu)
        gridsize <- 0.1
        r <- raster::raster(ext, res=gridsize)
#Indice de desarrollo humano        
        y <- read_sf("data/raw_shp/ind_desarrollo_humano2010.shp") %>% 
        mutate(Constant=1)
        
        rast <- raster::rasterize(y, r, field= "IDH10")
        crs(rast) <- "+proj=longlat"
        plot(rast)

        raster::writeRaster(rast, "data/features/indice_desarrollo_humano2010.tif", overwrite=T)
        

#Indice de rezago social       
        y <- read_sf("data/raw_shp/ind_rezago_social2010.shp") %>% 
                mutate(Constant=1)
        head(y)
        
        rast <- raster::rasterize(y, r, field= "IRS2010")
        crs(rast) <- "+proj=longlat"
        plot(rast)
   
        raster::writeRaster(rast, "data/features/indice_rezago_social_2010.tif", overwrite=T)

        
#Ingresos  
        # 1 Salario minimo
        y <- read_sf("data/raw_shp/ingresos2010.shp") %>% 
                mutate(Constant=1)
        head(y)
        
        rast <- raster::rasterize(y, r, field= "P_1SM10")
        crs(rast) <- "+proj=longlat"
        plot(rast)
        
        raster::writeRaster(rast, "data/features/ingresos_menores_1_salario_minimo_2010.tif", overwrite=T)
        
        #1 o 2 salarios minimos
        head(y)
        
        rast <- raster::rasterize(y, r, field= "P1_2SM10")
        crs(rast) <- "+proj=longlat"
        plot(rast)
        
        raster::writeRaster(rast, "data/features/ingresos_1-2_salarios_minimos_2010.tif", overwrite=T)
        
        # Mas de 2 salarios minimos
        head(y)
        
        rast <- raster::rasterize(y, r, field= "P2_5SM10")
        crs(rast) <- "+proj=longlat"
        plot(rast)
        
        raster::writeRaster(rast, "data/features/ingresos_mayores_2_salarios_minimos_2010.tif", overwrite=T)
        
#Licencias de extraccion      
        y <- read_sf("data/raw_shp/Licencia.shp") %>% 
                mutate(Constant=1)
        head(y)
        
        rast <- raster::rasterize(y, r, field= "Constant")
        crs(rast) <- "+proj=longlat"
        plot(rast)
        
        
        
        raster::writeRaster(rast, "data/features/licencia_extraccion.tif", overwrite=T)

        
#Indice de marginación social
        
        y <- read_sf("data/raw_shp/marginacion2010.shp") %>% 
                mutate(Constant=1) %>% 
                filter(IND_MARG != 0)
        head(y)
        
        rast <- raster::rasterize(y, r, field= "IND_MARG")
        crs(rast) <- "+proj=longlat"
        plot(rast)
        
        raster::writeRaster(rast, "data/features/indice_marginacion_2010.tif", overwrite=T)
        
#Riqueza de especies
        
        y <- read.csv("data/species_richness.csv") %>% 
                mutate(Constant=1) %>% 
                st_as_sf(., coords=c("Center.Lon", "Center.Lat"), crs=4326)
        head(y)
        
        gridsize2 <- 0.5
        r2 <- raster::raster(ext, res=gridsize2)
        
        rast <- raster::rasterize(y, r2, field= "Species.Co")
        crs(rast) <- "+proj=longlat"
        plot(rast)
        
        raster::writeRaster(rast, "data/features/riqueza_spp.tif", overwrite=T)
        

#Marine Ecoregions
        
        y <- read_sf("data/raw_shp/marine_ecoregions.shp") %>% 
                mutate(Constant=1) %>% 
                group_by(PROVINC) %>% 
                mutate(ID_PROVINC=  cur_group_id())
        head(y)
        
        rast <- raster::rasterize(y, r, field= "ID_PROVINC")
        crs(rast) <- "+proj=longlat"
        plot(rast)
        
        raster::writeRaster(rast, "data/features/marine_ecoregions.tif", overwrite=T)
        
        
#Produccion Compartida      
        y <- read_sf("data/raw_shp/Produccion Compartida.shp") %>% 
                mutate(Constant=1)
        head(y)
        
        rast <- raster::rasterize(y, r, field= "Constant")
        crs(rast) <- "+proj=longlat"
        plot(rast)
        
        raster::writeRaster(rast, "data/features/produccion_compartida.tif", overwrite=T)

#Concesiones vigentes      
        y <- read_sf("data/raw_shp/Vigentes.shp") %>% 
                mutate(Constant=1)
        head(y)
        
        rast <- raster::rasterize(y, r, field= "Constant")
        crs(rast) <- "+proj=longlat"
        plot(rast)
        
        raster::writeRaster(rast, "data/features/concesiones_vigentes.tif", overwrite=T)
        
#Indice de vulnerabilidad      
        y <- read_sf("data/raw_shp/vulnerabilidad2010.shp") %>% 
                mutate(Constant=1)
        head(y)
        unique(y$Vulnera)
        y <- y %>% 
                mutate(VUL= case_when(Vulnera=="Muy Bajo"~ 1,
                                      Vulnera=="Bajo"~2,
                                      Vulnera=="Medio"~3,
                                      Vulnera=="Alto"~4,
                                      Vulnera=="Muy Alto"~5,
                                      Vulnera=="S.D"~ 0
                                     )) %>% 
                filter(Vulnera!=0)
        
        rast <- raster::rasterize(y, r, field= "VUL")
        crs(rast) <- "+proj=longlat"
        plot(rast)
        
        raster::writeRaster(rast, "data/features/indice_vulnerabilidad_2010.tif", overwrite=T)
        
        
#Manglares      
        y <- read_sf("data/raw_shp/gmw_mx_2020.shp", crs=4326) %>% 
                mutate(Constant=1)
        head(y)
        
        rast <- raster::rasterize(y, r, field= "Constant")
        crs(rast) <- "+proj=longlat"
        plot(rast)
        head(y)
        raster::writeRaster(rast, "data/features/gmw_mx_2020.tif", overwrite=T)
        
        
        

#Diving Sites count
        y <- read_sf("data/raw_shp/dive_sites.shp", crs=4326) %>% 
                as.data.frame() %>% 
                mutate(Constant=1) %>% 
                group_by(latitude, longitude) %>% 
                unique() %>% 
                st_as_sf(.)
                
        count <- st_intersection(pu, y)
        
        
        
        diving_sites <- count %>% 
                as.data.frame() %>% 
                group_by(id) %>% 
                summarise(x=mean(longitude),
                          y=mean(latitude),
                          count= n()) %>% 
                st_as_sf(., coords=c("x", "y"), crs=4326)    
        
        rast <- raster::rasterize(diving_sites, r, field= "count")
        crs(rast) <- "+proj=longlat"
        plot(rast)
      
        raster::writeRaster(rast, "data/features/Indexes/diving_sites_count.tif", overwrite=T)
        
        