library(sf)
library(ggplot2)
library(tidyverse)


# Load files --------------------------------------------------------------

# Marine Protected Areas
mpa <- st_read("shp/final_mpa_layer.shp", crs=4326) %>% 
        st_make_valid()


#Planning units
pu <- st_read("shp/fine_pu_BM.shp", crs=4326)

#Planning Units Interstecting MPAs (locked in planning units)
pu2 <- pu[mpa,]

# Planning Units intersecting Resource Extraction Concessions








# Merge Planning Units with locked in Planning Units ----------------------


pu2 <- pu2 %>% 
        as.data.frame() %>% 
        mutate(locked_in=1 ) %>% 
        select(-geometry)
pu <- pu %>% 
        as.data.frame()

pu_lockedin <- merge(pu, pu2, by=c("id", "left", "top", "right", "bottom"), all.x = T) %>% 
        mutate(locked_in= ifelse(is.na(locked_in), 0, locked_in )) %>% 
        st_as_sf(.,crs=4326 )


plot(st_as_sf(pu_lockedin[, "locked_in"]), main = "Protected area coverage")

#Export shp

st_write(pu_lockedin, "shp/pu_lockedin.shp")



# END ---------------------------------------------------------------------


