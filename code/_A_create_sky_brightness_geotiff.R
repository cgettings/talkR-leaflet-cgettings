###########################################################################################-
###########################################################################################-
##
## Create sky_brightness_geotiff.tif ----
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries
#-----------------------------------------------------------------------------------------#

library(jsonlite)
library(terra)
library(tidyverse)
library(sf)
library(stars)
library(here)
library(glue)
library(tigris)
library(fs)

#-----------------------------------------------------------------------------------------#
# Loading data ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# border for states
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

state_border <- 
    states(cb = TRUE, resolution = "500k", year = 2021) %>% 
    filter(STUSPS %in% c("NY")) %>%
    st_transform(st_crs(2263)) %>% 
    st_union() %>% 
    st_transform(st_crs(4326)) %>% 
    vect()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# luminance & finding which points are within specified borders
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# downloaded from http://doi.org/10.5880/GFZ.1.4.2016.001
# unzipped size is 2.8 GB

luminance <- 
    rast(here("data/World_Atlas_2015.tif")) %>% 
    crop(state_border, mask = TRUE)

#-----------------------------------------------------------------------------------------#
# Subsetting and computing ----
#-----------------------------------------------------------------------------------------#

sky_brightness <- 
    
    luminance %>% 
    
    # turn into stars object, so that as_tibble returns all 3 columns
    
    st_as_stars() %>% 
    
    # turning into a tbl to compute sky brightness
    
    as_tibble() %>% 
    
    # renaming to something that makes sense
    
    rename(luminance = World_Atlas_2015) %>% 
    
    # drop NAs representing cropped/masked values
    
    drop_na(luminance) %>% 
    
    # Computing mag/arcsec^2
    
    mutate(
        
        # adding 0.171168465 to each value. ensuring that a luminance of 0 produces 
        #   sky_brightness of 22.0 (which is considered the darkest a dark sky gets), 
        #   and also keeps `log10()` happy
        # 
        # This replicates the procedure done by lightpollution.info when displaying
        #   luminance values
        
        sky_brightness = (log10(((luminance+0.171168465)/1000)/10.8e4))/-0.4
        
    ) %>% 
    
    select(-luminance) %>%
    
    # transforming to stars object, to make adding to leaflet easier
    
    st_as_stars() %>%
    
    # re-setting CRS to original data
    
    st_set_crs(value = st_crs(4326))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# saving cropped raster
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# sky_brightness_geotiff <- rast(sky_brightness)

write_stars(
    sky_brightness,
    "data/ny_sky_brightness_geotiff.tif"
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
