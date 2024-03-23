###########################################################################################-
###########################################################################################-
##
## 3. Raster - advanced ----
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
library(dplyr)
library(tibble)
library(tidyr)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(terra)
library(stars)
library(geojsonio)
library(sf)
library(viridisLite)
library(here)
library(htmlwidgets)
library(htmltools)
library(conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

#-----------------------------------------------------------------------------------------#
# modifications of {leaflet} functions
#-----------------------------------------------------------------------------------------#

# `leaflet.extras::addResetMapButton` but allowing specification of position

source(here("code/functions/addResetMapButtonPosition.R"))


#-----------------------------------------------------------------------------------------#
# Loading data -----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# border for boro
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

base_url <- "https://raw.githubusercontent.com/nychealth/EHDP-data/production/"

queens_border <- 
    topojson_read(paste0(base_url, "geography/borough.topo.json"), crs = 4326) %>% 
    as_tibble() %>% 
    st_as_sf() %>% 
    filter(name == "Queens") %>% 
    pull(geometry) %>% 
    vect()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# NYCCAS rasters
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# predicted annual average fine particulate matter <2.5 microns, Dec 2020-Dec 2021
# units: ug/m3

# downloaded from https://data.cityofnewyork.us/Environment/NYCCAS-Air-Pollution-Rasters/q68s-8qxv/about_data

nyccas_pm25_qns_stars <- 
    rast(here("data/NYCCAS/AnnAvg1_13_300mRaster/aa13_pm300m")) %>%
    project("epsg:4326") %>% 
    crop(queens_border, mask = TRUE) %>% 
        
    # transforming to stars object, to make adding to leaflet easier
    
    st_as_stars() %>%
    
    # re-setting CRS to original data
    
    st_set_crs(value = st_crs(4326))


#=========================================================================================#
# Mapping ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Constructing map
#-----------------------------------------------------------------------------------------#

nyccas_map <- 
    
    # initialize
    
    leaflet() %>%
    
    # enable browser tile caching, to speed up the map
    
    enableTileCaching() %>%
    
    # add tiles in groups, which we'll use for the tile layer switcher
    
    addProviderTiles(
        providers$Stadia.AlidadeSmoothDark, 
        group = "Dark", 
        options = providerTileOptions(zIndex = -1000)
    ) %>%
    addProviderTiles(
        providers$Stadia.StamenTonerLite, 
        group = "Light", 
        options = providerTileOptions(zIndex = -1000)
    ) %>%
    addProviderTiles(
        providers$CartoDB.Voyager, 
        group = "Streets", 
        options = providerTileOptions(zIndex = -1000)
    ) %>%
    
    # NYCCAS_pm25 raster
    
    # all the `group` and `layerId` arguments have to be the same to get the raster, image query, and layers control to work (as far as I can tell)
    
    addGeoRaster(
        x = nyccas_pm25_qns_stars,
        group = "NYCCAS PM 2.5",
        layerId = "NYCCAS PM 2.5",
        resolution = 64,
        project = TRUE,
        colorOptions =
            colorOptions(
                palette = inferno(64, direction = 1),
                na.color = "#00000000"
            ),
        
        # options that reduce overhead while rendering the raster into tiles
        options =
            tileOptions(
                zIndex = 1000,
                updateWhenZooming = FALSE,
                updateWhenIdle = TRUE
            )
    ) %>%
    
    # adding controls
    
    addLayersControl(
        baseGroups = c("Dark", "Light", "Streets"),
        overlayGroups = "NYCCAS PM 2.5",
        options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE),
        position = "topright"
    ) %>%
    
    # raster mouseover values (put this after layers control for better positioning on map)
    
    addImageQuery(
        x = nyccas_pm25_qns_stars,
        group = "NYCCAS PM 2.5",
        layerId = "NYCCAS PM 2.5",
        position = "topright",
        digits = 2,
        type = "mousemove",
        prefix = "",
        project = TRUE
    ) %>%
    
    # reset buttons
    
    addResetMapButtonPosition(position = "bottomleft")


# now print the map

nyccas_map 

#-----------------------------------------------------------------------------------------#
# Saving map ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Self-contained
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

saveWidget(
    widget = nyccas_map,
    file = here("output/nyccas_map_self-contained.html"),
    selfcontained = TRUE,
    title = "NYCCAS PM2.5 2022 Queens"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Non-self-contained
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

saveWidget(
    widget = nyccas_map,
    file = here("output/nyccas_map_non-self-contained.html"),
    selfcontained = FALSE,
    title = "NYCCAS PM2.5 2022 Queens"
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
