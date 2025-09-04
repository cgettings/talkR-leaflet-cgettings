###########################################################################################-
###########################################################################################-
##
## 3. Raster - Advanced ----
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# running package install script
#-----------------------------------------------------------------------------------------#

# find script

install_packages_loc <-
    list.files(
        getwd(),
        pattern = "0-install-packages.R",
        full.names = TRUE,
        recursive = TRUE
    )

# run script

source(install_packages_loc)


#-----------------------------------------------------------------------------------------#
# Loading libraries
#-----------------------------------------------------------------------------------------#

library(jsonlite)
library(dplyr)
library(tibble)
library(tidyr)
library(here)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(terra)
library(stars)
library(sf)
library(viridisLite)
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

# set base_url

base_url <- "https://raw.githubusercontent.com/nychealth/EHDP-data/production/"

# get nyc borders 

nyc_borders <- 
    read_sf(paste0(base_url, "geography/borough.topo.json"), crs = 4326) %>% 
    as_tibble()

# now get just the queens border

queens_border <- 
    nyc_borders %>% 
    filter(name == "Queens") %>% 
    st_as_sf()

# turn into a SpatVector to use for cropping

queens_border_vec <- vect(queens_border$geometry)

# convert to json (which we'll pass to `onRender` as a demonstration)

nyc_borders_json <- toJSON(nyc_borders)


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

# access token for JawgMaps (free, and can be revoked!)

jawg_token <- "jBn4H6Bv04xoEkuaRdWm4vIcIJjGYmLsD1jZ2kRL5uSZk61d1YhwzvdVM4FBaadM"


nyccas_map <- 
    
    # initialize
    
    leaflet() %>%
    
    # enable browser tile caching, to speed up the map
    
    enableTileCaching() %>%
    
    # add tiles in groups, which we'll use for the tile layer switcher
    
    # zIndex = -1000 makes sure that the tiles are always the lowest layer
    
    addProviderTiles(
        # provider = providers$Stadia.AlidadeSmoothDark, 
        # provider = providers$CartoDB.DarkMatter, 
        provider = providers$Jawg.Matrix, 
        options = providerTileOptions(zIndex = -1000, accessToken = jawg_token),
        group = "Dark"
    ) %>%
    addProviderTiles(
        # provider = providers$Stadia.StamenTonerLite, 
        # provider = providers$CartoDB.Positron, 
        provider = providers$Jawg.Light, 
        options = providerTileOptions(zIndex = -1000, accessToken = jawg_token),
        group = "Light"
    ) %>%
    addProviderTiles(
        provider = providers$CartoDB.Voyager, 
        options = providerTileOptions(zIndex = -1000),
        group = "Streets"
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
    
    addResetMapButtonPosition(position = "bottomleft") %>% 
    
    # # add data and custom javascript
    
    onRender(
        jsCode = 
            "function(element, widget, data) { 
        
                console.log('> element:', element);
                console.log('> widget:', widget);
                console.log('> data:', data);

                let queens = data.filter(function(d) {return d.name == 'Queens'})
                // let queens = data.filter(d => d.name == 'Queens')
        
                console.log('> queens:', queens);
            }",
        data = nyc_borders_json
        # data = nyc_borders
        # data = list("nyc_borders" = nyc_borders)
    )


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
    file = here("docs/3-raster_map_self-contained.html"),
    selfcontained = TRUE,
    title = "3. Raster - Advanced"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Non-self-contained
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

saveWidget(
    widget = nyccas_map,
    file = here("docs/3-raster_map_non-self-contained.html"),
    selfcontained = FALSE,
    title = "3. Raster - Advanced"
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
