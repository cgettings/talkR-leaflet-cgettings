###########################################################################################-
###########################################################################################-
##
## 4. mapping light pollution - Very complicated: ----
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
library(leaflet)
library(leaflet.extras)
library(leafem)
library(stars)
library(viridisLite)
library(htmlwidgets)
library(htmltools)
library(here)
library(glue)
library(tigris)
library(fs)

#-----------------------------------------------------------------------------------------#
# Reading raster ----
#-----------------------------------------------------------------------------------------#

sky_brightness <- 
    rast(here("data/ny_sky_brightness_geotiff.tif")) %>% 
    st_as_stars() %>% 
    rename(brightness_values = ny_sky_brightness_geotiff.tif)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# getting bbox for setting map bounds
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

sky_brightness_bbox <- st_bbox(sky_brightness)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# converting to tbl, to pass to onRender
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

sky_brightness_coords <- 
    sky_brightness %>% 
    as_tibble() %>% 
    drop_na(brightness_values)

# collecting garbage, because `stars` object is huge

invisible(gc())

#-----------------------------------------------------------------------------------------#
# Adding extras ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# plugins & dependencies
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

registerPlugin <- 
    function(map, plugin) {
        map$dependencies <- c(map$dependencies, list(plugin))
        map
    }

# my own FA library

fa_dir <- here("code/plugins/@fortawesome/fontawesome-free")

fa_plugin <-
    htmlDependency(
        name = "fontawesome", 
        version = fromJSON(path(fa_dir, "package.json"))$version,
        src = c(file = fa_dir),
        stylesheet = "css/all.css",
        all_files = TRUE
    )


# geoblaze raster computation

geoblaze_dir <- here("code/plugins/geoblaze")

geoblaze_plugin <-
    htmlDependency(
        name = "geoblaze", 
        version = fromJSON(path(geoblaze_dir, "package.json"))$version,
        src = c(file = path(geoblaze_dir, "dist")),
        script = "geoblaze.web.min.js",
        all_files = FALSE
    )


# extramarkers

ExtraMarkers_dir <- here("code/plugins/Leaflet.ExtraMarkers")

ExtraMarkers_plugin <-
    htmlDependency(
        name = "ExtraMarkers", 
        version = fromJSON(path(ExtraMarkers_dir, "package.json"))$version,
        src = c(file = path(ExtraMarkers_dir, "dist")),
        stylesheet = "css/leaflet.extra-markers.min.css",
        script = "js/leaflet.extra-markers.min.js",
        all_files = TRUE
    )


# geocoder

Geocoder_dir <- here("code/plugins/Control.Geocoder")

Geocoder_plugin <-
    htmlDependency(
        name = "geocoder",
        version = fromJSON(path(Geocoder_dir, "package.json"))$version,
        src = list(file = path(Geocoder_dir, "dist")),
        stylesheet = "Control.Geocoder.css",
        script = "Control.Geocoder.js",
        all_files = TRUE
    )


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# modifications of {leaflet} functions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# `addResetMapButton` from {leaflet.extras}, but allowing specification of position

source("code/functions/addResetMapButtonPosition.R")

# `addEasyButton` from {leaflet}, but removing fontawesome dependency (so I can use the current 
#   version from node repo)

source("code/functions/addEasyButtonNoFaDeps.R")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Custom JavaScript and HTML for `onRender` ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

closest_dark_place_js <- read_file(here("code/js/closest_dark_place.js"))
dark_point_control    <- read_file(here("code/html/dark_point_control.html"))


# Replacing "####" in JavaScript with HTML

closest_dark_place <- 
    str_replace(
        closest_dark_place_js, 
        "####",
        dark_point_control
    )


#=========================================================================================#
# Mapping
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Constructing map
#-----------------------------------------------------------------------------------------#

light_pollution_map <- 
    
    # initialize
    
    leaflet(
        options = list(
            "duration" = 0.375,
            "zoomSnap" = 0.5,
            "padding" = c(10, 10),
            "preferCanvas" = FALSE, 
            "updateWhenZooming" = FALSE,
            "updateWhenIdle" = TRUE
        )
    ) %>%
    
    fitBounds(
        lng1 = sky_brightness_bbox[[1]],
        lat1 = sky_brightness_bbox[[2]],
        lng2 = sky_brightness_bbox[[3]],
        lat2 = sky_brightness_bbox[[4]]
    ) %>%
    
    # enable browser tile caching, to speed up the map
    
    enableTileCaching() %>%
    
    # add tiles in groups, which we'll use for the tile layer switcher
    
    # zIndex = -1000 makes sure that the tiles are always the lowest layer
    
    addProviderTiles(
        provider = providers$Stadia.AlidadeSmoothDark, 
        options = providerTileOptions(zIndex = -1000),
        group = "Dark"
    ) %>%
    addProviderTiles(
        provider = providers$Stadia.StamenTonerLite, 
        options = providerTileOptions(zIndex = -1000),
        group = "Light"
    ) %>%
    addProviderTiles(
        provider = providers$CartoDB.Voyager, 
        options = providerTileOptions(zIndex = -1000),
        group = "Streets"
    ) %>%
    
    # you xcan also add tiles from other places
    
    addTiles(
        urlTemplate = 
            paste0(
                "//services.arcgisonline.com/ArcGIS/rest/services/",
                "USA_Topo_Maps/MapServer/tile/{z}/{y}/{x}"
            ), 
        attribution = 
            glue(
                "Map tiles by <a href='http://goto.arcgisonline.com/maps/USA_Topo_Maps'>Esri</a> - ",
                "Map Data © 2013 National Geographic Society, i-cubed"
            ), 
        options = tileOptions(zIndex = -1000),
        group = "Topo"
    ) %>% 
    
    # sky brightness raster
    
    addGeoRaster(
        x = sky_brightness,
        project = TRUE,
        group = "Sky Brightness",
        layerId = "Sky Brightness",
        resolution = 64,
        colorOptions =
            colorOptions(
                palette = inferno(64, direction = -1),

                # modifying breaks to get a better mapping of visual differences to
                #   photometric categories
                #
                # 16 = 2^4, so need 4 `sqrt()` calls to reverse:

                breaks =
                    sqrt(sqrt(sqrt(sqrt(
                        seq(
                            min(sky_brightness$brightness_values, na.rm = TRUE)^16,
                            max(sky_brightness$brightness_values, na.rm = TRUE)^16,
                            length.out = 64
                        )
                    )))),
                na.color = "#00000000"
            ),
        options =
            tileOptions(
                zIndex = 1000,
                updateWhenZooming = FALSE,
                updateWhenIdle = TRUE
            )
    ) %>%
    
    # adding controls
    
    addLayersControl(
        baseGroups = c("Dark", "Light", "Streets", "Topo"),
        overlayGroups = "Sky Brightness",
        options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE),
        position = "topright"
    ) %>%
    
    # sky brightness raster mouseover values
    
    addImageQuery(
        x = sky_brightness,
        group = "Sky Brightness",
        layerId = "Sky Brightness",
        position = "topright",
        digits = 2,
        type = "mousemove",
        prefix = "",
        project = TRUE
    ) %>% 
    
    # reset buttons
    
    addResetMapButtonPosition(position = "bottomleft") %>%
    
    # registering dependencies
    
    addAwesomeMarkersDependencies(libs = c("ion", "glyphicon")) %>%
    
    registerPlugin(fa_plugin) %>%
    registerPlugin(geoblaze_plugin) %>%
    registerPlugin(ExtraMarkers_plugin) %>%
    registerPlugin(Geocoder_plugin) %>%
    
    # adding specialty JavaScript to find closest dark place to click
    
    onRender(
        str_c(
            "function(el, x, data) {\n",
            closest_dark_place,
            "}"
        ),
        data = sky_brightness_coords
    )
    

# light_pollution_map

#=========================================================================================#
# Save the map ----
#=========================================================================================#

# We'll save 2 different versions: self-contained, and non-self-contained

#-----------------------------------------------------------------------------------------#
# Self-contained
#-----------------------------------------------------------------------------------------#

# In this version, all the data and dependencies are included in-line in the single HTML output file.

# The self-contained version can get pretty big, but you don't have to worry about anything except that 1 output file.

saveWidget(
    widget = light_pollution_map,
    file = here("output/light_pollution_map_self-contained.html"),
    selfcontained = FALSE,
    title = "Light Pollution Map for New York"
)


#-----------------------------------------------------------------------------------------#
# Non-self-contained
#-----------------------------------------------------------------------------------------#

# Here, the data and dependencies are put into folders, which the HTML will point to.

saveWidget(
    widget = light_pollution_map,
    file = here("output/light_pollution_map_non-self-contained.html"),
    selfcontained = FALSE,
    title = "Light Pollution Map for New York"
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
