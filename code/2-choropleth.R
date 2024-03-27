###########################################################################################-
###########################################################################################-
##
## 2. Choropleth - Intermediate ----
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries
#-----------------------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(leaflet)
library(jsonlite)
library(sf)
library(viridisLite)
library(here)
library(htmlwidgets)
library(conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


#=========================================================================================#
# Now, something more interesting ----
#=========================================================================================#

# Let's make a choropleth using data from the Environment and Health Data Portal(https://a816-dohbesp.nyc.gov/IndicatorPublic/). We'll download data, metadata, and geo data from the `EHDP-data` GitHub repository.

#-----------------------------------------------------------------------------------------#
# Load data 
#-----------------------------------------------------------------------------------------#

# set base url

base_url <- "https://raw.githubusercontent.com/nychealth/EHDP-data/production/"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# First get the actual data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Indicator 2416 is "Litter basket coverage" (https://a816-dohbesp.nyc.gov/IndicatorPublic/data-explorer/active-design/?id=2416).

litter_baskets_data <- 
    fromJSON(paste0(base_url, "indicators/data/2416.json")) %>% 
    as_tibble() %>% 
    filter(GeoType == "NTA2020", MeasureID == 1317)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Then get the metadata (names, time periods, etc.)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# ==== indicator metadata ==== #

litter_baskets_metadata <- 
    fromJSON(paste0(base_url, "indicators/metadata/metadata.json")) %>% 
    as_tibble() %>% 
    filter(IndicatorID == 2416) %>% 
    unnest(cols = Measures) %>% 
    select(IndicatorID, IndicatorName, MeasureID, MeasurementType, DisplayType) %>% 
    filter(MeasureID == 1317)


# ==== time periods metadata ==== #

time_periods <- 
    fromJSON(paste0(base_url, "indicators/metadata/TimePeriods.json")) %>% 
    as_tibble() %>% 
    select(TimePeriodID, TimePeriod)


# ==== get geo data ==== #

# The `topojson_read` function from `geojsonio` returns an `sf`-classed object with a `geometry` column. This is what leaflet is going to use to create the choropleth.

nta_2020 <- read_sf(paste0(base_url, "geography/NTA_2020.topo.json"), crs = 4326)


#-----------------------------------------------------------------------------------------#
# Join the data and metadata
#-----------------------------------------------------------------------------------------#

litter_baskets <- 
    
    # join the indicator metadata and data
    
    left_join(
        litter_baskets_metadata,
        litter_baskets_data,
        by = "MeasureID"
    ) %>% 
    
    # join the geo data
    
    left_join(
        .,
        nta_2020,
        by = c("GeoID" = "GEOCODE")
    ) %>% 
    
    # join the time periods metadata
    
    left_join(
        .,
        time_periods,
        by = "TimePeriodID"
    ) %>% 
    
    # convert the tibble to an `sf` class object, so that leaflet knows what to do with it. the whole tibble will inherit the geo properties from the geometry column
    
    st_as_sf() %>% 
    
    # construct the text for the labels and popups
    
    mutate(
        
        # html works for popups
        popup = 
            paste0(
                "<span style='font-size:1rem'>",
                "<b>", GEONAME, "</b>", 
                "<br/><br/>", 
                MeasurementType, ", ", TimePeriod, ":", 
                "<br/>", 
                "&nbsp;", DisplayValue, " ", DisplayType,
                "<span>"
            ),
        
        # html doesn't work for labels, though it's supposed to
        label = paste0(GEONAME, ": ", DisplayValue),
    )

litter_baskets


#=========================================================================================#
# Constructing the map
#=========================================================================================#

# but first generate a color palette function using viridis

pal_fun <- colorNumeric("viridis", domain = NULL)

# ok now map it!

litter_baskets_map <- 
    
    # initialize the map with the joined data and a nice view
    
    leaflet(litter_baskets) %>% 
    setView(lng = -73.88, lat = 40.75, zoom = 11) %>% 
    
    # add some nice low-key tiles
    
    # addProviderTiles(providers$Stadia.StamenTonerLite) %>%
    addProviderTiles(providers$CartoDB.Voyager) %>%
    
    # add the choropleth polygons, plus the palette function and the labels
    
    addPolygons(
        fillColor = ~pal_fun(Value),
        popup = ~popup,
        label = ~label,
        labelOptions = labelOptions(textsize = "15px"),
        highlightOptions = 
            highlightOptions(
                stroke = TRUE,
                color = "white",
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = TRUE
            ),
        weight = 0.5,
        opacity = 1,
        color = "#cccccc",
        fillOpacity = 0.7
    ) %>%
    
    # finish with a legend
    
    addLegend(
        "bottomright", 
        pal = pal_fun, 
        values = ~Value,
        title = "baskets/mi<sup>2</sup>",
        opacity = 1
    )

litter_baskets_map


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
    widget = litter_baskets_map,
    file = here("docs/2-choropleth_self-contained.html"),
    selfcontained = TRUE,
    title = "2. Choropleth - Intermediate"
)

#-----------------------------------------------------------------------------------------#
# Non-self-contained
#-----------------------------------------------------------------------------------------#

# Here, the data and dependencies are put into folders, which the HTML will point to.

saveWidget(
    widget = litter_baskets_map,
    file = here("docs/2-choropleth_non-self-contained.html"),
    selfcontained = FALSE,
    title = "2. Choropleth - Intermediate"
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
