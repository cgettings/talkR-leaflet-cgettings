###########################################################################################-
###########################################################################################-
##
## 1. Basic ----
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

library(dplyr)
library(tidyr)
library(leaflet)
library(conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

#=========================================================================================#
# Create some simple maps ----
#=========================================================================================#

# This is all you need to create a leaflet map:

useless_map <- leaflet() %>% addTiles()
useless_map

# It's not very useful, but it's nice that it works out of the box. Let's set the initial view so we can see something:

sheep_meadow <- useless_map %>% setView(lng = -73.975, lat = 40.772, zoom = 15)
sheep_meadow

# It still only has a tile layer. Let's add a marker with a popup.

sheep_meadow_marker <- 
    sheep_meadow %>% 
    addMarkers(
        lng = -73.975, 
        lat = 40.772, 
        label = "This is the Sheep Meadow",
        popup = "It's a nice place to hang out"
    )

sheep_meadow_marker


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
    widget = sheep_meadow_marker,
    file = here("docs/1-basic_map_self-contained.html"),
    selfcontained = TRUE,
    title = "1. Basic"
)

#-----------------------------------------------------------------------------------------#
# Non-self-contained
#-----------------------------------------------------------------------------------------#

# Here, the data and dependencies are put into folders, which the HTML will point to.

saveWidget(
    widget = sheep_meadow_marker,
    file = here("docs/1-basic_map_non-self-contained.html"),
    selfcontained = FALSE,
    title = "1. Basic"
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
