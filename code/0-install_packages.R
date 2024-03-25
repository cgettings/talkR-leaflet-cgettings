
# These are the packages you already have

packages_you_have <- installed.packages()[, "Package"]

# These are all the packages we'll use

packages_we_use <- c("rmarkdown", "jsonlite", "dplyr", "tibble", "tidyr", "readr", "stringr", "leaflet", "leaflet.extras", "leafem", "terra", "stars", "geojsonio", "sf", "viridisLite", "here", "htmlwidgets", "htmltools", "conflicted")

# These are the packages you need to install

packages_you_need <- packages_we_use[!packages_we_use %in% packages_you_have]

# This will install the packages you don't already have

if (length(packages_you_need)) {
    
    cat("Installing:", paste(packages_you_need, collapse = ", "))
    
    install.packages(packages_you_need)
}
