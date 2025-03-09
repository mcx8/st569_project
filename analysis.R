# Maxine Cruz 
# Jessica Naranjo

# Date created: 13 February 2025

# Last modified: 4 March 2025




# ----- ABOUT -----

# Contains script for analyzing final project data




# ----- SET WORKING DIRECTORY -----

# Set the working directory to start at st569_project




# ----- LOAD LIBRARIES -----

# For plotting
library(ggplot2)
library(leaflet)

# For saving maps
library(mapview)




# ----- LOAD DATA -----

# Read data set
data <- read.csv("data/gbif/cleaned_species.csv")




# ----- CREATE SPECIES OCCURRENCE MAP -----

# Create map of data
spp_plot <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                             attributionControl = FALSE)) %>%
  addCircleMarkers(data = data,
                   color = "#CD1076",
                   radius = 5,
                   fillOpacity = 0.8,
                   stroke = FALSE) %>%
  addProviderTiles("OpenStreetMap.Mapnik")


# Check
spp_plot

# Save map
mapshot(spp_plot,
        file = "output/mouse_map.png")




# ----- COMPLETE SPATIAL RANDOMNESS -----




