# Maxine Cruz 
# Jessica Naranjo

# Date created: 13 February 2025

# Last modified: 4 March 2025




# ----- ABOUT -----

# Contains script for analyzing final project data




# ----- SET WORKING DIRECTORY -----

# Set the working directory to start at st569_project




# ----- LOAD LIBRARIES -----

library(sf)
library(terra)
library(spatstat)

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




# ----- ANALYSES -----

# reading in data
mouse <- read.csv("~/st569_project/data/gbif/cleaned_species.csv")

# (for Maxine's directory)
mouse <- read.csv("data/gbif/cleaned_species.csv")

# making it into sf object
# assuming (epsg:4326, https://epsg.io/4326)
mouse_sf <- st_as_sf(mouse, coords = c("longitude", "latitude"), crs = 4326)


# --- making it into point process object

# bounding box of spatial data
bbox <- st_bbox(mouse_sf) 

# converting sf object to ppp
mouse_pp <- ppp(x = st_coordinates(mouse_sf)[, 1],  # x-coordinates (longitude)
                y = st_coordinates(mouse_sf)[, 2],  # y-coordinates (latitude)
                window = owin(c(bbox["xmin"], bbox["xmax"]), 
                              c(bbox["ymin"], bbox["ymax"])))  # bounding box as window

# checking if it is a point process object
class(mouse_pp) 

# printing the ppp object
print(mouse_pp)

# --- plot the point pattern object
# Note to Maxine: it's basically the same plot you produced, but less pretty lol
# st_as_sf(rescale(mouse_pp)) |>
#   ggplot() +
#   geom_sf(size = 3) +
#   ggtitle("mouse")


# --- looking at intensity of the events (mean number of events per area)
# intensity function of point pattern
mouse_den <- density(mouse_pp)

# -- plotting intensity and adding in the density curves

# - using the textbook way (didn't like the it aesthetically)
plot(mouse_den, main = "Intensity")
contour(mouse_den, add = TRUE)

# - ggplot2 way (better, but idk if this is the one we want)
# install.packages("reshape2")
library(reshape2)
mouse_den_df <- melt(mouse_den)

# Rename columns for clarity
colnames(mouse_den_df) <- c("x", "y", "z")

library(maps)
world <- map_data("world")
bbox_x <- range(mouse_den_df$x)
bbox_y <- range(mouse_den_df$y) 
world_cropped <- world[world$long >= bbox_x[1] & world$long <= bbox_x[2] & 
                         world$lat >= bbox_y[1] & world$lat <= bbox_y[2], ]

# Plot using ggplot2
ggplot(mouse_den_df, aes(x = x, y = y, z = z)) +
  geom_tile(aes(fill = z)) +
  # Add border lines
  borders("world", 
          colour = "black", 
          xlim = c(bbox[1], bbox[3]),
          ylim = c(bbox[2], bbox[4]),
          fill = NA) +
  stat_contour(aes(z = z), color = "white") +  # Add contour lines
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() + 
  labs(title = "Intensity of the Eurasian Harvest Mouse Observations",
       x = "Longitude",
       y = "Latitude",
       fill = "Intensity\n(Mean number of\nevents per unit area)") +
  coord_fixed(xlim = c(bbox[1], bbox[3]), 
              ylim = c(bbox[2], bbox[4]), 
              expand = F) +
  theme(aspect.ratio = 3/4,
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 8)),
        plot.title = element_text(face = "bold"))


# --- Complete Spatial Randomness (CSR)

# testing whether the point pattern is completely spatially random
# H0: pattern is CSR
# HA: pattern is not CSR
quadrat.test(mouse_pp, method = "MonteCarlo") # evidence against CSR

# 

# --- homogeneous Poisson point process

hpp <- rpoispp(lambda = 100)
ggplot(st_as_sf(hpp)) + 
  geom_sf()

# -- K-function
E <- envelope(mouse_pp)
plot(E)
