# Maxine Cruz 
# Jessica Naranjo

# Date created: 13 February 2025

# Last modified: 13 February 2025



# --- ABOUT ---

# Contains script for analyzing final project data



# --- 

# Create map of data


# --- loading in packages
library(sf)
library(ggplot2)
library(spatstat)

# reading in data
mouse <- read.csv("~/st569_project/data/gbif/cleaned_species.csv")

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
#install.packages("reshape2")
library(reshape2)
mouse_den_df <- melt(mouse_den)

# Rename columns for clarity
colnames(mouse_den_df) <- c("x", "y", "z")

# Plot using ggplot2
ggplot(mouse_den_df, aes(x = x, y = y, z = z)) +
  geom_tile(aes(fill = z)) +
  stat_contour(aes(z = z), color = "black") +  # Add contour lines
  scale_fill_viridis_c(option = "plasma") +
  theme_bw() + 
  labs(title = "Intensity")


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
