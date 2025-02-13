# Maxine Cruz 
# Jessica Naranjo

# Date created: 13 February 2025

# Last modified: 13 February 2025



# --- ABOUT ---

# Contains script for preparing final project data



# ----- ABOUT -----

# 1) Retrieves data from GBIF for the following species:

# Common name (Scientific name) - Taxon key
# Eurasian harvest mouse (Micromys minutus) - 5219833

# 2) Cleans data

# 3) Saves cleaned data as: data/GBIF/cleaned_species.csv




# ----- LOAD LIBRARIES -----

library(rgbif)
library(CoordinateCleaner)
library(dplyr)
library(sf)
library(sp)
library(mapview)



# ----- GET DATA -----

# Request full occurrence record of orgnanisms from GBIF

# NOTE: occ_download requires a setup with GBIF account on a .Renviron file
# See https://docs.ropensci.org/rgbif/articles/gbif_credentials.html

occ_download(pred_in("speciesKey", 5219833))

# Prints download and citation info as follows:

# Download Info:
# Username: tmcruz
# E-mail: tmcruz@arizona.edu
# Format: DWCA
# Download key: 0000650-250213122211068
# Created: 2025-02-13T18:18:38.789+00:00

# Citation Info:  
# Please always cite the download DOI when using this data.
# https://www.gbif.org/citation-guidelines
# DOI: 10.15468/dl.87sesg

# Citation:
# GBIF Occurrence Download 
# https://doi.org/10.15468/dl.87sesg
# Accessed from R via rgbif 

# Retrieve download
raw_data <- 
  occ_download_get(key = "0000650-250213122211068", path = "data/gbif/") %>%
  occ_download_import()

# 35218 observations, 223 variables

# Save raw data as csv
write.csv(raw_data, "data/gbif/raw_species.csv", row.names = FALSE)




# ----- CLEAN DATA: FILTERING -----

# If starting here, read in the raw data (downloaded on 13 February 2025)
raw_data <- read.csv("data/gbif/raw_species.csv")

# 35218 observations, 223 variables

# Clean data
data <- raw_data %>%
  # Remove NA latitude / longitude values (removes 3123, 32095 left)
  filter(!is.na(decimalLatitude), 
         !is.na(decimalLongitude)) %>%
  # Remove fossil records / specimens (removes 2, 32093 left)
  filter(!basisOfRecord %in% "LIVING_SPECIMEN") %>% 
  # Organisms are land-dwellers, so remove records in the ocean (removes 1339, 30754 left)
  cc_sea(lon = "decimalLongitude", 
         lat = "decimalLatitude") %>%
  # Remove those with issues (removes 14, 30740 left)
  filter(hasGeospatialIssues == FALSE) %>% 
  # Remove duplicates (21583, removes 9157)
  distinct(decimalLongitude, 
           decimalLatitude, 
           year,
           month,
           day,
           speciesKey, 
           datasetKey, 
           .keep_all = TRUE)

# 21583 observations, 223 variables

# Reduce columns to what is / might be necessary for analyses / figures
data <- data %>%
  select(67, 68, 69, 98, 99, 202, 201, 157, 86, 85) %>%
  rename(latitude = decimalLatitude,
         longitude = decimalLongitude) %>%
  arrange(species, kingdom, year, month, day)

# 13248 observations, 10 variables

# Columns identified using which(colnames(data) == "column_name")
# 67 = year
# 68 = month
# 69 = day
# 85 = countryCode
# 86 = stateProvince
# 98 = decimalLatitude
# 99 = decimalLongitude
# 157 = kingdom
# 201 = speciesKey
# 202 = species

# NOTE: It seems the numbers change? Make sure to check column names.

# Save as csv
write.csv(data, "data/gbif/cleaned_species.csv", row.names = FALSE)





