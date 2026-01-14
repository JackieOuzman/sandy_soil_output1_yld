## Stirling Roberton and modified by Jackie
## Purpose: This script is built to undertake base analsysis of trial strip data

## 1) get the field sampling location, date and results

rm(list=ls())
library(terra)
library(sf)
library(ggplot2)
library(multcomp)
library(dplyr)
library(tidyr)
library(broom)
library(multcompView)
library(lubridate)

################################################################################
########################            Define the directory              ##########
################################################################################

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
site_number <- "1.Walpeup_MRS125"
site_name <- "Walpeup_MRS125"
headDir <- paste0(dir, "/work/Output-1/", site_number)




clean.dat <- "No"
analysis.yr <- "25"


metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

crs_used <- 4326
################################################################################
########################    Read in metadata info file names and path ##########
################################################################################

file_path_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "location of file and details") %>% 
  filter(Site == site_number)

seasons <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "seasons") %>% 
  filter(Site == site_number)

harvest_data_file <-  readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "Harvest_data") %>% 
  filter(Site == site_number)


################################################################################
## Read in field data with the Sential, drone and planet data too


collated_data_fld_Sent_drone_planet <- st_read(paste0(headDir,
                        "/10.Analysis/25/Processing_Jackie/", 
                        site_name,"_collated_data_raw_Sen_drone_planet", 
                        analysis.yr,".shp"))

################################################################################
################ Read in planet data ########################

## get the list of all the file options
harvest_files_all_options <- list.files(path = paste0(headDir, "/", harvest_data_file$files))
harvest_files_all_options

## select which file I will use
harvest_files <- list.files(path = paste0(headDir, "/", harvest_data_file$files), pattern = ".shp")
harvest_files

# Load the file 
harvest_raw <- st_read(
  paste0(headDir,"/", harvest_data_file$files,harvest_files))
plot(harvest_raw)

#############
## work out which clm to use 

harvest_raw
names(harvest_raw)

harvest_raw %>%
  select(VRYIELDMAS, WetMass, Moisture, DRYMATTER) %>%
  st_drop_geometry() %>%
  summary()

#Looks like VRYIELDMAS, WetMass are almost the same... Check with Stirling - use VRYIELDMAS

#############
## work out projection

# Check current CRS (Coordinate Reference System)
st_crs(harvest_raw)
# ===== FIX THE CRS METADATA =====
# Your shapefile has WGS84 but with malformed metadata
# Reset it to proper WGS84

harvest_raw <- st_set_crs(harvest_raw, 4326)
# Alternative: GDA2020 / MGA Zone 54 (Australian standard for the area)
harvest_raw_mga <- st_transform(harvest_raw, 7854)
################################################################################
harvest_raw_mga
collated_data_fld_Sent_drone_planet

ggplot() +
  # Plot harvest data with VRYIELDMAS column
  geom_sf(data = harvest_raw_mga, 
          aes(color = VRYIELDMAS), 
          size = 0.5) +
  scale_color_viridis_c(name = "Yield Mass") +
  # Add drone/planet points
  geom_sf(data = collated_data_fld_Sent_drone_planet, 
          color = "red", 
          size = 2,
          shape = 16) +
  theme_minimal() +
  labs(title = "Harvest Yield with sampling Points",
       subtitle = "Walpeup, VIC") +
  theme(legend.position = "right")


### umm lots of zero values?

# Filter out zero values from harvest data
harvest_filtered <- harvest_raw_mga %>%
  filter(VRYIELDMAS > 0)

ggplot() +
  # Plot harvest data with VRYIELDMAS column
  geom_sf(data = harvest_filtered, 
          aes(color = VRYIELDMAS), 
          size = 0.5) +
  scale_color_viridis_c(name = "Yield Mass") +
  # Add drone/planet points
  geom_sf(data = collated_data_fld_Sent_drone_planet, 
          color = "red", 
          size = 2,
          shape = 16) +
  theme_minimal() +
  labs(title = "Harvest Yield with sampling Points",
       subtitle = "Walpeup, VIC") +
  theme(legend.position = "right")
################################################################################

#clean the data

## subset the data so I only have yield data
names(harvest_raw_mga)
harvest_raw_mga_VRYIELDMAS <- harvest_raw_mga %>% select(VRYIELDMAS, geometry)
harvest_raw_mga_VRYIELDMAS <- harvest_raw_mga_VRYIELDMAS %>%
  mutate(ID = row_number(), .before = 1)

names(harvest_raw_mga_VRYIELDMAS)

## clean the data by removing all the zero values
#Remove zero values from VRYIELDMAS column
harvest_raw_mga_VRYIELDMAS <- harvest_raw_mga_VRYIELDMAS %>%
  filter(VRYIELDMAS > 0 )


### Trim 1
# Calculate mean and standard deviation
mean_yield1 <- mean(harvest_raw_mga_VRYIELDMAS$VRYIELDMAS, na.rm = TRUE)
sd_yield1 <- sd(harvest_raw_mga_VRYIELDMAS$VRYIELDMAS, na.rm = TRUE)

harvest_raw_mga_VRYIELDMAS <- harvest_raw_mga_VRYIELDMAS %>% 
  mutate(norm1 = VRYIELDMAS - mean_yield1/sd_yield1 )

harvest_raw_mga_VRYIELDMAS
#filter out all norm1 values below -3 and above +3

harvest_raw_mga_VRYIELDMAS_trim1 <- harvest_raw_mga_VRYIELDMAS %>%
  filter(between(norm1, -3, 3))


ggplot() +
  # Plot harvest data with VRYIELDMAS column
  geom_sf(data = harvest_raw_mga_VRYIELDMAS_trim1 , 
          aes(color = VRYIELDMAS), 
          size = 0.5) +
  scale_color_viridis_c(name = "Yield Mass") +
  # Add drone/planet points
  geom_sf(data = collated_data_fld_Sent_drone_planet, 
          color = "red", 
          size = 2,
          shape = 16) +
  theme_minimal() +
  labs(title = "Harvest Yield Trim 1 with sampling Points",
       subtitle = "Walpeup, VIC") +
  theme(legend.position = "right")


############ Extract values based on location ie the closest point

# Ensure both datasets have the same CRS
harvest_raw_mga_VRYIELDMAS_trim1
collated_data_fld_Sent_drone_planet


#Spatial join - find nearest harvest point to each sampling point
# First, get the indices of nearest features
nearest_indices <- st_nearest_feature(
  collated_data_fld_Sent_drone_planet,
  harvest_raw_mga_VRYIELDMAS_trim1
)
nearest_indices

# Perform the spatial join
extracted_values <- st_join(
  collated_data_fld_Sent_drone_planet,
  harvest_raw_mga_VRYIELDMAS_trim1,
  join = st_nearest_feature
)
distances <- st_distance(
  collated_data_fld_Sent_drone_planet,
  harvest_raw_mga_VRYIELDMAS_trim1[nearest_indices, ],
  by_element = TRUE
)

# Add distance column to extracted values
extracted_values <- extracted_values %>%
  mutate(distance_m = as.numeric(distances))
