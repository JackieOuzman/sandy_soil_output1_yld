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


################################################################################
## Read in field data 


collated_data <- st_read(paste0(headDir,
                        "/10.Analysis/25/Processing_Jackie/", 
                        site_name,"_collated_data_raw_", 
                        analysis.yr,".shp"))


################################################################################
################ Read in drone data ########################
#date
tif_files <- list.files(path = paste0(headDir, "/",file_path_details$drone), pattern = "\\.tif$|\\.TIF$")
print(tif_files)


file_details1 <- paste0(headDir, "/",file_path_details$drone,tif_files[1]  )
file_details2 <- paste0(headDir, "/",file_path_details$drone,tif_files[2]  )

# Load the raster
drone.dat1 <- terra::rast(file_details1)




drone.dat1
## project the data and resample 1 m grid

#The standard and official Coordinate Reference System (CRS) projection 
#for general use in Walpeup, Victoria, Australia is GDA2020 / MGA zone 54. 
#EPSG Code: 7854

# Method 1: Project to a specific EPSG code
drone.dat1_projected <- project(drone.dat1, "EPSG:7854")  # WGS84 as an example


# Step 2: Create template with 1m resolution
ext_r <- ext(drone.dat1_projected)
template <- rast(ext_r, resolution = 1, crs = crs(drone.dat1_projected))

# Step 3: Resample onto the 1-meter grid
drone.dat1_projected_1m <- resample(drone.dat1_projected, template, method = "bilinear")

plot(drone.dat1_projected_1m)
drone.dat1_projected_1m
## clean up workspace
rm(template, drone.dat1, drone.dat1_projected)

str(collated_data)
drone.dat1_projected_1m


# Transform your shapefile to match the raster CRS
collated_data_projected <- st_transform(collated_data, crs = 7854)

#these projections should match
collated_data_projected
drone.dat1_projected_1m

# Extract values from raster at point locations
extracted_values <- terra::extract(drone.dat1_projected_1m, collated_data_projected)


# Bind all extracted columns to your data (exclude ID column)
collated_data_projected <- bind_cols(collated_data_projected, extracted_values[, -1])
# Rename the clm
collated_data_projected <- collated_data_projected %>% rename(NDVI_drone_2 = "...12")

names(collated_data_projected)


##tidy up
rm(collated_data, extracted_values,ext_r)


################################################################################
write_sf(collated_data_projected,
         paste0(headDir,"/10.Analysis/25/Processing_Jackie/", site_name,"_collated_data_raw_drone", analysis.yr,".shp"))
