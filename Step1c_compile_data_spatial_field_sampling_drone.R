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

Walpeup_125_ndvi_2025
file_details1 <- paste0(headDir, "/",file_path_details$drone,tif_files[1]  )
file_details2 <- paste0(headDir, "/",file_path_details$drone,tif_files[2]  )

# Load the raster
drone.dat1 <- terra::rast(file_details1)

#### This is claude.ai code but I need to fix it up.
#the issue is that the raster needs to be projcetd first before it can be resampled, 
#but I think the below code is too complex and I don't think the projection is correct











# Step 1: Project to a metric CRS (UTM) instead of WGS84
# Auto-detect appropriate UTM zone
lon_center <- mean(c(xmin(drone.dat1), xmax(drone.dat1)))
lat_center <- mean(c(ymin(drone.dat1), ymax(drone.dat1)))

utm_zone <- floor((lon_center + 180) / 6) + 1
hemisphere <- ifelse(lat_center >= 0, "north", "south")

if (hemisphere == "north") {
  epsg_code <- paste0("epsg:", 32600 + utm_zone)
} else {
  epsg_code <- paste0("epsg:", 32700 + utm_zone)
}

cat("Projecting to:", epsg_code, "\n")

# Project to metric CRS
drone.dat1 <- terra::project(drone.dat1, epsg_code, method = "bilinear")

# Step 2: Create template with 1m resolution
ext_r <- ext(drone.dat1)
template <- rast(ext_r, resolution = 1, crs = crs(drone.dat1))

# Step 3: Resample onto the 1-meter grid
drone.dat1_1m <- resample(drone.dat1, template, method = "bilinear")

# Verify the result
cat("Original resolution:", res(drone.dat1), "meters\n")
cat("Resampled resolution:", res(drone.dat1_1m), "meters\n")
cat("Dimensions:", dim(drone.dat1_1m), "\n")


plot(drone.dat1_1m)
drone.dat1_1m
