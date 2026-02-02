
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
library(terra)
library(tidyterra)

library(broom)
#install.packages("nngeo")
library(nngeo)

################################################################################
########################            Define the directory              ##########
################################################################################

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
site_number <- "1.Walpeup_MRS125"
site_name <- "Walpeup_MRS125"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.type <- "Harvest"

subfolder1 <- "testing_cleaning_trimming_etc"

raw_data <- st_read(
  paste0(
    headDir,
    "/",
    "/10.Analysis/25/Harvest/testing_cleaning_trimming_etc/",
    "Rawish_data_projected_joined/",
    "MRS125_Harvest_MGA_clipped1HeadlandRemoved.shp"))
plot(raw_data)


### clip it to the block boundary
strips <- st_read(
  paste0(
    headDir,
    "/",
    "/10.Analysis/25/Harvest/testing_cleaning_trimming_etc/",
    "Rawish_data_projected_joined/",
    "MRS125_Strips_FINAL_MGA.shp"))

### add in zones
zones <- st_read(
  paste0(
    headDir,
    "/",
    "/10.Analysis/25/Harvest/testing_cleaning_trimming_etc/",
    "Rawish_data_projected_joined/",
    "MRES125_NewZONES_MGA_clipped.shp"))


# Set the CRS to match another dataframe
st_crs(raw_data) == st_crs(strips)
st_crs(raw_data) == st_crs(zones)

zones <- st_transform(zones, st_crs(strips))

# Clip raw_data to strips
raw_data_clipped <- st_intersection(raw_data, strips)
plot(raw_data_clipped)

str(raw_data_clipped)
names(raw_data_clipped)
summary(raw_data_clipped)

# Spatial join - adds strips attributes to points that fall inside
#raw_data_clipped_with_strips <- st_join(raw_data_clipped, strips) # I must of did this already
raw_data_clipped_with_zone <- st_join(raw_data_clipped, zones)


str(raw_data_clipped_with_zone)
plot(raw_data_clipped_with_zone)


st_write(raw_data_clipped_with_zone, 
         paste0(headDir, '/10.Analysis/25/', analysis.type,
                "/", subfolder1,
                "/Rawish_data_projected_joined/", "raw_data_clipped_with_zone.shp"),
         delete_dsn = TRUE)
