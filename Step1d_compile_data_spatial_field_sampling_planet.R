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

plant_path_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "Planet data") %>% 
  filter(Site == site_number)

################################################################################
## Read in field data with the Sential and drone data too


collated_data_fld_Sent_drone <- st_read(paste0(headDir,
                        "/10.Analysis/25/Processing_Jackie/", 
                        site_name,"_collated_data_raw_Sen_drone", 
                        analysis.yr,".shp"))

################################################################################
################ Read in planet data ########################

## get the list of files

tif_files <- list.files(path = paste0(headDir, "/", plant_path_details$files), 
                        pattern = "3B_AnalyticMS_SR_8b_clip.*\\.(tif|TIF)$")


#Pull out the date part of the file names

tif_files_dates <- substr(tif_files, 1, 8)


tif_files_dates_df <- data.frame(
  tif_files_dates = tif_files_dates,
  date = as.Date(tif_files_dates, format = "%Y%m%d")
)

################################################################################
## sampling dates
str(collated_data_fld_Sent_drone)

sampling_dates <- collated_data_fld_Sent_drone %>% 
  st_drop_geometry() %>%  # Remove geometry column
  distinct(fld_ob, .keep_all = TRUE) %>% 
  select(site, fld_ob, date) %>% 
  mutate(date = as.Date(date, format = "%Y/%m/%d"))
sampling_dates


## split this into multiple 
Emergence <- sampling_dates %>%  
  filter(fld_ob == "Emergence") %>% 
  select(date) %>% 
  rename(Emergence = date)

Peak_Biomass <- sampling_dates %>%  
  filter(fld_ob == "Peak_Biomass") %>% 
  select(date) %>% 
  rename(Peak_Biomass = date)
rm(sampling_dates)
##############################################################################
## add this to our list of planet dates and calulate the difference between image and sampling dates

tif_files_dates_df <- tif_files_dates_df %>% 
  mutate(Emergence = Emergence,
         Peak_Biomass = Peak_Biomass)
         
str(tif_files_dates_df)


tif_files_dates_df <- tif_files_dates_df %>%
  mutate(days_to_emergence = abs(as.numeric(difftime(Emergence$Emergence, date, units = "days"))),
         days_to_Peak_Biomass = abs(as.numeric(difftime(Peak_Biomass$Peak_Biomass, date, units = "days"))))

head(tif_files_dates_df)

##############################################################################
## what is the closest planet image to sampling events
min_days_df <- tif_files_dates_df %>%
  filter(days_to_emergence == min(days_to_emergence) | 
           days_to_Peak_Biomass == min(days_to_Peak_Biomass))

min_emergence <- tif_files_dates_df %>%
  filter(days_to_emergence == min(days_to_emergence))
emergence_date_pattern <- min_emergence$tif_files_dates

min_peak_biomass <- tif_files_dates_df %>%
  filter(days_to_Peak_Biomass == min(days_to_Peak_Biomass))
peak_biomass_date_pattern <- min_peak_biomass$tif_files_dates


tif_file_emergence <- list.files(path = paste0(headDir, "/", plant_path_details$files), 
                                 pattern = paste0(emergence_date_pattern, ".*3B_AnalyticMS_SR_8b_clip.*\\.(tif|TIF)$"),
                                 full.names = TRUE)


tif_file_peak_biomass <- list.files(path = paste0(headDir, "/", plant_path_details$files), 
                                 pattern = paste0(peak_biomass_date_pattern, ".*3B_AnalyticMS_SR_8b_clip.*\\.(tif|TIF)$"),
                                 full.names = TRUE)

rm(min_days_df, 
   #min_peak_biomass, min_emergence,
   emergence_date_pattern, 
   peak_biomass_date_pattern, 
   Emergence, Peak_Biomass)

################################################################################
## These are the files I want  


tif_file_emergence
tif_file_peak_biomass



# Load the raster for file 1
planet_1 <- terra::rast(tif_file_emergence)
#str(planet_1)
#plot(planet_1)
#########  NDVI processing
# Access the red and NIR bands from the raster stack
red_band <- planet_1[[6]]  # 6th band is red
nir_band <- planet_1[[8]]  # 8th band is NIR
  # Calculate NDVI
ndvi_emergence <- (nir_band - red_band) / (nir_band + red_band)
  
# Load the raster for file 2
planet_2 <- terra::rast(tif_file_peak_biomass)

#########  NDVI processing
# Access the red and NIR bands from the raster stack
red_band <- planet_2[[6]]  # 6th band is red
nir_band <- planet_2[[8]]  # 8th band is NIR
# Calculate NDVI
ndvi_peak_biomass <- (nir_band - red_band) / (nir_band + red_band)

rm(red_band, nir_band, planet_1, planet_2)


################################################################################
## extract the values from the grids to match the sampling locations
collated_data_fld_Sent_drone # Projected CRS: GDA2020 / MGA zone 54
ndvi_emergence
ndvi_peak_biomass

### match the coordinates first

# Get the CRS from your shapefile
shapefile_crs <- st_crs(collated_data_fld_Sent_drone)

# Reproject the SpatRaster to match the shapefile CRS
ndvi_emergence_reprojected <- project(ndvi_emergence, shapefile_crs$wkt)
ndvi_peak_biomass_reprojected <- project(ndvi_peak_biomass, shapefile_crs$wkt)


### 
# Extract values from raster at point locations
extracted_values1 <- terra::extract(ndvi_emergence_reprojected, collated_data_fld_Sent_drone)
extracted_values2 <- terra::extract(ndvi_peak_biomass_reprojected, collated_data_fld_Sent_drone)

############# Extract 1
# Bind  extracted columns to  data (exclude ID column)
collated_data_fld_Sent_drone <- bind_cols(collated_data_fld_Sent_drone, extracted_values1[, -1])
names(collated_data_fld_Sent_drone)
# Rename the clm
collated_data_fld_Sent_drone <- collated_data_fld_Sent_drone %>% rename(NDVI_planet_emergence = "...15")

names(collated_data_fld_Sent_drone)
############# Extract 2
# Bind  extracted columns to  data (exclude ID column)
collated_data_fld_Sent_drone <- bind_cols(collated_data_fld_Sent_drone, extracted_values2[, -1])
names(collated_data_fld_Sent_drone)
# Rename the clm
collated_data_fld_Sent_drone <- collated_data_fld_Sent_drone %>% rename(NDVI_planet_peak_biomass = "...16")

names(collated_data_fld_Sent_drone)

#################################################################################
collated_data_fld_Sent_drone <- collated_data_fld_Sent_drone %>% 
  mutate(NDVI_Planet = 
           case_when(
             fld_ob == "Emergence" ~ NDVI_planet_emergence,
             fld_ob == "Peak_Biomass" ~ NDVI_planet_peak_biomass,
             .default = -999
             
           ))

collated_data_fld_Sent_drone <- collated_data_fld_Sent_drone %>% 
  select(-NDVI_planet_emergence, -NDVI_planet_peak_biomass)
################################################################################
## Dates of the planet images collected

min_emergence
min_peak_biomass

collated_data_fld_Sent_drone <- collated_data_fld_Sent_drone %>% 
  mutate(NDVI_Planet_dates = 
           case_when(
             fld_ob == "Emergence" ~ as.Date(min_emergence$date),
             fld_ob == "Peak_Biomass" ~ as.Date(min_peak_biomass$date),
             .default = as.Date(NA)
           ))
###############################################################################
## tidy up the clm names
names(collated_data_fld_Sent_drone)

collated_data_fld_Sent_drone <- collated_data_fld_Sent_drone %>% 
  rename(Fld_ob_dt = date,
         
         Dr_val  =  NDVI_Dr,
         Dr_dat  =  NDVI_D_,
         
         Plan_val = NDVI_Planet,
         Plan_dat = NDVI_Planet_dates)


################################################################################
write_sf(collated_data_fld_Sent_drone,
         paste0(headDir,"/10.Analysis/25/Processing_Jackie/", site_name,"_collated_data_raw_Sen_drone_planet", analysis.yr,".shp"))
################################################################################
# Extract coordinates and convert to data frame
collated_data_fld_Sent_drone_df <- collated_data_fld_Sent_drone %>%
  st_drop_geometry() %>%  # Remove geometry column
  bind_cols(
    st_coordinates(collated_data_fld_Sent_drone) %>%  # Extract coordinates
      as.data.frame() %>%
      rename(longitude = X, latitude = Y)  # Rename to meaningful names
  )

# Write to CSV
write.csv(collated_data_fld_Sent_drone_df,
          paste0(headDir, "/10.Analysis/25/Processing_Jackie/", 
                 site_name, "_collated_data_raw_sentinel_drone_planet", analysis.yr, ".csv"),
          row.names = FALSE)

         