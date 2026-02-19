
library(purrr)
library(readr)
library(stringr)
library(terra)
library(sf)
library(ggplot2)
library(multcomp)
library(dplyr)
library(tidyr)
library(broom)
library(multcompView)
library(lubridate)
library(readxl)



# site_number <- "1.Walpeup_MRS125"
# site_name <- "Walpeup_MRS125"

# site_number <-"2.Crystal_Brook_Brians_House" 
# site_name <-  "Crystal_Brook_Brians_House"

# site_number <- "3.Wynarka_Mervs_West"
# site_name <- "Wynarka_Mervs_West"

# site_number <- "4.Wharminda"
# site_name <- "Wharminda"

# site_number <- "5.Walpeup_Gums"
# site_name <- "Walpeup_Gums"

site_number <- "6.Crystal_Brook_Randals"
site_name <- "Crystal_Brook_Randals"


dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)
headDir

metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

#################################################################################

path <- paste0(headDir,"/10.Analysis/25/Processing_Jackie/")
# Get all CSV files in the directory
csv_files <- list.files(path = path, 
                        pattern = "\\.csv$", 
                        full.names = TRUE)




# Get just the filenames (no paths)
# Create dataframe with file number and name
# Create dataframe with file number, name, and dates
file_df <- data.frame(
  file_number = 1:length(csv_files),
  file_name = basename(csv_files),
  date_created = file.info(csv_files)$ctime,
  full_path = csv_files,
  date_modified = file.info(csv_files)$mtime,
  file_size = file.info(csv_files)$size
)
file_df



  #dat_combined <- csv_files[1:8] %>%
  #dat_combined <- csv_files[1:3] %>%
  #dat_combined <- csv_files[1:6] %>%
  dat_combined <- csv_files[1:7] %>%
  lapply(function(file) {
    read_csv(file, col_types = cols(date_field_observation = col_character())) %>%
      mutate(source_file = basename(file))
  }) %>%
  bind_rows()

################################################################################
## Join the zone and treatments

file_path_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "location of file and details") %>% 
  filter(Site == site_number)

zones <- st_read(
  paste0(headDir,file_path_details$`location of zone shp`))
strip <- st_read(
  paste0(headDir,file_path_details$`trial.plan`))

strip <- strip %>% rename(treat_id = id)

## convert combined data into pt shape file

# Convert dataframe to sf object
dat_combined_sf <- st_as_sf(dat_combined, 
                   coords = c("easting", "northing"),
                   crs = "EPSG:7854")
  
  
# Create the plot just to check location 
ggplot() +
   geom_sf(data = zones, fill = NA, color = "red", linewidth = 0.5) +
  geom_sf(data = strip, fill = NA, color = "blue", linewidth = 0.5) +
  geom_sf(data = dat_combined_sf, size = 2) 
  

# points are located in zone and strips
names(zones)
zones <- zones %>% rename("zone" = "cluster"  ,
                          #"zone" = "cluster3"  ,
                          #"zone" = "fcl_mdl"  ,
                          #"zone" = "gridcode"  ,
                          "zone_ha" = "POLY_AREA")
names(strip)
strip <- strip %>% rename("strip_ha" = "POLY_AREA") 
#strip <- strip %>% rename("strip_ha" = "POLY_AREA") %>% dplyr::select(-"rep" )

# Spatial join to find which zone polygon each point falls into
dat_with_zone <- st_join(dat_combined_sf, zones)

# Spatial join to find which strip polygon each point falls into
dat_with_both <- st_join(dat_with_zone, strip)

# View the result
str(dat_with_both)  
  
############################################################################
# Write to Shapefile
write_sf(dat_with_both,
         paste0(headDir,"/10.Analysis/25/Processing_Jackie/merged_pt_sampling/", 
                "plant_sample_merged_2025.shp"))


# Write to CSV

# Extract coordinates and write to CSV
dat_with_both_csv <- dat_with_both %>%
  mutate(easting = st_coordinates(.)[,1],
         northing = st_coordinates(.)[,2]  ) %>%  
  st_drop_geometry()  # Remove the geometry column
str(dat_with_both_csv)



write.csv(dat_with_both_csv, 
          paste0(headDir,"/10.Analysis/25/Processing_Jackie/merged_pt_sampling/", 
                 "plant_sample_merged_2025.csv"),
          row.names = FALSE)


write.csv(dat_combined, 
          paste0(headDir,"/10.Analysis/25/Processing_Jackie/merged_pt_sampling/", 
                 "plant_sample_merged_2025_no_zone_strips.csv"),
          row.names = FALSE)

