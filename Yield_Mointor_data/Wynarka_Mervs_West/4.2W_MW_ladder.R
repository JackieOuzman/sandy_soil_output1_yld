## Stirling Roberton
## Purpose: This script is built to undertake base analsysis of trial strip data

## 1) Run analysis on observed data:                  i) whole of paddock; ii) By Zone

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
library(readxl)
library(broom)


################################################################################
########################            Define the directory              ##########
################################################################################
# site_number <- "1.Walpeup_MRS125"
# site_name <- "Walpeup_MRS125"

# site_number <-"2.Crystal_Brook_Brians_House" 
# site_name <-  "Crystal_Brook_Brians_House"

site_number <- "3.Wynarka_Mervs_West"
site_name <- "Wynarka_Mervs_West"



dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.type <- "Harvest"


clean.dat <- "No"
analysis.yr <- "25"


metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

crs_used <- 7854
################################################################################
########################    Read in metadata info file names and path ##########
################################################################################
# List all sheet names
sheet_names_metadata <- excel_sheets( paste0(metadata_path,metadata_file_name))
print(sheet_names_metadata)

zone_shapefile_path <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "location of file and details") %>% 
  filter(Site == site_number) %>% 
  select(`location of zone shp`) %>% 
  pull()

zone_shapefile_path

harvest_data_file <-  readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "Harvest_data") %>% 
  filter(Site == site_number)

file_path_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "location of file and details") %>% 
  filter(Site == site_number)

################################################################################
# Load the files 

# harvest_raw <- st_read(
#   paste0(headDir,"/", harvest_data_file$files))

## This file is the raw data supplied projected in QGIS thinned only 
#1.Walpeup had header rows removed manually 

harvest_raw_ish <- st_read(
  paste0(headDir,"/", harvest_data_file$files))


str(harvest_raw_ish)
#############
## work out which clm to use ### Stirling doesn't want to adjust for moisture

harvest_raw_ish
names(harvest_raw_ish)
### what is "CRUDEFI"    "CrdPrPr"

harvest_raw_ish %>% dplyr::distinct(Machine)

##subset the data

harvest_raw_ish <- harvest_raw_ish %>% 
  #select(DryYield,  Easting, Northing  )
  select(VRYIELDMAS,  Easting, Northing  )
names(harvest_raw_ish)



###############################################################################
#Load in the zones
## Read in data
zones <- st_read(
  paste0(headDir,file_path_details$`location of zone shp`))
strip <- st_read(
  paste0(headDir,file_path_details$`trial.plan`))

################################################################################
### Load in the ladders that I made in qgis using Christina tools

ladders_qgis <-st_read(
  paste0(headDir,"/", harvest_data_file$ladder_shapefile))
  
 

########################################################


################################################################################

### add ladder ID to yield data
# Spatial join - adds polygon attributes to points

ggplot() +
  geom_sf(data = harvest_raw_ish, color = "red", size = 0.5) +
  geom_sf(data = ladders_qgis, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = zones, fill = NA, color = "blue", linewidth = 0.5) +
  coord_sf(xlim = st_bbox(ladders_qgis)[c(1, 3)],  # xmin, xmax
           ylim = st_bbox(ladders_qgis)[c(2, 4)]) +  # ymin, ymax
  theme_minimal()

### keep only the yld value in the ladders

harvest_clipped <- st_intersection(harvest_raw_ish, 
                                   st_union(ladders_qgis))

ggplot() +
  geom_sf(data = harvest_clipped, color = "red", size = 0.5) +
  geom_sf(data = ladders_qgis, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = zones, fill = NA, color = "blue", linewidth = 0.5) +
  coord_sf(xlim = st_bbox(ladders_qgis)[c(1, 3)],  # xmin, xmax
           ylim = st_bbox(ladders_qgis)[c(2, 4)]) +  # ymin, ymax
  theme_minimal()  

#append the ladder info to the yld data

yld_data_with_ladders <- st_join(harvest_clipped, ladders_qgis, join = st_within)

str(yld_data_with_ladders)
#append the zone info to the yld data
names(zones)
zones <- zones %>% rename(zone = fcl_mdl, clust_ha = POLY_AREA)
                   #rename(zone = cluster, clust_ha = POLY_AREA)

yld_data_with_ladders_clus <- st_join(yld_data_with_ladders, zones, join = st_within)
str(yld_data_with_ladders_clus)
################################################################################



yld_data_with_ladders_clus <- yld_data_with_ladders_clus %>% rename(Ladder_PointID = PointID)
## the ladderID is the same number acrosss all treatments

yld_data_with_ladders_clus <- yld_data_with_ladders_clus %>% 
  mutate(treat_Ladder_PointID = paste0(treat, "_", Ladder_PointID))

str(yld_data_with_ladders_clus)
#
count(yld_data_with_ladders_clus %>% dplyr::distinct(treat_Ladder_PointID))
unique(yld_data_with_ladders_clus$treat_Ladder_PointID)



yld_data_with_summary <- yld_data_with_ladders_clus %>% 
  group_by(treat_Ladder_PointID, treat, treat_id, treat_desc, Ladder_PointID) %>% 
  summarise(mean_yld = mean(VRYIELDMAS, na.rm = TRUE),
            #mean_yld = mean(DryYield, na.rm = TRUE),
            mean_zone = round(mean(zone, na.rm = TRUE)),
            n_yld_pt = n(),
            .groups = "drop")  # This automatically keeps geometry for sf objects

# Convert MULTIPOINT to centroid POINT
yld_data_with_summary_pt <- yld_data_with_summary %>% 
  st_centroid()


ggplot() +
  geom_sf(data = yld_data_with_summary_pt, color = "red", size = 1) +
  geom_sf(data = ladders_qgis, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = zones, fill = NA, color = "blue", linewidth = 0.5) +
  coord_sf(xlim = st_bbox(ladders_qgis)[c(1, 3)],  # xmin, xmax
           ylim = st_bbox(ladders_qgis)[c(2, 4)]) +  # ymin, ymax
  theme_minimal()

str(yld_data_with_summary_pt)

st_write(yld_data_with_summary_pt, 
         paste0(headDir,"/8.Yield_Data/25/Processed/",
                "Yld_data_av_to_ladder.shp"),
         delete_dsn = TRUE)


# Add X and Y coordinates as columns
yld_data_with_summary_pt_coords <- yld_data_with_summary_pt %>%
  mutate(
    X = st_coordinates(.)[,1],
    Y = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()  # Remove geometry column

# Write to CSV
write.csv(
  yld_data_with_summary_pt_coords,
  paste0(
    headDir,
    "/8.Yield_Data/25/Processed/",
    "Yld_data_av_to_ladder.csv"
  ),
  row.names = FALSE
)


unique(yld_data_with_summary_pt$treat)
