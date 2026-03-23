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

# site_number <- "3.Wynarka_Mervs_West"
# site_name <- "Wynarka_Mervs_West"

 

site_number <- "98.Auxillary_Sites/11.Qualeup_Spanners"
site_name <- "Auxillary_Sites_11.Qualeup_Spanners"


dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.type <- "Harvest"


clean.dat <- "No"
#analysis.yr <- "25"


metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

crs_used <- 7850
################################################################################
########################    Read in metadata info file names and path ##########
################################################################################
# List all sheet names
sheet_names_metadata <- excel_sheets( paste0(metadata_path,metadata_file_name))
print(sheet_names_metadata)

zone_shapefile_path <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>% 
  filter(Site == site_number) %>% 
  filter(variable == "location of zone shp") %>% 
  pull("file path")
strips_shapefile_path <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>% 
  filter(Site == site_number) %>% 
  filter(variable == "trial.plan") %>% 
  pull("file path")

  
ladders_data_file_path <-  readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>% 
  filter(Site == site_number) %>% 
  filter(variable == "ladder polygons") %>% 
  pull("file path")

### This site has multiple years

harvest_data_file_path_2020 <-  readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>% 
  filter(Site == site_number) %>% 
  filter(variable == "yield files projected thinned 2020") %>% 
  pull("file path")


harvest_data_file_path_2021 <-  readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>% 
  filter(Site == site_number) %>% 
  filter(variable == "yield files projected thinned 2021") %>% 
  pull("file path")

harvest_data_file_path_2022 <-  readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>% 
  filter(Site == site_number) %>% 
  filter(variable == "yield files projected thinned 2022") %>% 
  pull("file path")

harvest_data_file_path_2023 <-  readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>% 
  filter(Site == site_number) %>% 
  filter(variable == "yield files projected thinned 2023") %>% 
  pull("file path")



zone_shapefile_path
strips_shapefile_path
ladders_data_file_path

harvest_data_file_path_2020
harvest_data_file_path_2021
harvest_data_file_path_2022
harvest_data_file_path_2023

################################################################################
# Load the files 

# harvest_raw <- st_read(
#   paste0(headDir,"/", harvest_data_file$files))

## This file is the raw data supplied projected in QGIS thinned only 
#1.Walpeup had header rows removed manually 

harvest_raw_ish_2020 <- st_read(paste0(headDir,"/", harvest_data_file_path_2020))
harvest_raw_ish_2021 <- st_read(paste0(headDir,"/", harvest_data_file_path_2021))
harvest_raw_ish_2022 <- st_read(paste0(headDir,"/", harvest_data_file_path_2022))
harvest_raw_ish_2023 <- st_read(paste0(headDir,"/", harvest_data_file_path_2023))


#############
## work out which clm to use ### Stirling doesn't want to adjust for moisture
names(harvest_raw_ish_2020)
names(harvest_raw_ish_2021)
names(harvest_raw_ish_2022)
names(harvest_raw_ish_2023)



##subset the data

harvest_raw_ish_2020 <- harvest_raw_ish_2020 %>% select(YldMassDry,  Easting, Northing  ) %>% dplyr::mutate(year = 2020)
harvest_raw_ish_2021 <- harvest_raw_ish_2021 %>% select(YldMassDry,  Easting, Northing  ) %>% dplyr::mutate(year = 2021)
harvest_raw_ish_2022 <- harvest_raw_ish_2022 %>% select(YldMassDry,  Easting, Northing  ) %>% dplyr::mutate(year = 2022)
harvest_raw_ish_2023 <- harvest_raw_ish_2023 %>% select(YldMassDry,  Easting, Northing  ) %>% dplyr::mutate(year = 2023)

###############################################################################
#Load in the zones
## Read in data
zones <- st_read(
  paste0(headDir,zone_shapefile_path))
strip <- st_read(
  paste0(headDir,strips_shapefile_path))

unique(strip$treat_desc)

################################################################################
### Load in the ladders that I made in qgis using Christina tools

ladders_qgis <-st_read(
  paste0(headDir,"/", ladders_data_file_path))
  
unique(ladders_qgis$treat_desc)
#Just removes any white spaces
ladders_qgis <- ladders_qgis %>%
  mutate(treat_desc = stringr::str_remove_all(treat_desc, "\n") %>% trimws())
unique(ladders_qgis$treat_desc)


################################################################################

### add ladder ID to yield data
# Spatial join - adds polygon attributes to points



ggplot() +
  geom_sf(data = harvest_raw_ish_2020, color = "red", size = 0.5) +
  geom_sf(data = harvest_raw_ish_2021, color = "green", size = 0.5) +
  geom_sf(data = harvest_raw_ish_2022, color = "pink", size = 0.5) +
  geom_sf(data = harvest_raw_ish_2023, color = "orange", size = 0.5) +
  
  geom_sf(data = ladders_qgis, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = zones, fill = NA, color = "blue", linewidth = 0.5) +
  coord_sf(xlim = st_bbox(ladders_qgis)[c(1, 3)],  # xmin, xmax
           ylim = st_bbox(ladders_qgis)[c(2, 4)]) +  # ymin, ymax
  theme_minimal()

### keep only the yld value in the ladders

st_crs(ladders_qgis)$epsg
st_crs(harvest_raw_ish_2020)$epsg
st_crs(harvest_raw_ish_2021)$epsg
st_crs(harvest_raw_ish_2022)$epsg
st_crs(harvest_raw_ish_2023)$epsg



harvest_clipped_2020 <- st_intersection(harvest_raw_ish_2020, st_union(ladders_qgis))
harvest_clipped_2021 <- st_intersection(harvest_raw_ish_2021, st_union(ladders_qgis))
harvest_clipped_2022 <- st_intersection(harvest_raw_ish_2022, st_union(ladders_qgis))
harvest_clipped_2023 <- st_intersection(harvest_raw_ish_2023, st_union(ladders_qgis))


ggplot() +
  geom_sf(data = harvest_clipped_2020, color = "red", size = 0.5) +
  geom_sf(data = harvest_clipped_2021, color = "green", size = 0.5) +
  geom_sf(data = harvest_clipped_2022, color = "pink", size = 0.5) +
  geom_sf(data = harvest_clipped_2023, color = "orange", size = 0.5) +
  
  geom_sf(data = ladders_qgis, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = zones, fill = NA, color = "blue", linewidth = 0.5) +
  coord_sf(xlim = st_bbox(ladders_qgis)[c(1, 3)],  # xmin, xmax
           ylim = st_bbox(ladders_qgis)[c(2, 4)]) +  # ymin, ymax
  theme_minimal()  

#append the ladder info to the yld data

yld_data_with_ladders2020 <- st_join(harvest_clipped_2020, ladders_qgis, join = st_within)
yld_data_with_ladders2021 <- st_join(harvest_clipped_2021, ladders_qgis, join = st_within)
yld_data_with_ladders2022 <- st_join(harvest_clipped_2022, ladders_qgis, join = st_within)
yld_data_with_ladders2023 <- st_join(harvest_clipped_2023, ladders_qgis, join = st_within)


#append the zone info to the yld data
names(zones)
zones <- zones %>% rename(zone = fcl_mdl, clust_ha = POLY_AREA)
                   #rename(zone = gridcode, clust_ha = POLY_AREA)
                   #rename(zone = cluster, clust_ha = POLY_AREA)

yld_data_with_ladders_clus2020 <- st_join(yld_data_with_ladders2020, zones, join = st_within)
yld_data_with_ladders_clus2021 <- st_join(yld_data_with_ladders2021, zones, join = st_within)
yld_data_with_ladders_clus2022 <- st_join(yld_data_with_ladders2022, zones, join = st_within)
yld_data_with_ladders_clus2023 <- st_join(yld_data_with_ladders2023, zones, join = st_within)



################################################################################



yld_data_with_ladders_clus2020 <- yld_data_with_ladders_clus2020 %>% rename(Ladder_PointID = PointID)
yld_data_with_ladders_clus2021 <- yld_data_with_ladders_clus2021 %>% rename(Ladder_PointID = PointID)
yld_data_with_ladders_clus2022 <- yld_data_with_ladders_clus2022 %>% rename(Ladder_PointID = PointID)
yld_data_with_ladders_clus2023 <- yld_data_with_ladders_clus2023 %>% rename(Ladder_PointID = PointID)

## the ladderID is the same number acrosss all treatments

yld_data_with_ladders_clus2020 <- yld_data_with_ladders_clus2020 %>%mutate(treat_Ladder_PointID = paste0(treat, "_", Ladder_PointID))
yld_data_with_ladders_clus2021 <- yld_data_with_ladders_clus2021 %>%mutate(treat_Ladder_PointID = paste0(treat, "_", Ladder_PointID))
yld_data_with_ladders_clus2022 <- yld_data_with_ladders_clus2022 %>%mutate(treat_Ladder_PointID = paste0(treat, "_", Ladder_PointID))
yld_data_with_ladders_clus2023 <- yld_data_with_ladders_clus2023 %>%mutate(treat_Ladder_PointID = paste0(treat, "_", Ladder_PointID))


#

yld_data_with_summary2020 <- yld_data_with_ladders_clus2020 %>% 
  group_by(treat_Ladder_PointID, treat, treat_id, treat_desc, Ladder_PointID) %>% 
  summarise(mean_yld = mean(YldMassDry, na.rm = TRUE),
            mean_zone = round(mean(zone, na.rm = TRUE)),
            n_yld_pt = n(),
            .groups = "drop")  # This automatically keeps geometry for sf objects


yld_data_with_summary2021 <- yld_data_with_ladders_clus2021 %>% 
  group_by(treat_Ladder_PointID, treat, treat_id, treat_desc, Ladder_PointID) %>% 
  summarise(mean_yld = mean(YldMassDry, na.rm = TRUE),
            mean_zone = round(mean(zone, na.rm = TRUE)),
            n_yld_pt = n(),
            .groups = "drop")  # This automatically keeps geometry for sf objects


yld_data_with_summary2022 <- yld_data_with_ladders_clus2022 %>% 
  group_by(treat_Ladder_PointID, treat, treat_id, treat_desc, Ladder_PointID) %>% 
  summarise(mean_yld = mean(YldMassDry, na.rm = TRUE),
            mean_zone = round(mean(zone, na.rm = TRUE)),
            n_yld_pt = n(),
            .groups = "drop")  # This automatically keeps geometry for sf objects

yld_data_with_summary2023 <- yld_data_with_ladders_clus2023 %>% 
  group_by(treat_Ladder_PointID, treat, treat_id, treat_desc, Ladder_PointID) %>% 
  summarise(mean_yld = mean(YldMassDry, na.rm = TRUE),
            mean_zone = round(mean(zone, na.rm = TRUE)),
            n_yld_pt = n(),
            .groups = "drop")  # This automatically keeps geometry for sf objects

# Convert MULTIPOINT to centroid POINT
yld_data_with_summary2020_pt <- yld_data_with_summary2020 %>% st_centroid() %>% dplyr::mutate(year = 2020)
yld_data_with_summary2021_pt <- yld_data_with_summary2021 %>% st_centroid() %>% dplyr::mutate(year = 2021)
yld_data_with_summary2022_pt <- yld_data_with_summary2022 %>% st_centroid() %>% dplyr::mutate(year = 2022)
yld_data_with_summary2023_pt <- yld_data_with_summary2023 %>% st_centroid() %>% dplyr::mutate(year = 2023)

# Check for mixed types
st_geometry_type(yld_data_with_summary2020_pt) |> unique()
st_geometry_type(yld_data_with_summary2021_pt) |> unique()
st_geometry_type(yld_data_with_summary2022_pt) |> unique()
st_geometry_type(yld_data_with_summary2023_pt) |> unique()



ggplot() +
  geom_sf(data = yld_data_with_summary2020_pt, color = "red", size = 1) +
  geom_sf(data = yld_data_with_summary2021_pt, color = "pink", size = 1) +
  geom_sf(data = yld_data_with_summary2022_pt, color = "green", size = 1) +
  geom_sf(data = yld_data_with_summary2023_pt, color = "orange", size = 1) +
  
  
  geom_sf(data = ladders_qgis, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = zones, fill = NA, color = "blue", linewidth = 0.5) +
  coord_sf(xlim = st_bbox(ladders_qgis)[c(1, 3)],  # xmin, xmax
           ylim = st_bbox(ladders_qgis)[c(2, 4)]) +  # ymin, ymax
  theme_minimal()


yld_data_with_summary_pt <- rbind(yld_data_with_summary2020_pt,
                                  yld_data_with_summary2021_pt,  
                                  yld_data_with_summary2022_pt, 
                                  yld_data_with_summary2023_pt)

# Check for mixed types
st_geometry_type(yld_data_with_summary_pt) |> unique()


st_write(yld_data_with_summary_pt, 
         paste0(headDir,"/8.Yield_Data/Processed/",
                "Yld_data_av_to_ladder.shp"),
         delete_dsn = TRUE)


# Check for mixed types
st_geometry_type(yld_data_with_summary_pt) |> unique()



# Add X and Y coordinates as columns
yld_data_with_summary_pt_coords <- yld_data_with_summary_pt %>%
  dplyr::mutate(
    X = st_coordinates(.)[,1],
    Y = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()  # Remove geometry column

# Write to CSV
write.csv(
  yld_data_with_summary_pt_coords,
  paste0(
    headDir,
    "/8.Yield_Data/Processed/",
    "Yld_data_av_to_ladder.csv"
  ),
  row.names = FALSE
)



# Write shapefile per year
years <- unique(yld_data_with_summary_pt$year)

for(yr in years) {
  yld_data_with_summary_pt %>%
    filter(year == yr) %>%
    st_write(
      paste0(headDir, "/8.Yield_Data/Processed/",
             "Yld_data_av_to_ladder_", yr, ".shp"),
      delete_dsn = TRUE  # overwrite if exists
    )
  cat("Written shapefile for year", yr, "\n")
}

unique(yld_data_with_summary_pt$treat)
