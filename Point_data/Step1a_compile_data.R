## Stirling Roberton and modified by Jackie
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
library(readxl)
library(readr)

################################################################################
########################            Define the directory              ##########
################################################################################

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
site_number <-"2.Crystal_Brook_Brians_House" # "1.Walpeup_MRS125"
site_name <-  "Crystal_Brook_Brians_House"# "Walpeup_MRS125"


headDir <- paste0(dir, "/work/Output-1/", site_number)


analysis.yr <- "25"

metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")

metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

crs_used <- 4326 # Name: WGS 84 (World Geodetic System 1984) Type: Geographic coordinate system (latitude/longitude)
projetion_crs <- 7854 #GDA2020 / MGA Zone 54 (EPSG:7854).

################################################################################
########################    Read in metadata info file names and path ##########
################################################################################

file_path_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "location of file and details") %>% 
  filter(Site == site_number)




### Rinse and Repeat from here



#Bring in the data for each field sampling event. These analysis.type are defined by Stirling

#analysis.type <- "Establishment" #
analysis.type <- "Establishment CV" #
#analysis.type <- "Biomass_flowering" #This is sometimes called biomass, or biomass at flowering 4.Peak_Biomass
#analysis.type <- "Biomass_maturity" # Maturity_biomass
#analysis.type <- "Grain yield" # 
#analysis.type <- "Thousand grain weight" # 
#analysis.type <- "Harvest index" # 




field_details %>%  filter(Site == site_number)


field_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "location of observation data") %>% 
  filter(Site == site_number) %>% 
  filter(analysis_type == analysis.type )

variable <- field_details$variable_clm_name
treat.col.name <- "treat_desc"



#################################################################################
### sometimes you will need to specify the sheet###
dat.raw <-   read_excel(file.path(headDir,    field_details$data), sheet = "Jackie")
#dat.raw <-   read_excel(file.path(headDir,   field_details$data), sheet = "Jackie_MRS125")
data.pts <-  st_read   (file.path(headDir,   field_details$sampling_GPS))
## Note that the data in these 2 files do not always match

# data.pts_wgs <- st_transform(data.pts, crs= crs_used)
# data.pts_proj <- st_transform(data.pts_wgs, crs = projetion_crs)
#if no projection is needed
data.pts_proj <-data.pts

################################################################################
##################### STOP AND CHECK ###########################################

### might need to stuff around here to makes sure the zone get imported
names(dat.raw)
names(data.pts_proj) 

## I cant trust that the "zone" or "cluster" always drop it from the dataset
## I wonder if its the same story for the treatments?

data.pts_proj <- data.pts_proj %>% dplyr::select(pt_id, geometry)
  
names(dat.raw)

dat.raw <- dat.raw %>%   dplyr::select(pt_id, target.variable = all_of(analysis.type))  
  

names(dat.raw)
names(data.pts_proj)
  





###############################################################################
#dat.all <- inner_join(data.pts_proj,dat.raw, by = join_by("pt_id" == "Point"))
dat.all <- inner_join(data.pts_proj,dat.raw, by = "pt_id")
data_sf <- st_as_sf(dat.all) #converts the data frame into sf object
str(data_sf)
rm(dat.raw,data.pts, data.pts_proj, dat.all)




################################################################################
## Add some clms AND Retain only a subset of clms

names(data_sf)
str(field_details)

data_sf <- data_sf %>% 
  dplyr::mutate(site = site_name,
         year = analysis.yr,
         field_observation = analysis.type, 
         date_field_observation = field_details$Date_collected,
         variable_name =  field_details$variable_name,
         variable_units =  field_details$varibale_units
         ) 

names(data_sf)
#
data_sf <- data_sf %>% 
  dplyr::select(site,
         pt_id,  geometry,
         year,
         
         field_observation,
         date_field_observation,
         target.variable,
         variable_units
         
        )


#data_sf <-data_sf %>% rename("target.variable" = !!variable)

str(data_sf)


write_sf(data_sf,
        paste0(headDir,"/10.Analysis/25/Processing_Jackie/", analysis.type, "_", analysis.yr,".shp"))


# Extract coordinates and write to CSV
data_sf_csv <- data_sf %>%
  mutate(easting = st_coordinates(.)[,1],
         northing = st_coordinates(.)[,2],
         crs = paste0("EPSG:", projetion_crs)) %>%  # Add CRS column
  st_drop_geometry()  # Remove the geometry column
str(data_sf_csv)

# Write to CSV
write.csv(data_sf_csv, 
          paste0(headDir,"/10.Analysis/25/Processing_Jackie/", analysis.type, "_", analysis.yr,".csv"),
          row.names = FALSE)


rm( data_sf, analysis.type, variable , field_details, data_sf_csv, data.pts_wgs)


