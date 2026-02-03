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
site_number <- "1.Walpeup_MRS125"
site_name <- "Walpeup_MRS125"
headDir <- paste0(dir, "/work/Output-1/", site_number)

clean.dat <- "No"
analysis.yr <- "25"

metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

crs_used <- 4326 # Name: WGS 84 (World Geodetic System 1984) Type: Geographic coordinate system (latitude/longitude)
projetion_crs <- 28354 #GDA94 / MGA Zone 54 (EPSG:28354).
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

## Read in data
boundary   <- st_read(file.path(headDir, file_path_details$boundary))
strips <-     st_read(file.path(headDir, file_path_details$trial.plan))
strips <- st_make_valid(strips) #Checks whether a geometry is valid, or makes an invalid geometry valid

#data.raw <- st_read(paste0(headDir,'/7.In_Season_data/24/4.Biomass/Biomass_NDVI_Walpeup_2024_merged_data.gpkg'))

zones <- rast(paste0(file.path(headDir, file_path_details$`location of zone tif`)))
zones <- terra::project(zones,paste0('epsg:',crs_used),method='near')

### project the spatial data 
# Transform to MGA Zone 54
boundary_mga <- st_transform(boundary, crs = projetion_crs)
strips_mga <- st_transform(strips, crs = projetion_crs)
zones_mga <- project(zones, paste0("EPSG:", projetion_crs))


rm(boundary,strips,zones)

################################################################################
### Rinse and Repeat from here



#Bring in the data for each field sampling event. These analysis.type are defined by Stirling

#analysis.type <- "Emergence" #
#analysis.type <- "Peak_Biomass" #This is sometimes called biomass, or biomass at flowering 4.Peak_Biomass
analysis.type <- "Maturity" # N
#analysis.type <- "Harvest" # N

#analysis.type <- "InSeason" ## ? not sure what this is

# analysis.type <- "PreSeason" ## ? not sure what this is




field_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "location of observation data") %>% 
  filter(Site == site_number) %>% 
  filter(analysis_type == analysis.type )

variable <- field_details$variable_clm_name
treat.col.name <- "treat_desc"



#################################################################################
### sometimes you will need to specify the sheet###
dat.raw <-   read_excel(file.path(headDir,   field_details$data), sheet = "Sheet1")
data.pts <-  st_read   (file.path(headDir,   field_details$sampling_GPS))
## Note that the data in these 2 files do not always match

data.pts_wgs <- st_transform(data.pts, crs= crs_used)
data.pts_proj <- st_transform(data.pts_wgs, crs = projetion_crs)

## remove some clm to help with join - this is not required with every data set
# data.pts_proj <- data.pts_proj %>% select(- c("pt_uuid", "site",
#                                               "location", "treat_desc",
#                                               "treat")) #remove some clm to help with the join


################################################################################
##################### STOP AND CHECK ###########################################

### might need to stuff around here to makes sure the zone get imported
names(dat.raw)
names(data.pts_proj) 

#data.pts_proj <- data.pts_proj %>% select("pt_id", "geometry" ) 
#data.pts_proj <- data.pts_proj %>% select("pt_id", "geometry", "cluster3" )
data.pts_proj <- data.pts_proj %>% select("pt_id", "geometry", "cluster3",  "treat" , "treat_desc" )
#data.pts_proj <- data.pts_proj %>% select("pt_id", "geometry", "cluster3","NDVI_drone" ) 
  


names(dat.raw)
names(data.pts_proj)
  
str(dat.raw)
str(data.pts_proj)


### some quirky stuff to sort ###
## at Walpeup MRS125 maturity sample has tow pt with 28 called 28.1 and 28.2

# dat.raw <- dat.raw %>%
#   mutate(Point = case_when(
#     Point == 28.1 ~ 28,
#     Point == 28.2 ~ 28,
#     TRUE ~ Point
#   )) %>%
#   group_by(Project, Location, Paddock, Point) %>%
#   summarise(
#     `Dry Biomass` = mean(`Dry Biomass`, na.rm = TRUE),
#     Bag = mean(Bag, na.rm = TRUE),
#     `Net Biomass (g)` = mean(`Net Biomass (g)`, na.rm = TRUE),
#     `Dry Biomass (t/ha)` = mean(`Dry Biomass (t/ha)`, na.rm = TRUE),
#     `Grain (g)` = mean(`Grain (g)`, na.rm = TRUE),
#     `Grain Bag (g)` = mean(`Grain Bag (g)`, na.rm = TRUE),
#     `Net Grain (g)` = mean(`Net Grain (g)`, na.rm = TRUE),
#     `Grain Yield (t/ha)` = mean(`Grain Yield (t/ha)`, na.rm = TRUE),
#     `250 g/w` = mean(`250 g/w`, na.rm = TRUE),
#     `1000 gw (g)` = mean(`1000 gw (g)`, na.rm = TRUE),
#     `Harvest Index %` = mean(`Harvest Index %`, na.rm = TRUE),
#     Notes = first(Notes),
#     .groups = "drop"
#   )


dat.all <- inner_join(data.pts_proj,dat.raw, by = join_by("pt_id" == "Point"))
#dat.all <- inner_join(data.pts_proj,dat.raw, by = "pt_id")
data_sf <- st_as_sf(dat.all) #converts the data frame into sf object
str(data_sf)
rm(dat.raw,data.pts, data.pts_proj, dat.all)


################################################################################

#assign the control when named vaguely

strips_mga <- strips_mga %>%
  mutate(treat_desc = 
           case_when(
             treat_desc == "Control (-Tillage -Lime)" ~ "Control",
             treat_desc == "Control.." ~ "Control",
             .default = as.character(treat_desc)
           ) )
         
       



################################################################################
## Add some clms AND Retain only a subset of clms

str(data_sf)
str(field_details)

data_sf <- data_sf %>% 
  mutate(site = site_name,
         year = analysis.yr,
         field_observation = analysis.type,
         date_field_observation = field_details$Date_collected,
         variable_name =  field_details$variable_name,
         variable_units =  field_details$varibale_units
         ) 

names(data_sf)


data_sf <- data_sf %>% 
  select(site,
         pt_id, treat, treat_desc, geometry,
         year,
         field_observation,
         date_field_observation,   
         !!variable, # Base tidyeval approach (unquoting)
         # These are clms that might need modfiying
         cluster3#,
        # "NDVI_drone" 
        )


data_sf <-data_sf %>% rename("target.variable" = !!variable)

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


rm( data_sf, analysis.type, variable , field_details)


