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

################################################################################
########################            Define the directory              ##########
################################################################################

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
site_number <- "1.Walpeup_MRS125"
site_name <- "Walpeup_MRS125"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.type <- "Emergence" #Harvest, InSeason, PeakBiomass, PreSeason
variable <- "Total"
treat.col.name <- "treat_desc"

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


field_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "location of observation data") %>% 
  filter(Site == site_number) %>% 
  filter(analysis_type == analysis.type )



################################################################################







################################################################################

## Read in data
boundary   <- st_read(file.path(headDir, file_path_details$boundary))
strips <-     st_read(file.path(headDir, file_path_details$trial.plan))
strips <- st_make_valid(strips) #Checks whether a geometry is valid, or makes an invalid geometry valid

#data.raw <- st_read(paste0(headDir,'/7.In_Season_data/24/4.Biomass/Biomass_NDVI_Walpeup_2024_merged_data.gpkg'))

zones <- rast(paste0(file.path(headDir, file_path_details$`location of zone tif`)))
zones <- terra::project(zones,paste0('epsg:',crs_used),method='near')



dat.raw <-   read_excel(file.path(headDir,  field_details$data))
data.pts <-  st_read(file.path(headDir,   field_details$sampling_GPS))
data.pts_proj <- st_transform(data.pts, crs= crs_used)
## remove some clm to help with join - this is not required with every data set
# data.pts_proj <- data.pts_proj %>% select(- c("pt_uuid", "site",
#                                               "location", "treat_desc",
#                                               "treat")) #remove some clm to help with the join
data.pts_proj <- data.pts_proj %>% select("pt_id", "geometry" ) 

  
names(dat.raw)
names(data.pts_proj)
  
dat.all <- inner_join(dat.raw,data.pts_proj,by = "pt_id")
data_sf <- st_as_sf(dat.all) #converts the data frame into sf object
str(data_sf)
rm(dat.raw,data.pts, data.pts_proj, dat.all)


################################################################################
data.raw <- data_sf #renaming data into generic name


#assign the control when named vaguely

strips <- strips %>%
  mutate(treat_desc = 
           case_when(
             treat_desc == "Control (-Tillage -Lime)" ~ "Control",
             treat_desc == "Control.." ~ "Control",
             .default = as.character(treat_desc)
           ) )
         
         




################################################################################



site.info <- list(
  site_id    = site_name,
  boundary   = boundary,      # sf
  trial_plan = strips,    # sf
  seasons    = seasons        # tibble
)
class(site.info) <- c("ssii_site", class(site.info))

###############################################################################
###############################################################################
## Step 2) Clean observation data (if applicable)

## Step 2.1) Clip to variable of interest and crop to trial area
names(data.raw)


data.crop <- st_crop(data.raw[,variable],strips)
names(data.crop)[1] <- "target.variable" #renames 1st clm heading with a generic name

if(clean.dat=="Yes"){ 
  Q1 <- quantile(data.crop$target.variable, 0.25)
  Q3 <- quantile(data.crop$target.variable, 0.75)
  
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  data.clean <- data.crop %>%
    filter(target.variable >= lower_bound & target.variable <= upper_bound)
  
}else{
  data.clean <- data.crop
}

rm(data.raw, data.crop, data_sf)

###############################################################################
## Step 3) Drill treatments and zones


treat.drilled <- st_intersection( data.clean,  strips)

zones.drilled <- terra::extract(zones,treat.drilled)
names(zones.drilled)[2] <- "zone" #renames 2nd clm heading with a generic name

all.dat <- na.omit(cbind(treat.drilled,zones.drilled))

rm(treat.drilled, zones.drilled)
###############################################################################
## Save outputs ready for the next step.

str(all.dat)

write_sf(all.dat, 
          paste0(headDir,"/10.Analysis/25/Processing_Jackie/", analysis.type, "_", analysis.yr,".shp"))



