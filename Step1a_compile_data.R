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

## Read in data
boundary   <- st_read(file.path(headDir, file_path_details$boundary))
strips <-     st_read(file.path(headDir, file_path_details$trial.plan))
strips <- st_make_valid(strips) #Checks whether a geometry is valid, or makes an invalid geometry valid

#data.raw <- st_read(paste0(headDir,'/7.In_Season_data/24/4.Biomass/Biomass_NDVI_Walpeup_2024_merged_data.gpkg'))

zones <- rast(paste0(file.path(headDir, file_path_details$`location of zone tif`)))
zones <- terra::project(zones,paste0('epsg:',crs_used),method='near')


################################################################################
### Rinse and Repeat from here



#Bring in the data for each field sampling event. These analysis.type are defined by Stirling

#analysis.type <- "Emergence" #
#analysis.type <- "Harvest" # No data yet

#analysis.type <- "InSeason" ## ? not sure what this is
analysis.type <- "PeakBiomass" #This is sometimes called biomass, or biomass at flowering
# analysis.type <- "PreSeason" ## ? not sure what this is




field_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "location of observation data") %>% 
  filter(Site == site_number) %>% 
  filter(analysis_type == analysis.type )

variable <- field_details$variable_clm_name
treat.col.name <- "treat_desc"



#################################################################################

dat.raw <-   read_excel(file.path(headDir,  field_details$data))
data.pts <-  st_read(file.path(headDir,   field_details$sampling_GPS))
data.pts_proj <- st_transform(data.pts, crs= crs_used)
## remove some clm to help with join - this is not required with every data set
# data.pts_proj <- data.pts_proj %>% select(- c("pt_uuid", "site",
#                                               "location", "treat_desc",
#                                               "treat")) #remove some clm to help with the join


################################################################################
##################### STOP AND CHECK ###########################################

### might need to stuff around here to makes sure the zone get imported
names(data.pts_proj) 
#data.pts_proj <- data.pts_proj %>% select("pt_id", "geometry" ) 
data.pts_proj <- data.pts_proj %>% select("pt_id", "geometry", "cluster3","NDVI_drone" ) 
  


names(dat.raw)
names(data.pts_proj)
  
dat.all <- inner_join(dat.raw,data.pts_proj,by = "pt_id")
data_sf <- st_as_sf(dat.all) #converts the data frame into sf object
str(data_sf)
rm(dat.raw,data.pts, data.pts_proj, dat.all)


################################################################################
data.raw <- data_sf #renaming data into generic name
rm(data_sf)

#assign the control when named vaguely

strips <- strips %>%
  mutate(treat_desc = 
           case_when(
             treat_desc == "Control (-Tillage -Lime)" ~ "Control",
             treat_desc == "Control.." ~ "Control",
             .default = as.character(treat_desc)
           ) )
         
       



################################################################################
## Add some clms AND Retain only a subset of clms

str(data.raw)
str(field_details)

data.raw <- data.raw %>% 
  mutate(site = site_name,
         year = analysis.yr,
         field_observation = analysis.type,
         date_field_observation = field_details$Date_collected,
         variable_name =  field_details$variable_name,
         variable_units =  field_details$varibale_units
         ) 

names(data.raw)


data.raw <- data.raw %>% 
  select(site,
         pt_id, treat, treat_desc, geometry,
         year,
         field_observation,
         date_field_observation,   
         !!variable, # Base tidyeval approach (unquoting)
         # These are clms that might need modfiying
         cluster3,
         "NDVI_drone" )


data.raw <-data.raw %>% rename("target.variable" = !!variable)

str(data.raw)


write_sf(data.raw,
        paste0(headDir,"/10.Analysis/25/Processing_Jackie/", analysis.type, "_", analysis.yr,".shp"))

rm( data.raw, analysis.type, variable , field_details)


###############################################################################
#### ONCE I get confirmation of all the variable this will grow.

###############################################################################
### Collate the variables into one file for the site
analysis.yr <- 25
data1 <- "Emergence"
file1 <- st_read(paste0(headDir,"/10.Analysis/25/Processing_Jackie/", 
                         data1, "_", analysis.yr,".shp"))  


data2 <- "PeakBiomass"
file2 <- st_read(paste0(headDir,"/10.Analysis/25/Processing_Jackie/", 
                        data2, "_", analysis.yr,".shp"))  


#### Getting the clm names matching
names(file1)
names(file2)

file1 <- file1 %>%
  rename("trgt_vr" = "Total" , "zone" = "clustr3") %>%
  mutate(NDVI_drone = "nothing_yet")

file2 <- file2 %>%
  rename(
         #"trgt_vr" = "Total" , 
         "zone" = "clustr3",
         "NDVI_drone" = "NDVI_dr" )

file1_2 <- rbind(file1, file2)
names(file1_2)

file1_2 <- file1_2 %>%
  rename(date = dt_fld_,
        fld_ob = fld_bsr ) %>% 
  mutate(date = ymd_hms(date))
  
  
str(file1_2)
###############################################################################
########                     Lots of zeros               ######################
###############################################################################

file1_2 <- file1_2 %>%
  mutate(across(c(trgt_vr), 
                ~case_when(. == 0 ~ NA_real_,
                           TRUE ~ .)))



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



