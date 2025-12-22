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

## Read in field data 
boundary   <- st_read(file.path(headDir, file_path_details$boundary))
strips <-     st_read(file.path(headDir, file_path_details$trial.plan))
strips <- st_make_valid(strips) #Checks whether a geometry is valid, or makes an invalid geometry valid


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
rm(dat.raw,data.pts,  dat.all)
data.raw <- data_sf #renaming data into generic name


#assign the control when named vaguely

strips <- strips %>%
  mutate(treat_desc = 
           case_when(
             treat_desc == "Control (-Tillage -Lime)" ~ "Control",
             treat_desc == "Control.." ~ "Control",
             .default = as.character(treat_desc)
           ) )

data.crop <- st_crop(data.raw[,variable],strips)
names(data.crop)[1] <- "target.variable" #renames 1st clm heading with a generic name
data.crop <- st_intersection( data.crop,  strips)
data.crop <- data.crop %>% mutate(field_ob = analysis.type,
                                  date_of_field_ob = field_details$Date_collected
                                  )
rm(data.raw, data_sf)
###############################################################################
###############################################################################
# Get the spatial data

################################################################################
############################  Sentinel-2   #############################
################################################################################
# ====================== list of bad dates for satellite ======================

bad_dates <- readxl::read_excel(
  paste0(file.path(headDir, "7.In_Season_data", 
                   "Sentinel_list_bad_dates.xlsx") ))
bad_dates$Dates <- as.character(bad_dates$Dates)

# Convert to a list where each column becomes a list element
bad_dates_list <- as.list(bad_dates)


ratio_name <- 
  "NDVI" 
#"EVI2" 
# "ExG" 
# "NDMI" 
# "NDRE" 

ratio_type <- paste0(ratio_name , "_Stack")

str(file_path_details)
file_path <- paste0(file_path_details$`Generic path`,
                    site_number, "/",
                    "7.In_Season_data/", 
                    analysis.yr, 
                    "/2.Satellite_Imagery/",
                    "Sentinel/", 
                    ratio_name,
                    "_Stack_", 
                    file_path_details$`sential file name extension`,
                    ".tif")
file_path



sen.dat <- terra::rast(file_path)
sen.dat <- terra::project(sen.dat,'epsg:4326')
nm <- names(sen.dat)



##### REMOVE CLOUD IMAGES - Future task to automate this!!

# ---- 1) Parse acquisition dates from layer names (robust) ----

# prefer 8-digit yyyymmdd anywhere in the name; fallback to yyyy-mm-dd
dates_8   <- stringr::str_extract(nm, "(?<!\\d)\\d{8}(?!\\d)")
dates_hy  <- stringr::str_extract(nm, "\\d{4}-\\d{2}-\\d{2}")
dates_chr <- ifelse(!is.na(dates_8), dates_8, gsub("-", "", dates_hy))

if (any(is.na(dates_chr))) {
  stop("Could not parse dates for layers: ", paste(nm[is.na(dates_chr)], collapse = ", "))
}

img_dates_sen <- as.Date(dates_chr, format = "%Y%m%d")

# ---- 2) Make order deterministic: sort by date (oldest â newest) ----
o <- order(img_dates_sen)
sen.dat        <- sen.dat[[o]]
img_dates_sen  <- img_dates_sen[o]

# Give layers clean, informative names
names(sen.dat) <- format(img_dates_sen, "%Y-%m-%d")



bad_dates_year <- bad_dates_list$Dates 
if (is.null(bad_dates_year)) bad_dates_year <- character(0)  
# Accept both "YYYY-mm-dd" and "YYYYmmdd"
bad_dates_year <- as.Date(bad_dates_year, tryFormats = c("%Y-%m-%d", "%Y%m%d"))
drop_idx <- which(img_dates_sen %in% bad_dates_year)  

if (length(drop_idx)) {
  message("Dropping ", length(drop_idx), " Sentinel layers by date: ",
          paste(format(img_dates_sen[drop_idx], "%Y-%m-%d"), collapse = ", "))
  sen.dat       <- sen.dat[[-drop_idx]]
  img_dates_sen <- img_dates_sen[-drop_idx]
  names(sen.dat) <- format(img_dates_sen, "%Y-%m-%d")
}  

## I am having trouble writing this new raster 
sen.dat
# #Note: Crystal Brook straddles 2x tiles, but the difference is not noticeable
# #We can simply just drop one of the tile sets.
nm <- names(sen.dat)


# Keep only the first occurrence of each unique name
sen.dat <- sen.dat[[!duplicated(nm)]]

nm_df <- data.frame(sential_dates= nm)
str(nm_df)
nm_df <- nm_df %>%  
  dplyr::mutate(date_ob = field_details$Date_collected[1],
                Fld_ob = field_details$analysis_type[1])

nm_df$sential_dates <- as.Date(nm_df$sential_dates)
nm_df$date_ob <- as.Date(nm_df$date_ob)

nm_df <- nm_df %>%  
  dplyr::mutate(date_from_fld_ob = abs(sential_dates - date_ob))%>%
  arrange(date_from_fld_ob) 
nm_df$date_from_fld_ob <- as.double(nm_df$date_from_fld_ob)
str(nm_df)

date_closeset_to_fld_ob <- nm_df %>% 
  filter(date_from_fld_ob == min(date_from_fld_ob))

closest_date <- date_closeset_to_fld_ob$sential_dates[1]
sen.dat_closest_date <- sen.dat[[as.character(closest_date)]]
sen.dat_closest_date


################################################################################
# Extract the raster value for the sampling points
data.pts_proj
sen.dat_closest_date



# Extract values from raster at point locations
extracted_values <- terra::extract(sen.dat_closest_date, data.pts_proj)

# Add the extracted values to your points data frame
data.pts_proj$raster_value <- extracted_values[, 2]  # Column 2 contains the values

################################################################################
data.pts_proj <- data.pts_proj %>% 
  select(pt_id, raster_value) %>%
  st_drop_geometry()  # Convert to regular data frame
################################################################################

data.crop <- cbind(data.crop, data.pts_proj) %>% 
  mutate(sential_date = date_closeset_to_fld_ob$sential_dates,
         image_type = paste0("sential_",ratio_name))
data.crop
data.crop_df <- data.crop %>% 
  st_drop_geometry()  # Convert to regular data frame
data.crop_df

################################################################################
savedfilename <- paste0("sential_", analysis.type,"_extracted_pts.csv")

location_for_saving <- paste0(headDir,"/10.Analysis/",analysis.yr,'/',analysis.type,"/"
       ,savedfilename)

write_csv(data.crop_df ,location_for_saving )
