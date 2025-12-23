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



# analysis.type <- "Emergence" #Harvest, InSeason, PeakBiomass, PreSeason
# variable <- "Total"
# treat.col.name <- "treat_desc"

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
## Read in field data 


collated_data <- st_read(paste0(headDir,
                        "/10.Analysis/25/Processing_Jackie/", 
                        site_name,"_collated_data_raw_", 
                        analysis.yr,".shp"))


################################################################################
################ Read in Sentinel-2 data ########################


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

################################################################################
## read in the sentinel stack
################################################################################

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


################################################################################

sen.dat
# #Note: Crystal Brook straddles 2x tiles, but the difference is not noticeable
# #We can simply just drop one of the tile sets.
nm <- names(sen.dat)
# Keep only the first occurrence of each unique name
sen.dat <- sen.dat[[!duplicated(nm)]]

################################################################################
### work out which images are closest to sampling event

list_sentinel_dates_df <- data.frame(sential_dates= nm)
str(list_sentinel_dates_df)
str(collated_data)

collated_data <- collated_data %>%  mutate(date = ymd_hms(date))
sampling_date <- collated_data %>% 
  st_drop_geometry() %>%
  distinct(date, .keep_all = TRUE) %>% 
  select(fld_ob, date)

list_sentinel_dates_df <- list_sentinel_dates_df %>%  
  dplyr::mutate(date_ob1 = sampling_date$date[1],
                Fld_ob1 = sampling_date$fld_ob[1],
                
                date_ob2 = sampling_date$date[2],
                Fld_ob2 = sampling_date$fld_ob[2])

str(list_sentinel_dates_df)
list_sentinel_dates_df$sential_dates <- as.Date(list_sentinel_dates_df$sential_dates)
list_sentinel_dates_df$date_ob1 <- as.Date(list_sentinel_dates_df$date_ob1)
list_sentinel_dates_df$date_ob2 <- as.Date(list_sentinel_dates_df$date_ob2)

list_sentinel_dates_df <- list_sentinel_dates_df %>%  
  dplyr::mutate(date_from_fld_ob1 = abs(sential_dates - date_ob1),
                date_from_fld_ob2 = abs(sential_dates - date_ob2))
   
list_sentinel_dates_df$date_from_fld_ob1 <- as.double(list_sentinel_dates_df$date_from_fld_ob1)
list_sentinel_dates_df$date_from_fld_ob2 <- as.double(list_sentinel_dates_df$date_from_fld_ob2)


str(nm_df)

date_closeset_to_fld_ob <-  list_sentinel_dates_df %>%
  filter(
    date_from_fld_ob1 == min(date_from_fld_ob1, na.rm = TRUE) |
    date_from_fld_ob2 == min(date_from_fld_ob2, na.rm = TRUE)
  )



closest_date <- date_closeset_to_fld_ob$sential_dates
sen.dat_closest_date <- sen.dat[[as.character(closest_date)]]
sen.dat_closest_date




################################################################################
# Extract the raster value for the sampling points
collated_data
sen.dat_closest_date



# Extract values from raster at point locations
extracted_values <- terra::extract(sen.dat_closest_date, collated_data)

# Bind all extracted columns to your data (exclude ID column)
collated_data <- bind_cols(collated_data, extracted_values[, -1])


##tidy up
rm(extracted_values, sen.dat_closest_date, closest_date, list_sentinel_dates_df)
rm(bad_dates, bad_dates_year, bad_dates_list, date_8, dates_hy, dates_chr, drop_idx,
   img_dates_sen, nm, nm_df, o, sen.dat)

## make one clm for all the Sentinel dates
date_closeset_to_fld_ob

#slit into 2 files
#Fld_ob1 is the Emergence
Fld_ob1 <- date_closeset_to_fld_ob %>% 
  select(Fld_ob1, sential_dates , date_from_fld_ob1) %>% 
  filter(date_from_fld_ob1 == min(date_from_fld_ob1, na.rm = TRUE)) %>% 
  rename(fld_ob = Fld_ob1)

#Fld_ob2 is the PeakBiomass
Fld_ob2 <- date_closeset_to_fld_ob %>% 
  select(Fld_ob2, sential_dates , date_from_fld_ob2) %>% 
  filter(date_from_fld_ob2 == min(date_from_fld_ob2, na.rm = TRUE))%>% 
  rename(fld_ob = Fld_ob2)

str(collated_data)

collated_data <- collated_data %>% 
  mutate(sential_dates = case_when(
    fld_ob == "Emergence" ~ Fld_ob1$sential_dates,
    fld_ob == "PeakBiomass" ~ Fld_ob2$sential_dates
  ))



collated_data <- collated_data %>% 
  mutate(sential_value = case_when(
    sential_dates == "2025-05-22" ~ `2025-05-22`,
    sential_dates == "2025-09-29" ~ `2025-09-29`
  ))

collated_data <- collated_data %>% 
  select(-`2025-05-22`, -`2025-09-29`)

rm(Fld_ob1, Fld_ob2, date_closeset_to_fld_ob) 

################################################################################
