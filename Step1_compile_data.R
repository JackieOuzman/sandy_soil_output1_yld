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
variable <- "total_count"
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
  filter(Site == site_number)

################################################################################

### This needs to be adjusted in order of predicted resposne?

order <- c(
  "Control",
  "Control (+Lime)",
  "Control (-Tillage -Lime)..",
  "Control..",
  
  "Active Inclusion..",
  "Bednar",
  "Bednar + Delve",
  "Chicken Litter",
  "Deep Rip..",
  "Deep_Ripping",
  "Horsch",
  "Inlcusion_Ripping",
  "Kuhn Performer..",
  "Lime Control (3t)..",
  "Lime Control (3t)",
  "Passive Inclusion..",
  "Plozza_Plough",
  "Rip",
  "Rip (+Lime)",
  "Rip + Lime (3t)",
  
  "Rip + Delve",
  "Rip + Lime (3t)..",
  "Rip + Mix (+Lime) + FUTURE",
  "Rip + Mix + FUTURE",
  "Rip + PasInclusion",
  "Rip + PasInclusion + Chicken Litter",
  "Rip + PasInclusion + Chicken Litter +Dry",
  "Rip + Spade",
  "Rip..",
  "Rip+Mix",
  "Rip+Mix (+Lime)",
  "Rotary_Spading",
  "Spade + Delve",
  "Spade + Lime (3t)..",
  "Spade + Lime (3t)",
  "Spade + Rip + Lime (3t)..",
  "Spade + Rip + Lime (3t)",
  "Spade + Rip..",
  "Spade + Rip",
  "Spade..",
  "Spade",
  "TopDown")
  
order_df <- as.data.frame(order)
order_df <- order_df %>%  dplyr::rename(Treatments = order)
## add a oder index to this order_df.

order_df$order_rank <- 1:nrow(order_df)  
  





################################################################################

## Read in data
boundary   <- st_read(file.path(headDir, file_path_details$boundary))
strips <-     st_read(file.path(headDir, file_path_details$trial.plan))
strips <- st_make_valid(strips) #Checks whether a geometry is valid, or makes an invalid geometry valid

#data.raw <- st_read(paste0(headDir,'/7.In_Season_data/24/4.Biomass/Biomass_NDVI_Walpeup_2024_merged_data.gpkg'))

zones <- rast(paste0(file.path(headDir, file_path_details$`location of zone tif`)))
zones <- terra::project(zones,paste0('epsg:',crs_used),method='near')


dat.raw <-   read.csv(file.path(headDir,  field_details$Emergence_data))
data.pts <-  st_read(file.path(headDir,   field_details$Emergence_sampling_GPS))
data.pts_proj <- st_transform(data.pts, crs= crs_used)
data.pts_proj <- data.pts_proj %>% select(- c("pt_uuid", "site",
                                              "location", "treat_desc",
                                              "treat")) #remove some clm to help with the join

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
         
         
#order the treatments by appending the order data frame to the strips
names(order_df)
names(strips)
strips <- left_join(strips, order_df, by = join_by(treat_desc == Treatments))



#### Helpers for later when plotting ####

# Zone labels used only for facet strip text
# zone_labels <- file_path_details %>% 
#   filter(Site == site_number ) %>% 
#   select(`zone label names` )
# 
# list_of_zone_labels_1 <- as.list(zone_labels)
# list_of_zone_labels_2 <- unlist(strsplit(list_of_zone_labels_1$`zone label names`, ","))
# names(list_of_zone_labels_2) <- unique(long_zone$zone_id)


# zone_desc <- c("1" = "Transition", "2" = "Dune","3" = "Swale")  # others keep their ID
# zone_labeller <- ggplot2::labeller(
#   zone_id = function(z) {
#     zc <- as.character(z)
#     desc <- zone_desc[zc]
#     ifelse(is.na(desc), zc, paste0(zc, " — ", desc))
#   }
# )

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



