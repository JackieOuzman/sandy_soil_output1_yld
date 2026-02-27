
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
# site_number <- "101.Andrew_Thomas_Tanks"
# site_name <- "Andrew_Thomas_Tanks"

 site_number <- "1.Walpeup_MRS125"
 site_name <- "Walpeup_MRS125"

# site_number <-"2.Crystal_Brook_Brians_House" 
# site_name <-  "Crystal_Brook_Brians_House"

# site_number <- "3.Wynarka_Mervs_West"
# site_name <- "Wynarka_Mervs_West"



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



ladders_data_file_path <-  readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>% 
  filter(Site == site_number) %>% 
  filter(variable == "ladder polygons Temp") %>% 
  pull("file path")


ladders_data_file_path
################################################################################

################################################################################
### Load in the ladders that I made in qgis using Christina tools

ladders_qgis <-st_read(
  paste0(headDir,"/", ladders_data_file_path))

unique(ladders_qgis$treat_desc)
#Just removes any white spaces
ladders_qgis <- ladders_qgis %>%
  mutate(treat_desc = stringr::str_remove_all(treat_desc, "\n") %>% trimws())
unique(ladders_qgis$treat_desc)

str(ladders_qgis)
### what strips need fixing up?
list_of_strips_to_fix <- c(
  "Control (-Tillage -Lime)",
  "Lime Control (3t)"  ,
  "Rip"   ,
  "Rip + Lime (3t)"  ,
  "Spade" ,
  "Spade + Lime (3t)"
  #"Spade + Rip"        ,
 # "Spade + Rip + Lime (3t)"
)

ladders_qgis <- ladders_qgis %>%
  mutate(PointID = ifelse(
    treat_desc %in% list_of_strips_to_fix,
    max(PointID[treat_desc %in% list_of_strips_to_fix], na.rm = TRUE) + 1 - PointID,
    PointID
  ))

ladders_data_file_path

st_write(ladders_qgis, paste0(headDir,"/", 
                              "8.Yield_Data/25/Processed/MRS125_Seg_Dist10.0_VarWidth_poly3.shp"))

