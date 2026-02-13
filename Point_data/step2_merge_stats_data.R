################################################################################
### Once all the variables have been analysed I want to make a complie the dataset 

library(terra)
library(sf)
library(ggplot2)
library(multcomp)
library(dplyr)
library(tidyr)
library(broom)
library(multcompView)
library(lubridate)
library(readr)
library(emmeans)
################################################################################
########################            Define the directory              ##########
################################################################################
# site_number <- "1.Walpeup_MRS125"
# site_name <- "Walpeup_MRS125"

site_number <-"2.Crystal_Brook_Brians_House" 
site_name <-  "Crystal_Brook_Brians_House"

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)


analysis.yr <- "25"
analysis_folder <- "/10.Analysis/25/Processing_Jackie/Stats_pt_sampling/"

metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

headDir_analysis_folder <- paste0(headDir,analysis_folder )

################################################################################
### List of files 
csv_files_strips <- list.files(path = headDir_analysis_folder, 
                        pattern = "_summary_stats_strip\\.csv$", 
                        full.names = FALSE) # replace this with false is you just want to see the names


csv_files_strip_zones <- list.files(path = headDir_analysis_folder, 
                               pattern = "_summary_stats_strip_zones\\.csv$", 
                               full.names = FALSE)

print(csv_files_strips)
print(csv_files_strip_zones)


# Read all CSV files into a list
csv_list_strips <- lapply(paste0(headDir_analysis_folder, "/", csv_files_strips), read.csv)
csv_list_strips_zones <- lapply(paste0(headDir_analysis_folder, "/", csv_files_strip_zones), read.csv)
# Merge all dataframes into ONE file
merged_strips <- do.call(rbind, csv_list_strips)
merged_strips_zones <- do.call(rbind, csv_list_strips_zones)

rm(csv_files_strips, csv_files_strip_zones,
   csv_list_strips, csv_list_strips_zones)
####################################################################################

write.csv(merged_strips, 
          paste0(headDir_analysis_folder,"merged_strips_stats.csv"), 
          row.names = FALSE)
write.csv(merged_strips_zones, 
          paste0(headDir_analysis_folder,"merged_strips_zones_stats.csv"), 
          row.names = FALSE)
          