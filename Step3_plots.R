
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
library(tidyverse)

################################################################################
########################            Define the directory              ##########
################################################################################

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
site_number <- "1.Walpeup_MRS125"
site_name <- "Walpeup_MRS125"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.type <- "Emergence" #Harvest, InSeason, PeakBiomass, PreSeason
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


#### Bring in the data 
summary_stats.2 <- read_csv(paste0(headDir,'/10.Analysis/',analysis.yr,'/',analysis.type,'/summary-stats-whole-pdk.csv'))




################################################################################
## Step 5) Make a ggplot


# Create the bar plot
summary_stats.2

site.bar.plot <-summary_stats.2 %>% 
  ggplot(aes(x = treat.col.name, y = median, fill = treat.col.name)) +
  geom_col(alpha = 0.7)+
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black")+
  labs(
    title = "Plant Count by Treatment",
    x = "Treatment Description",
    y = "Plant Density (plants/m2)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    axis.title = element_text(size = 18),          # Increase axis label font size
    axis.text = element_text(size = 16),           # Axis text size (already set above, just making explicit)
    plot.title = element_text(size = 22, hjust = 0.5),  # Center and enlarge title
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
site.bar.plot


#ggsave(paste0(headDir,'/10.Analysis/',analysis.yr,'/',analysis.type,'/Emergence/emergence-plot-whole-pdk.png'),site.bar.plot)
