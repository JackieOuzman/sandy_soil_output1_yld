
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
summary_stats_whole <- read_csv(paste0(headDir,'/10.Analysis/',analysis.yr,'/',analysis.type,'/summary-stats-whole-pdk.csv'))
#### Bring in the data 
summary_stats_zone <- read_csv(paste0(headDir,'/10.Analysis/',analysis.yr,'/',analysis.type,'/summary-stats-zone.csv'))

################################################################################

### This needs to be adjusted in order of predicted response?

order <- c(
  "Control",                 ###***
  "Control (+Lime)",
  "Control (-Tillage -Lime)..",
  "Control..",
  
  "Lime Control (3t)..",     ###***
  "Lime Control (3t)",       ###***
  
  "Active Inclusion..",
  "Bednar",
  "Bednar + Delve",
  "Chicken Litter",
  "Deep Rip..",
  "Deep_Ripping",
  "Horsch",
  "Inlcusion_Ripping",
  "Kuhn Performer..",
  
 
  
  "Passive Inclusion..",
  "Plozza_Plough",
  "Rip",                     ###***
  "Rip..",                   ###***
  
  "Spade..",                 ###***
  "Spade",                   ###***
  
  "Rip (+Lime)",             ###***
  "Rip + Lime (3t)",         ###***
  "Rip + Lime (3t)..",
  
  
  "Spade + Rip..",           ###***
  "Spade + Rip",             ###***
  "Rip + Spade",
  
  
  "Rip + Delve",
  
  "Rip + Mix (+Lime) + FUTURE",
  "Rip + Mix + FUTURE",
  
  "Rip + PasInclusion",
  "Rip + PasInclusion + Chicken Litter",
  "Rip + PasInclusion + Chicken Litter +Dry",
  
  
  "Rip+Mix",
  "Rip+Mix (+Lime)",
  "Rotary_Spading",
  
  "Spade + Delve",
  "Spade + Lime (3t)..",
  "Spade + Lime (3t)",
 
  "Spade + Rip + Lime (3t)..",
  "Spade + Rip + Lime (3t)",
  
  
  "TopDown")

order_df <- as.data.frame(order)
order_df <- order_df %>%  dplyr::rename(treat.col.name = order)
## add a oder index to this order_df.

order_df$order_rank <- 1:nrow(order_df)  



#order the treatments by appending the order data frame to the summary df
names(order_df)
names(summary_stats_whole)
summary_stats_whole <- left_join(summary_stats_whole, order_df, by = join_by(treat.col.name == treat.col.name))
summary_stats_zone <- left_join(summary_stats_zone, order_df, by = join_by(treat.col.name == treat.col.name))

################################################################################
## Step 5) Make a ggplot


# Create the bar plot
summary_stats_whole
analysis.type


summary_stats_whole_plot <-summary_stats_whole %>% 
  mutate(treat.col.name = fct_reorder(treat.col.name, order_rank)) %>%
  ggplot(aes(x = treat.col.name, y = median, fill = treat.col.name)) +
  geom_col(alpha = 0.7)+
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black")+
  labs(
    title = paste0( analysis.type," by Treatment"),
    caption = paste0("Site: ", site_name, ". Year: 20", analysis.yr),
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    axis.title = element_text(size = 18),          # Increase axis label font size
    #axis.text = element_text(size = 16),           # Axis text size (already set above, just making explicit)
    plot.title = element_text(size = 22),  # Center and enlarge title
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
summary_stats_whole_plot

savedfilename <- paste0("summary_", analysis.type," plot-whole-pdk.png")

ggsave(paste0(headDir,"/10.Analysis/",analysis.yr,'/',analysis.type,"/"
              ,savedfilename),
              summary_stats_whole_plot)


################################################################################
### Do the zones have names?
zone_names_temp <- file_path_details$`zone names`
zone_labels_temp <- file_path_details$`zone label names`

zone_names_label_temp <- data.frame(zone = as.double(strsplit(zone_names_temp, ",")[[1]]),
                   zone_label = as.character(strsplit(zone_labels_temp, ",")[[1]])
                   )

zone_names_label_temp

## if so add them to the df

summary_stats_zone <- left_join( summary_stats_zone, zone_names_label_temp )
str(summary_stats_zone)

summary_stats_zone_plot <-summary_stats_zone %>% 
  mutate(treat.col.name = fct_reorder(treat.col.name, order_rank)) %>%
  ggplot(aes(x = treat.col.name, y = median, fill = treat.col.name)) +
  geom_col(alpha = 0.7)+
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black")+
  labs(
    title = paste0( analysis.type," by Treatment"),
    caption = paste0("Site: ", site_name, ". Year: 20", analysis.yr),
    x = "",
    y = ""
  ) +
  facet_wrap(.~zone_label)+
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    axis.title = element_text(size = 18),          # Increase axis label font size
    #axis.text = element_text(size = 16),           # Axis text size (already set above, just making explicit)
    plot.title = element_text(size = 22),  # Center and enlarge title
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

summary_stats_zone_plot

savedfilename_zone <- paste0("summary_", analysis.type," plot-zone.png")

ggsave(paste0(headDir,"/10.Analysis/",analysis.yr,'/',analysis.type,"/"
              ,savedfilename_zone),
       summary_stats_zone_plot)
# 
# ggsave(paste0(headDir,'/10.Analysis/',analysis.yr,'/',analysis.type,'/Emergence/emergence-by-zone.png'),zone.bar.plot)



