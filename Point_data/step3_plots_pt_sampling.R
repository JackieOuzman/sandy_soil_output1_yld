################################################################################
### Once all the variables have been analysed and mereged I can make the plots 


library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(multcompView)
library(lubridate)
library(readr)
library(readxl)

# library(terra)
# library(sf)
# library(emmeans)
# library(multcomp)


################################################################################
########################            Define the directory              ##########
################################################################################

# site_number <- "1.Walpeup_MRS125"
# site_name <- "Walpeup_MRS125"

# site_number <-"2.Crystal_Brook_Brians_House" 
# site_name <-  "Crystal_Brook_Brians_House"

# site_number <- "3.Wynarka_Mervs_West"
# site_name <- "Wynarka_Mervs_West"

site_number <- "4.Wharminda"
site_name <- "Wharminda"

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)


analysis.yr <- "25"
analysis_folder <- "/10.Analysis/25/Processing_Jackie/Stats_pt_sampling/"

metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"



headDir_analysis_folder <- paste0(headDir,analysis_folder )
headDir_analysis_folder

################################################################################
### Load the metadata files

# List all sheet names
sheet_names_metadata <- excel_sheets( paste0(metadata_path,metadata_file_name))
print(sheet_names_metadata)

strip_names_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "treatment names") %>% 
  filter(Site == site_number)

names(strip_names_details)
strip_names_details <- strip_names_details %>% 
  dplyr::select("treat" ,
         "Shorthand names" ,
         "Treatment name",
         "Order_in_paddock" ,
         "Colour_number_plot")

Pt_sampling_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "location of observation data") %>% 
  filter(Site == site_number)

names(Pt_sampling_details)
Pt_sampling_details <- Pt_sampling_details %>% 
  dplyr::select("variable_name" ,
         "Date_collected" ,
         "varibale_units") %>% 
  mutate(variable_name = paste0(variable_name, "_Yr_", analysis.yr))
Pt_sampling_details
################################################################################
### List of files 
strips_merged_stats <- list.files(path = headDir_analysis_folder, 
                               pattern = "_strips_stats\\.csv$", 
                               full.names = FALSE) # replace this with false is you just want to see the names

strips_zones_merged_stats <- list.files(path = headDir_analysis_folder, 
                                  pattern = "_strips_zones_stats\\.csv$", 
                                  full.names = FALSE) # 

print(strips_merged_stats)
print(strips_zones_merged_stats)

##################################################################################
### Load the files
strips_merged_stats_df <- read_csv(paste0(headDir_analysis_folder,
                                          strips_merged_stats
                                 ))
strips_zones_merged_stats_df <- read_csv(paste0(headDir_analysis_folder,
                                                strips_zones_merged_stats
))

################################################################################
#append the metadata
strips_merged_stats_df <- left_join(strips_merged_stats_df, strip_names_details)
names(strips_merged_stats_df)
names(Pt_sampling_details)
strips_merged_stats_df <- left_join(strips_merged_stats_df,
                                      Pt_sampling_details,
                                      join_by("analysis.type"  == "variable_name")) 



##################################################################################

#variable <- "Establishment" #
#variable <- "Establishment CV" #
variable <- "Biomass_flowering" #This is sometimes called biomass, or biomass at flowering 4.Peak_Biomass
#variable <- "Biomass_maturity" # Maturity_biomass
#variable <- "Grain yield" # 
#variable <- "Thousand grain weight" # 
#variable <- "Harvest index" # 
#variable <- "Protein"


# Create the bar plot from the strip analysis data
str(strips_merged_stats_df) #(from above)
names(strips_merged_stats_df)


# Reorder treat based on Order_in_paddock
strips_merged_stats_df <- strips_merged_stats_df %>%
  mutate(treat = reorder(treat, Order_in_paddock),
  `Treatment name` = reorder(`Treatment name`, Order_in_paddock))
str(strips_merged_stats_df)

# get the units of the variable
units_variable <- strips_merged_stats_df %>% 
  filter(analysis.type == paste0(variable, "_Yr_", analysis.yr)) %>%
  pull(varibale_units) %>% 
  unique()

site.bar.plot <-
  strips_merged_stats_df %>%
  filter(analysis.type == paste0(variable, "_Yr_", analysis.yr)) %>%
  mutate(treat = reorder(treat, Order_in_paddock)) %>%
  ggplot(aes(x = treat, y = mean, fill = `Treatment name`)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black") +
  geom_text(
    aes(label = Significance, y = Q3),  # Position at Q3
    vjust = -0.5,  # Move above Q3
    size = 5,
    fontface = "bold"
  ) +
  labs(
    title = "",
    caption = "One-way ANOVA followed by Tukey's HSD post-hoc test at 95% confidence",
    x = NULL,
    y = paste0(variable, "\n", units_variable),  # \n creates line break
    fill = NULL
  ) +
  scale_fill_manual(values = setNames(strips_merged_stats_df$Colour_number_plot, 
                                      strips_merged_stats_df$`Treatment name`)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.caption = element_text(size = 10),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Print the plot
site.bar.plot


headDir_analysis_folder

ggsave(paste0(headDir_analysis_folder,"plots/",
              variable,
              '_strip_mean_ANOVA_plot.png'), site.bar.plot)

