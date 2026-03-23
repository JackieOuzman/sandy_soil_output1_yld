
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

#site_number <- "3.Wynarka_Mervs_West"
#site_name <- "Wynarka_Mervs_West"

 
site_number <- "98.Auxillary_Sites/11.Qualeup_Spanners"
site_name <- "Auxillary_Sites_11.Qualeup_Spanners"

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.type <- "Harvest"
#analysis.yr <- "25"


metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

headDir_analysis_folder <- paste0(headDir, "/10.Analysis/Harvest/")
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
  select("treat" ,
         "Shorthand Name"  ,
         "Treatment Name" ,
         "Order in Paddock" ,
         "Hex" )

# zone_shapefile_path <- readxl::read_excel(
#   paste0(metadata_path,metadata_file_name),
#   sheet = "file location etc") %>% 
#   filter(Site == site_number) %>% 
#   filter(variable == "location of zone shp") %>% 
#   pull("file path")
# strips_shapefile_path <- readxl::read_excel(
#   paste0(metadata_path,metadata_file_name),
#   sheet = "file location etc") %>% 
#   filter(Site == site_number) %>% 
#   filter(variable == "trial.plan") %>% 
#   pull("file path")


zone_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "zone_details") %>% 
  filter(Site == site_number) %>% 
  select(`zone names`, `zone label names`)
zone_details

################################################################################
### List of files 
# strips_merged_stats <- list.files(path = headDir_analysis_folder, 
#                                   pattern = "_strips_stats\\.csv$", 
#                                   full.names = FALSE) # replace this with false is you just want to see the names
# 
# strips_zones_merged_stats <- list.files(path = headDir_analysis_folder, 
#                                         pattern = "_strips_zones_stats\\.csv$", 
#                                         full.names = FALSE) # 


strips_merged_stats <- list.files(
  path = headDir_analysis_folder,
  pattern = "Harvest_summary_strips_stats_\\d{4}\\.csv$",
  full.names = TRUE
)

strips_zones_merged_stats <- list.files(
  path = headDir_analysis_folder,
  pattern = "Harvest_summary_strips_zones_stats_\\d{4}\\.csv$",
  full.names = TRUE
)

print(strips_merged_stats)
print(strips_zones_merged_stats)

##################################################################################
### Load the files
# strips_merged_stats_df <- read_csv(paste0(headDir_analysis_folder,
#                                           strips_merged_stats
# ))
# strips_zones_merged_stats_df <- read_csv(paste0(headDir_analysis_folder,
#                                                 strips_zones_merged_stats
# ))


strips_merged_stats_df <- list.files(
  path = headDir_analysis_folder,
  pattern = "Harvest_summary_strips_stats_\\d{4}\\.csv$",
  full.names = TRUE
) %>%
  lapply(read.csv) %>%
  bind_rows()

strips_zones_merged_stats_df <- list.files(
  path = headDir_analysis_folder,
  pattern = "Harvest_summary_strips_zones_stats_\\d{4}\\.csv$",
  full.names = TRUE
) %>%
  lapply(read.csv) %>%
  bind_rows()

################################################################################
#append the metadata file 1
strips_merged_stats_df <- left_join(strips_merged_stats_df, strip_names_details)
names(strips_merged_stats_df)

# Reorder treat based on Order_in_paddock
strips_merged_stats_df <- strips_merged_stats_df %>%
  dplyr::mutate(treat = reorder(treat, `Order in Paddock`),
         `Treatment Name` = reorder(`Treatment Name`, `Order in Paddock`))
str(strips_merged_stats_df)


###############################################################################
#append the metadata file 2
strips_zones_merged_stats_df <- left_join(strips_zones_merged_stats_df, strip_names_details)
names(strips_zones_merged_stats_df)

# Reorder treat based on Order_in_paddock
strips_zones_merged_stats_df <- strips_zones_merged_stats_df %>%
  mutate(treat = reorder(treat, `Order in Paddock`),
         `Treatment Name` = reorder(`Treatment Name`, `Order in Paddock`))
str(strips_zones_merged_stats_df)
zone_details
strips_zones_merged_stats_df$zone <- as.character(strips_zones_merged_stats_df$zone)
strips_zones_merged_stats_df <- left_join(strips_zones_merged_stats_df, 
                                          zone_details,
                                          join_by(zone == `zone names`)) 
strips_zones_merged_stats_df <- strips_zones_merged_stats_df %>% 
  rename(`zone_label` = `zone label names`)

##################################################################################

head(strips_merged_stats_df$analysis.type)

strips_merged_stats_df <- strips_merged_stats_df %>%
  mutate(year = as.integer(stringr::str_extract(analysis.type, "\\d{4}")))


site.bar.plot <-
  strips_merged_stats_df %>%
  mutate(treat = reorder(treat, `Order in Paddock`)) %>%
  ggplot(aes(x = treat, y = mean, fill = `Treatment Name`)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black") +
  geom_text(
    aes(label = Significance, y = Q3),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  labs(
    title = "",
    caption = "Treatment compared to the control using t-tests with Bonferroni-adjusted p-values, (p ≤ 0.10)",
    x = NULL,
    y = paste0("Yield. t/ha", "\n", "Header"),
    fill = NULL
  ) +
  scale_fill_manual(values = setNames(strips_merged_stats_df$Hex,
                                      strips_merged_stats_df$`Treatment Name`)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.caption = element_text(size = 10),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)  # <-- closing bracket here
  ) +
  facet_wrap(~ year)



# Print the plot
site.bar.plot

ggsave(paste0(headDir_analysis_folder,
              'Yld_monitor_strip_plot.png'), site.bar.plot)




################################################################################
## Step 7) Plot by Zone

strips_zones_merged_stats_df
strips_zones_merged_stats_df <- strips_zones_merged_stats_df %>% mutate(year = as.factor(year))
table(strips_zones_merged_stats_df$year)

site.bar.plot <-
  strips_zones_merged_stats_df %>%
  mutate(treat = reorder(treat, `Order in Paddock`)) %>%
  ggplot(aes(x = treat, y = mean, fill = `Treatment Name`)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black") +
  geom_text(
    aes(label = Significance, y = Q3),
    vjust = -0.8,      # increase to push letters higher above Q3
    size = 3,          # smaller text fits better in compact panels
    fontface = "bold"
  ) +
  facet_grid(year ~ paste0(zone_label, " (", zone, ")")) +
  labs(
    title = "",
    caption = "Treatment compared to the control using t-tests with Bonferroni-adjusted p-values, (p ≤ 0.10)",
    x = NULL,
    y = paste0("Yield. t/ha", "\n", "Header"),
    fill = NULL
  ) +
  scale_fill_manual(values = setNames(strips_zones_merged_stats_df$Hex,
                                      strips_zones_merged_stats_df$`Treatment Name`)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.30))) +  # more top padding
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8),          # smaller for all axis text
    axis.text.y = element_text(size = 8),        # y axis numbers smaller still
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)         # facet label size
  )


# Print the plot
site.bar.plot




ggsave(paste0(headDir_analysis_folder,
              'Yld_monitor_strip_zone_plot.png'), site.bar.plot)



#