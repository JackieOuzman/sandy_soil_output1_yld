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

site_number1 <- "1.Walpeup_MRS125"
site_number2 <-"2.Crystal_Brook_Brians_House"
site_number3 <- "3.Wynarka_Mervs_West"
site_number4 <- "4.Wharminda"
site_number5 <- "5.Walpeup_Gums"
site_number6 <- "6.Crystal_Brook_Randals"

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir1 <- paste0(dir, "/work/Output-1/", site_number1)
headDir2 <- paste0(dir, "/work/Output-1/", site_number2)
headDir3 <- paste0(dir, "/work/Output-1/", site_number3)
headDir4 <- paste0(dir, "/work/Output-1/", site_number4)
headDir5 <- paste0(dir, "/work/Output-1/", site_number5)
headDir6 <- paste0(dir, "/work/Output-1/", site_number6)



################################################################################
## Read in field point data 
site1 <- read_csv(paste0(headDir1,"/10.Analysis/25/Processing_Jackie/merged_pt_sampling/", 
                                      "plant_sample_merged_2025.csv"))
site2 <- read_csv(paste0(headDir2,"/10.Analysis/25/Processing_Jackie/merged_pt_sampling/", 
                         "plant_sample_merged_2025.csv"))
site3 <- read_csv(paste0(headDir3,"/10.Analysis/25/Processing_Jackie/merged_pt_sampling/", 
                         "plant_sample_merged_2025.csv"))
site4 <- read_csv(paste0(headDir4,"/10.Analysis/25/Processing_Jackie/merged_pt_sampling/", 
                         "plant_sample_merged_2025.csv"))
site5 <- read_csv(paste0(headDir5,"/10.Analysis/25/Processing_Jackie/merged_pt_sampling/", 
                         "plant_sample_merged_2025.csv"))
site6 <- read_csv(paste0(headDir6,"/10.Analysis/25/Processing_Jackie/merged_pt_sampling/", 
                         "plant_sample_merged_2025.csv"))

names(site1)
names(site2)
names(site3)
names(site4)
names(site5)
names(site6)

# Standardize column names and add missing columns before binding

# Fix Id/id inconsistency -> standardize to "id"
site1 <- site1 %>% dplyr::rename(id = Id)

# Drop fid_1 from site3
site3 <- site3 %>% dplyr::select(-fid_1)

# Add missing columns as NA placeholders
site1 <- site1 %>% dplyr::mutate(strip_ha = NA_real_)
site6 <- site6 %>% dplyr::mutate(plot = NA_real_)

site1 <- site1 %>% mutate(date_field_observation = as.character(date_field_observation))
site2 <- site2 %>% mutate(date_field_observation = as.character(date_field_observation))
site3 <- site3 %>% mutate(date_field_observation = as.character(date_field_observation))
site4 <- site4 %>% mutate(date_field_observation = as.character(date_field_observation))
site5 <- site5 %>% mutate(date_field_observation = as.character(date_field_observation))
site6 <- site6 %>% mutate(date_field_observation = as.character(date_field_observation))

# Now bind
all_sites <- bind_rows(site1, site2, site3, site4, site5, site6)


str(all_sites)

all_sites <- all_sites %>% 
  mutate(date_field_observation = as.Date(as.numeric(date_field_observation), origin = "1899-12-30"))
###############################################################################
#tidy up 
rm(site1,site2,site3,site4,site5,site6)
rm(headDir1,headDir2,headDir3,headDir4,headDir5,headDir6)
###############################################################################
analysis.yr <- "25"
metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"
###############################################################################

seasons <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "seasons") %>% 
  filter(Year == 2025)
seasons <- seasons %>%
  mutate(site = gsub("^[0-9]+\\.", "", Site))

str(all_sites)
str(seasons)

all_sites <- all_sites %>% left_join(seasons)


###############################################################################
distinct(all_sites, field_observation)
distinct(all_sites, site)
distinct(all_sites, Crop)

all_sites <- all_sites %>%
  mutate(field_observation = factor(field_observation, levels = c(
    "Establishment",
    "Establishment CV",
    "Biomass_flowering",
    "Biomass_maturity",
    "Grain yield",
    "Harvest index",
    "Thousand grain weight",
    "Protein"
  )))

# Create a new column combining field_observation and variable_units
all_sites <- all_sites %>%
  mutate(facet_label = paste0(field_observation, " (", variable_units, ")"))

# Preserve the factor order from field_observation
all_sites <- all_sites %>%
  mutate(facet_label = factor(facet_label, levels = unique(facet_label[order(field_observation)])))


all_sites <- all_sites %>%
  mutate(site_short = case_when(
    site == "Walpeup_MRS125"             ~ "MRS125",
    site == "Crystal_Brook_Brians_House" ~ "BH",
    site == "Wynarka_Mervs_West"         ~ "Merves",
    site == "Wharminda"                  ~ "Wharminda",
    site == "Walpeup_Gums"              ~ "Gums",
    site == "Crystal_Brook_Randals"      ~ "Randals",
    TRUE ~ site
  )) %>%
  mutate(site_short = factor(site_short, levels = c(
    "MRS125",
    "BH",
    "Merves",
    "Wharminda",
    "Gums",
    "Randals"
  )))
names(all_sites)


label_df <- all_sites %>%
  group_by(site_short, facet_label) %>%
  summarise(
    label = paste(unique(format(na.omit(date_field_observation), "%d-%b")), collapse = "\n"),
    y_pos = max(target.variable, na.rm = TRUE)
  ) %>%
  mutate(label = ifelse(label == "", "TBC", label)) %>%
  ungroup()

label_df
distinct(label_df, facet_label)

# Plot
ggplot(all_sites, aes(x = site_short, y = target.variable, colour = Crop)) +
  geom_point() +
  facet_wrap(. ~ facet_label, scales = "free_y") +
  geom_text(data = label_df, aes(x = site_short, y = y_pos, label = label), 
            vjust = -0.3, hjust = 0.5, size = 2.5, colour = "black", angle = 90) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = NULL, y = NULL)



bounds_df <- data.frame(
  facet_label = c("Establishment (plants/m2)", "Establishment (plants/m2)",
                  "Biomass_flowering (kg/ha)", "Biomass_flowering (kg/ha)",
                  "Biomass_maturity (kg/ha)", "Biomass_maturity (kg/ha)",
                  "Grain yield (kg/ha)", "Grain yield (kg/ha)",
                  "Harvest index (%)", "Harvest index (%)",
                  "Thousand grain weight (g/1000 grains)", "Thousand grain weight (g/1000 grains)",
                  "Protein (%)", "Protein (%)"),
  bound = c(100, 400,
            5000, 15000,
            8000, 20000,
            500, 6000,
            35, 55,
            25, 55,
            8, 16),
  bound_type = c("lower", "upper", "lower", "upper", "lower", "upper", "lower", "upper",
                 "lower", "upper", "lower", "upper", "lower", "upper")
)

facet_order <- c("Establishment (plants/m2)",
                 "Establishment CV (%)",
                 "Biomass_flowering (kg/ha)",
                 "Biomass_maturity (kg/ha)",
                 "Grain yield (kg/ha)",
                 "Harvest index (%)",
                 "Thousand grain weight (g/1000 grains)",
                 "Protein (%)")

bounds_df$facet_label <- factor(bounds_df$facet_label, levels = facet_order)
all_sites$facet_label <- factor(all_sites$facet_label, levels = facet_order)

ggplot(all_sites, aes(x = site_short, y = target.variable, colour = Crop)) +
  geom_point() +
  geom_hline(data = bounds_df, aes(yintercept = bound, linetype = bound_type), 
             colour = "red", linewidth = 0.5) +
  scale_linetype_manual(values = c("lower" = "dashed", "upper" = "dashed")) +
  facet_wrap(. ~ facet_label, scales = "free_y") +
  geom_text(data = label_df, aes(x = site_short, y = y_pos, label = label), 
            vjust = -0.3, hjust = 0.5, size = 2.5, colour = "black", angle = 90) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(linetype = "none") +
  labs(x = NULL, y = NULL,
       title = "Quality control 2026 point sampling output 1 2026",
       subtitle = "BH was often weedy, broadcast sowing at some sites",
       caption = "Check data BH harvest data to be included, and cals at Randals")



################################################################################
### collate the files source used for this anlysis.

