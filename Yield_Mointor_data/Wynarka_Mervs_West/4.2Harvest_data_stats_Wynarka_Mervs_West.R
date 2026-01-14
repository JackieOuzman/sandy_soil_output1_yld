## Stirling Roberton
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
library(readr)


################################################################################
########################            Define the directory              ##########
################################################################################

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
site_number <- "3.Wynarka_Mervs_West"
site_name <- "Wynarka_Mervs_West"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.type <- "Harvest"


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

harvest_data_file <-  readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "Harvest_data") %>% 
  filter(Site == site_number)


################################################################################

## get the list of all the file options
harvest_files_all_options <- list.files(path = paste0(headDir, "/", harvest_data_file$files))
harvest_files_all_options

## select which file I will use
harvest_files <- list.files(path = paste0(headDir, "/", harvest_data_file$files), pattern = ".shp")
harvest_files

# Load the file 
harvest_raw <- st_read(
  paste0(headDir,"/", harvest_data_file$files,harvest_files))
# harvest_raw_df <- read_csv(
#   paste0(headDir,"/", harvest_data_file$files,harvest_files))
# harvest_raw_df

# harvest_raw <- st_as_sf(harvest_raw_df, 
#                   coords = c("Longitude", "Latitude"),
#                   crs = 4326)  # WGS84 (standard lat/lon)


plot(harvest_raw)

#############
## work out which clm to use 

harvest_raw
names(harvest_raw)

harvest_raw %>%
  select(`VRYIELDMAS`, `WetMass`, `DRYMATT`) %>%
  st_drop_geometry() %>%
  summary()

#Looks like ... use VRYIELDMAS 


#############
## work out projection

# Check current CRS (Coordinate Reference System)
st_crs(harvest_raw)
# ===== FIX THE CRS METADATA =====
# Your shapefile has WGS84 but with malformed metadata
# Reset it to proper WGS84

harvest_raw <- st_set_crs(harvest_raw, 4326)
# Alternative: GDA2020 / MGA Zone 54 (Australian standard for the area)
harvest_raw_mga <- st_transform(harvest_raw, 7854)


################################################################################

## Read in data
boundary   <- st_read(file.path(headDir, file_path_details$boundary))
strips <-     st_read(file.path(headDir, file_path_details$trial.plan))
strips <- st_make_valid(strips) #Checks whether a geometry is valid, or makes an invalid geometry valid

#data.raw <- st_read(paste0(headDir,'/7.In_Season_data/24/4.Biomass/Biomass_NDVI_Walpeup_2024_merged_data.gpkg'))

zones <- rast(paste0(file.path(headDir, file_path_details$`location of zone tif`)))
zones <- terra::project(zones,paste0('epsg:',crs_used),method='near')


#################################################################################
data.raw <- harvest_raw
strips <- st_make_valid(strips)
strips <- strips %>%
  mutate(treat_desc = ifelse(treat_desc == "Control (-Tillage -Lime)", "Control", treat_desc))





################################################################################
## Step 1) Define variables
## Define variable in data to analyse
variable <- "VRYIELDMAS"

## Define treatment column name in strips
treat.col.name <- "treat_desc"

## Define name of control
control.name <- "Control"

## Define name of Buffer (if applicable)
buffer.name <- "Buffer"

clean.dat <- "Yes"

model <- "XGBoost" #"Random Forest

###############################################################################
## Step 2) Clean observation data (if applicable)

## Step 2.1) Clip to variable of interest and crop to trial area
data.crop <- st_crop(data.raw[,variable],strips)
names(data.crop)[1] <- "target.variable"

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

###############################################################################
## Step 3) Drill treatments and zones
treat.drilled <- st_intersection(data.clean,strips[,treat.col.name])
zones.drilled <- terra::extract(zones,treat.drilled)
names(zones.drilled)[2] <- "zone"

all.dat <- na.omit(cbind(treat.drilled,zones.drilled[-1]))

all.dat <- all.dat %>%
  filter(!(treat_desc %in% c(buffer.name, "Outside Control")))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%% General Stats - Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
## Step 4) Compute summary statistics for whole of field

df <- st_drop_geometry(all.dat)

control_group <- df %>% filter(!!sym(treat.col.name) == control.name) # Assuming Treat_Num 1 is the control group

summary_stats <- df %>%
  group_by(!!sym(treat.col.name)) %>%
  summarize(
    mean = mean(target.variable, na.rm = TRUE),
    sd = sd(target.variable, na.rm = TRUE),
    min = min(target.variable, na.rm = TRUE),
    max = max(target.variable, na.rm = TRUE),
    median = median(target.variable, na.rm = TRUE),
    target.variable = n()
  )

t_test_results <- df %>%
  filter(!!sym(treat.col.name) != control.name) %>% # Exclude the control group
  group_by(!!sym(treat.col.name)) %>%
  do(tidy(t.test(target.variable ~ !!sym(treat.col.name), data = rbind(control_group, .)))) %>%
  ungroup() %>%
  mutate(adj_p_value = p.adjust(p.value, method = "bonferroni"),
         significance = ifelse(adj_p_value <= 0.1, "Significant", "Not Significant"))

print(t_test_results)
print(summary_stats)

# Perform ANOVA
anova <- aov(as.formula(paste("target.variable", "~", treat.col.name)), data = df)
summary(anova)

# Tukey HSD post-hoc test
tukey <- TukeyHSD(anova)
tukey_results <- as.data.frame(tukey[treat.col.name])

# Get the significance letters from Tukey HSD results
letters <- multcompLetters4(anova, tukey)

# Convert to a dataframe
sig.out <- data.frame(
  treat = names(letters[[treat.col.name]]$Letters), 
  Significance = letters[[treat.col.name]]$Letters
)
names(sig.out)[1] <- treat.col.name

summary_stats.2 <- inner_join(summary_stats,sig.out, by  = treat.col.name)
print(summary_stats.2)




write.csv(summary_stats.2,paste0(headDir,'/10.Analysis/25/',analysis.type,'/summary-stats-whole-pdk.csv'))

################################################################################
## Step 5) Make a ggplot

# Compute summary statistics (median, 25th, and 75th percentiles)
summary_stats <- df %>%
  group_by(!!sym(treat.col.name)) %>%
  summarise(
    median = median(target.variable, na.rm = TRUE),
    Q1 = quantile(target.variable, 0.25, na.rm = TRUE),
    Q3 = quantile(target.variable, 0.75, na.rm = TRUE)
  )

# Create the bar plot
site.bar.plot <- ggplot(summary_stats, aes(x = !!sym(treat.col.name), y = median, fill = !!sym(treat.col.name))) +
  geom_col(alpha = 0.7) +  # Bars for median values
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black") +  # Quartile error bars
  labs(
    title = "Crop Yield by Treatment",
    x = "Treatment Description",
    y = "Yield (t/ha)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 16),          # Increase axis label font size
    axis.text = element_text(size = 16),           # Axis text size (already set above, just making explicit)
    plot.title = element_text(size = 16, hjust = 0.5),  # Center and enlarge title
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Print the plot
site.bar.plot


ggsave(paste0(headDir,"/10.Analysis/25/",analysis.type,"/summary-plot-whole-pdk.png"), site.bar.plot)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%% By Zone Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
## Step 6) Compute summary statistics by zone

unique(all.dat$zone)

for(i in 1:length(unique(all.dat$zone))){
  df <- all.dat %>%
    filter(zone == i)

  control_group <- df %>% filter(!!sym(treat.col.name) == control.name) # Assuming Treat_Num 1 is the control group

  summary_stats <- df %>%
    group_by(!!sym(treat.col.name)) %>%
    summarize(
      mean = mean(target.variable, na.rm = TRUE),
      sd = sd(target.variable, na.rm = TRUE),
      min = min(target.variable, na.rm = TRUE),
      max = max(target.variable, na.rm = TRUE),
      median = median(target.variable, na.rm = TRUE)
    )

  t_test_results <- df %>%
    filter(!!sym(treat.col.name) != control.name) %>% # Exclude the control group
    group_by(!!sym(treat.col.name)) %>%
    do(tidy(t.test(target.variable ~ !!sym(treat.col.name), data = rbind(control_group, .)))) %>%
    ungroup() %>%
    mutate(adj_p_value = p.adjust(p.value, method = "bonferroni"),
           significance = ifelse(adj_p_value <= 0.1, "Significant", "Not Significant"))

  # Perform ANOVA
  anova <- aov(as.formula(paste("target.variable", "~", treat.col.name)), data = df)
  summary(anova)

  # Tukey HSD post-hoc test
  tukey <- TukeyHSD(anova)
  tukey_results <- as.data.frame(tukey[treat.col.name])

  # Get the significance letters from Tukey HSD results
  letters <- multcompLetters4(anova, tukey)

  # Convert to a dataframe
  sig.out <- data.frame(
    treat = names(letters[[treat.col.name]]$Letters),
    Significance = letters[[treat.col.name]]$Letters
  )
  names(sig.out)[1] <- treat.col.name

  summary_stats.2 <- st_drop_geometry(inner_join(summary_stats,sig.out, by  = treat.col.name))
  print(summary_stats.2)
}



################################################################################
## Step 7) Plot by Zone
df <- all.dat
df$zone <- as.factor(df$zone)
df <- na.omit(df)
nrow(df)

summary_stats <- df %>%
  group_by(!!sym(treat.col.name), zone) %>%
  summarise(
    median = median(target.variable, na.rm = TRUE),
    Q1 = quantile(target.variable, 0.25, na.rm = TRUE),
    Q3 = quantile(target.variable, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# zone_desc <- c("1" = "Transition", "2" = "Dune","3" = "Swale")  # others keep their ID
# zone_labeller <- ggplot2::labeller(
#   zone_id = function(z) {
#     zc <- as.character(z)
#     desc <- zone_desc[zc]
#     ifelse(is.na(desc), zc, paste0(zc, " — ", desc))
#   }
# )



zone.bar.plot <- summary_stats %>%
  dplyr::rename(zone_id = zone) %>%
  ggplot(aes(x = !!sym(treat.col.name), y = median, fill = !!sym(treat.col.name))) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black") +
  labs(
    title = "Crop Yield by Treatment and Zone",
    x = NULL,
    y = "Yield (t/ha)",
    fill = "Treatment"
  ) +
  #facet_wrap(~ zone_id, scales = "free_x", labeller = zone_labeller) +
  facet_wrap(~ zone_id, scales = "free_x") +
  theme_minimal() +
  theme(
    text = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 18, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    
    # legend at bottom (matching your NDVI plot)
    legend.position        = "bottom",
    legend.box             = "vertical",
    legend.justification   = "center"
  )+
  guides(
    fill = guide_legend(title.position = "top", title.hjust = 0.5)
  )



zone.bar.plot

ggsave(paste0(headDir,"/10.Analysis/25/",analysis.type,"/summary-plot-zones-pdk.png"), zone.bar.plot)

























