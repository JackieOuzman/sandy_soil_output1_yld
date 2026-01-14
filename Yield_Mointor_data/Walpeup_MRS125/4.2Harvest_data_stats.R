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

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"

headDir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/1.Walpeup_MRS125"

analysis.type <- "Harvest" #Emergence, InSeason, PeakBiomass, PreSeason

## Read in data
boundary <- st_read(paste0(headDir,'/1.Paddock_Boundary/Walpeup_MRS125_Boundary_Masked_4326.shp'))
strips <- st_read(paste0(headDir,"/5.Trial_Plan/FINAL-Trial-Plan/GIS/MRS125_Strips_FINAL_wgs84.shp"))
#data.raw <- st_read(paste0(headDir,'/7.In_Season_data/24/4.Biomass/Biomass_NDVI_Walpeup_2024_merged_data.gpkg'))

zones <- rast(paste0(headDir,'/3.Covariates/6.Clusters_Zones/FINAL/MRS125_NEWZONES_round_wgs84.tif'))
zones <- terra::project(zones,'epsg:4326',method='near')

# zones.2 <- rast(paste0(headDir,'/3.Covariates/6.Clusters_Zones/FINAL/MRS125_Zones_round_wgs84.tif'))
# zones <- zones.2

data.raw.4326 <- st_read(paste0(headDir,"/8.Yield_Data/25/Raw/Export_20251209_1025_mrs125_25yield/doc/Lowan Ridge_Lowan Ridge_MRS 125new_Harvest_2025-12-07_00.shp"))
data.raw <- data.raw.4326

strips <- st_make_valid(strips)
strips <- strips %>%
  mutate(treat_desc = ifelse(treat_desc == "Control (-Tillage -Lime)", "Control", treat_desc))

zone_desc <- c("1" = "Transition", "2" = "Dune","3" = "Swale")  # others keep their ID
zone_labeller <- ggplot2::labeller(
  zone_id = function(z) {
    zc <- as.character(z)
    desc <- zone_desc[zc]
    ifelse(is.na(desc), zc, paste0(zc, " — ", desc))
  }
)

seasons <- tribble(
  ~year, ~crop_type, ~plant_date,   ~harvest_date,
  2024, "Lentils",  "29/05/2024",  NA_character_,
  2025, "Wheat",    "27/04/2025",  NA_character_,
  2026, NA,         NA_character_, NA_character_,
  2027, NA,         NA_character_, NA_character_
) %>%
  mutate(
    year         = as.integer(year),
    plant_date   = dmy(plant_date),
    harvest_date = dmy(harvest_date)
  )

site.info <- list(
  site_id    = "Walpeup_MRS125",
  boundary   = boundary,      # sf
  trial_plan = strips,    # sf
  seasons    = seasons        # tibble
)
class(site.info) <- c("ssii_site", class(site.info))

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

#ggsave(paste0(headDir,"/10.Analysis/25/Emergence/emergence-plot-whole-pdk.png"),site.bar.plot)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%% By Zone Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
## Step 6) Compute summary statistics by zone

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
  facet_wrap(~ zone_id, scales = "free_x", labeller = zone_labeller) +
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



























