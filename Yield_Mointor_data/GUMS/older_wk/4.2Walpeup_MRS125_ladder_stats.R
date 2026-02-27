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
library(terra)
library(tidyterra)

library(broom)


################################################################################
########################            Define the directory              ##########
################################################################################

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
site_number <- "1.Walpeup_MRS125"
site_name <- "Walpeup_MRS125"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.type <- "Harvest"

# subfolder1 <- "testing_cleaning_trimming_etc"
# subfolder2 <- "4.ladder_polygons"


analysis.yr <- "25"


metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

crs_used <- 4326  ## EPSG:4326 is WGS84 (World Geodetic System 1984) - a geographic coordinate system (not a projected coordinate system).
#GDA2020 / MGA Zone 54 (EPSG:7854) Most common for this area
#Walpeup is in MGA Zone 54 UTM-based, suitable for local/regional work
#st_transform(your_data, crs = 7854)


################################################################################
########################    Read in metadata info file names and path ##########
################################################################################
# List all sheet names
sheet_names_metadata <- excel_sheets( paste0(metadata_path,metadata_file_name))
print(sheet_names_metadata)

# zone_shapefile_path <- readxl::read_excel(
#   paste0(metadata_path,metadata_file_name),
#   sheet = "location of file and details") %>% 
#   filter(Site == site_number) %>% 
#   select(`location of zone shp`) %>% 
#   pull()


################################################################################
# Load the files 

yld_ladders <- read_csv(
  paste0(
    headDir,
    "/8.Yield_Data/25/Processed/",
    "Yld_data_av_to_ladder.csv"
  )
)

df<-yld_ladders

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%% General Stats - Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
################################################################################


## Step 4) Compute summary statistics for whole of field
### rename the df so the scripts will run

unique(df$treat_desc)
names(df)
str(df)

df <-  dplyr::rename(df, target.variable = mean_yld  )
str(df)



summary_stats <- df %>%
  group_by(treat) %>%
  summarize(
    mean = mean(target.variable, na.rm = TRUE),
    sd = sd(target.variable, na.rm = TRUE),
    min = min(target.variable, na.rm = TRUE),
    max = max(target.variable, na.rm = TRUE),
    median = median(target.variable, na.rm = TRUE),
    Q1 = quantile(target.variable, 0.25, na.rm = TRUE),
    Q3 = quantile(target.variable, 0.75, na.rm = TRUE),
    n = n()  # Move this to the end and rename it
  )

summary_stats



# Perform ANOVA
anova <- aov(as.formula(paste("target.variable", "~", "treat")), data = df)
summary(anova)

# Tukey HSD post-hoc test
tukey <- TukeyHSD(anova)
tukey_results <- as.data.frame(tukey["treat"])

# Get the significance letters from Tukey HSD results
letters <- multcompLetters4(anova, tukey)

# Convert to a dataframe
# Convert to a dataframe - CORRECTED
sig.out <- data.frame(
  treat = names(letters$treat$Letters),  # Add $ before treat
  Significance = letters$treat$Letters   # Add $ before treat
)

sig.out

#names(sig.out)[1] <- treat.col.name

summary_stats.2 <- inner_join(summary_stats,sig.out)
print(summary_stats.2)

## add the detailed version of the treatments names
str(df)
list_treatments <- df %>% 
  dplyr::distinct(treat, .keep_all = TRUE) %>% 
  select(treat, treat_id, treat_desc)




summary_stats.2 <- left_join(summary_stats.2, list_treatments)

write.csv(summary_stats.2,
          paste0(headDir,
                 '/10.Analysis/25/Processing_Jackie/Stats_Yld_monitor',
                  '/Yld_strips_stats.csv'))

################################################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%% By Zone Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
##  Compute summary statistics by zone

str(df)
df <- df %>% rename(zone=  mean_zone)





# Get unique gridcodes
zone <- unique(df$zone)

# Store results in lists
anova_list <- list()
tukey_list <- list()
sig_letters_list <- list()

for(gc in zone) {
  # Subset data for this gridcode
  df_subset <- df %>% filter(zone == gc)
  
  # Perform ANOVA
  anova_model <- aov(target.variable ~ treat, data = df_subset)
  
  cat("\n=== ANOVA for Gridcode", gc, "===\n")
  print(summary(anova_model))
  
  # Tukey HSD
  tukey <- TukeyHSD(anova_model)
  cat("\n=== Tukey HSD for Gridcode", gc, "===\n")
  print(tukey)
  
  # Get significance letters
  letters_obj <- multcompLetters4(anova_model, tukey)
  
  # Store ANOVA results
  anova_list[[as.character(gc)]] <- data.frame(
    gridcode = gc,
    tidy(anova_model)
  )
  
  # Store Tukey results
  tukey_list[[as.character(gc)]] <- data.frame(
    gridcode = gc,
    comparison = rownames(tukey$treat),
    tukey$treat,
    row.names = NULL
  )
  
  # Store significance letters
  sig_letters_list[[as.character(gc)]] <- data.frame(
    gridcode = gc,
    treat = names(letters_obj$treat$Letters),
    Significance = as.character(letters_obj$treat$Letters)
  )
}

# Combine all results into data frames
anova_results <- bind_rows(anova_list)
tukey_results <- bind_rows(tukey_list)
sig_letters_all <- bind_rows(sig_letters_list)

# View combined results
print("ANOVA Results:")
print(anova_results)

print("\nTukey HSD Results:")
print(tukey_results)

print("\nSignificance Letters:")
print(sig_letters_all)
sig_letters_all <- sig_letters_all %>% rename(zone = gridcode )

####################################################################
# Create summary statistics by gridcode and treatment
summary_stats_zone <- df %>%
  st_drop_geometry() %>% 
  group_by(zone, treat, treat_desc) %>%
  summarise(
    mean = mean(target.variable, na.rm = TRUE),
    median = median(target.variable, na.rm = TRUE),
    sd = sd(target.variable, na.rm = TRUE),
    Q1 = quantile(target.variable, 0.25, na.rm = TRUE),
    Q3 = quantile(target.variable, 0.75, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Join with significance letters
summary_stats.2 <- summary_stats_zone %>%
    inner_join(sig_letters_all, by = c("zone", "treat"))

print(summary_stats.2)



summary_stats.2 <- left_join(summary_stats.2, list_treatments)

write.csv(summary_stats.2,
          paste0(headDir,
                 '/10.Analysis/25/Processing_Jackie/Stats_Yld_monitor',
                 '/Yld_strips_zones_stats.csv'))






























