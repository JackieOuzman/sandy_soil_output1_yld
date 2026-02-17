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
# site_number <- "1.Walpeup_MRS125"
# site_name <- "Walpeup_MRS125"

# site_number <-"2.Crystal_Brook_Brians_House" 
# site_name <-  "Crystal_Brook_Brians_House"

site_number <- "4.Wharminda"
site_name <- "Wharminda"

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.type <- "Harvest"

analysis.yr <- "25"


metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

crs_used <- 7854
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


###############################################################################

# Load the file 
Yld_ladders_df <- read_csv(
   paste0(headDir,"/8.Yield_Data/25/Processed/",
     "Yld_data_av_to_ladder.csv"))
Yld_ladders_df





################################################################################
## Step 1) Define variables
## Define variable in data to analyse
variable <- "mean_yld"

## Define treatment column name in strips
treat.col.name <- "treat"

## Define name of control
control.name <- "Control"

## Define name of Buffer (if applicable)
buffer.name <- "Buffer"


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%% General Stats - Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
## Step 4) Compute summary statistics for whole of field


str(Yld_ladders_df)

### std some names and clm names
df <- Yld_ladders_df %>% 
  rename(target.variable = mean_yld)

### The analysis code doesn't like "-" so here we are replacing with "_"

names(df)
#This is a temp df to look up names if needed
new_names_temp <- df %>% 
  distinct(treat, .keep_all = TRUE) %>% 
  select(treat, treat_id, treat_desc)
#making a new clm 
df <- df %>%
  rename(treat_original = treat) %>%
  mutate(treat = gsub("-", "_", treat_original))


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
    n_total = n(),                      # Total number of rows
    n_valid = sum(!is.na(target.variable)),     # Count of non-NA values
    n_na = sum(is.na(target.variable))  
  )


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

### If you needed to modify the treat names replacing - with _ change it back

summary_stats.2 <- summary_stats.2 %>%
  mutate(treat = gsub("_", "-", treat))


write.csv(summary_stats.2,paste0(headDir,'/10.Analysis/25/',analysis.type,
                                 '/Harvest_summary_strips_stats.csv'))

################################################################################

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%% By Zone Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
## Step 6) Compute summary statistics by zone
str(df)
###
df <- df %>% rename(zone = mean_zone)
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

### If you needed to modify the treat names replacing - with _ change it back

summary_stats.2 <- summary_stats.2 %>%
  mutate(treat = gsub("_", "-", treat))



write.csv(summary_stats.2, 
          paste0(headDir,'/10.Analysis/',analysis.yr,'/Harvest/',
                 'Harvest_summary_strips_zones_stats.csv'),
          row.names = FALSE)






























