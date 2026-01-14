
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
site_number <- "1.Walpeup_MRS125"
site_name <- "Walpeup_MRS125"
headDir <- paste0(dir, "/work/Output-1/", site_number)

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

plant_path_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "Planet data") %>% 
  filter(Site == site_number)

################################################################################
## Read in field data with the Sential and drone data too


collated_data_all <- st_read(
  paste0(
    headDir,
    "/10.Analysis/25/Processing_Jackie/",
    site_name,
    "_collated_data_raw_Sen_drone_planet",
    analysis.yr,
    ".shp"
  )
)

collated_data_all_df <- read_csv(
  paste0(
    headDir,
    "/10.Analysis/25/Processing_Jackie/",
    site_name,
    "_collated_data_raw_sentinel_drone_planet",
    analysis.yr,
    ".csv"
  )
)


#### Clean the data? I have left this out as I only have 72 observations!

################################################################################
## Step 1) Define variables
## Define variable in data to analyse


names(collated_data_all_df)
unique(collated_data_all_df$fld_ob)

variable <- "Emergence" 

## Define treatment column name in strips
treat.col.name <- "trt_dsc"

## Define name of control
unique(collated_data_all_df$trt_dsc)
control.name <- "Lime Control (3t)" # was "Control"

## Define name of Buffer (if applicable)
buffer.name <- "Buffer"



###############################################################################

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%% General Stats - Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
## Step 4) Compute summary statistics for whole of field
#df <- st_drop_geometry(all.dat)
df <- collated_data_all_df

control_group <- df %>% filter(!!sym(treat.col.name) == control.name) # Assuming Treat_Num 1 is the control group
names(df)


summary_stats <- df %>%
  filter(fld_ob == variable ) %>% 
  group_by(trt_dsc) %>% 
  summarize(
    mean = mean(trgt_vr, na.rm = TRUE),
    sd = sd(trgt_vr, na.rm = TRUE),
    min = min(trgt_vr, na.rm = TRUE),
    max = max(trgt_vr, na.rm = TRUE),
    median = median(trgt_vr, na.rm = TRUE),
    n_total = n(),                      # Total number of rows
    n_valid = sum(!is.na(trgt_vr)),     # Count of non-NA values
    n_na = sum(is.na(trgt_vr))  
  )
summary_stats

#Make sure control_group is defined first
control_group <- df %>%
  filter(fld_ob == variable) %>%
  filter(!!sym(treat.col.name) == control.name)

# Fixed t-test code
t_test_results <- df %>%
  filter(fld_ob == variable) %>%
  filter(!!sym(treat.col.name) != control.name) %>% # Exclude the control group
  group_by(!!sym(treat.col.name)) %>%
  do({
    treatment_data <- .
    
    # Combine control and treatment data
    combined_data <- bind_rows(
      control_group %>% select(trgt_vr, all_of(treat.col.name)),
      treatment_data %>% select(trgt_vr, all_of(treat.col.name))
    )
    
    # Perform t-test
    test_result <- t.test(trgt_vr ~ !!sym(treat.col.name), data = combined_data)
    
    # Return tidy results
    tidy(test_result)
  }) %>%
  ungroup() %>%
  mutate(adj_p_value = p.adjust(p.value, method = "bonferroni"),
         significance = ifelse(adj_p_value <= 0.1, "Significant", "Not Significant"))


print(t_test_results)
print(summary_stats)

# Perform ANOVA
anova <- aov(as.formula(paste("trgt_vr", "~", treat.col.name)), data = df)
summary(anova)

# Tukey HSD post-hoc test
# Perform Tukey HSD post-hoc test
tukey <- TukeyHSD(anova)

# View Tukey results
cat("\n\nTukey HSD Results:\n")
print(tukey)

# Extract compact letter display
letter_element <- multcompLetters4(anova, tukey)
# its is a naming problem 