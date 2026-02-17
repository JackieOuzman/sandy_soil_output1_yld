
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
library(emmeans)

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


metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"


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


treatment_names <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "treatment names") %>% 
  filter(Site == site_number)

################################################################################
## Read in field point data 
merged_pt_sampling <- read_csv(paste0(headDir,"/10.Analysis/25/Processing_Jackie/merged_pt_sampling/", 
                 "plant_sample_merged_2025.csv"))
merged_pt_sampling <- merged_pt_sampling %>% 
  mutate(date_field_observation = as.Date(as.numeric(date_field_observation), origin = "1899-12-30"))








################################################################################
## Step 1) Define variables
## Define variable in data to analyse
merged_pt_sampling %>% distinct(field_observation)

#variable <- "Establishment" #
#variable <- "Establishment CV" #
variable <- "Biomass_flowering" #This is sometimes called biomass, or biomass at flowering 4.Peak_Biomass
#variable <- "Biomass_maturity" # Maturity_biomass
#variable <- "Grain yield" # 
#variable <- "Thousand grain weight" # 
#variable <- "Harvest index" # 
#variable <- "Protein"


str(merged_pt_sampling)

## Define treatment column name in strips
treat.col.name <- "treat_desc"


control.name <- treatment_names %>% 
  filter(`True control` == "Control") %>% 
  filter(!is.na(`Treatment name`)) %>%
  pull(`Shorthand names`)


## Define name of Buffer (if applicable)
buffer.name <- "Buffer"

df <- merged_pt_sampling

###############################################################################

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%% General Stats - Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
## Step 4) Compute summary statistics for whole of field



df <- df %>% 
  filter(field_observation == variable) %>% 
  filter(!is.na(target.variable))
### The analysis code doesn't like "-" so here we are replacing with "_"
names(df)
new_names_temp <- df %>% 
  distinct(treat, .keep_all = TRUE) %>% 
  dplyr::select(treat, treat_id, treat_desc)
df <- df %>%
  rename(treat_original = treat) %>%
  mutate(treat = gsub("-", "_", treat_original))


str(df)
summary_stats <- df %>%
  filter(field_observation == variable ) %>% 
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
summary_stats


# #Make sure control_group is defined first
# control_group <- df %>%
#   filter(field_observation == variable) %>%
#   filter(treat == control.name)




# Perform ANOVA
anova <- aov(target.variable ~ treat, data = df)
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


# Convert to a dataframe
sig.out <- data.frame(
  treat_desc = names(letter_element$treat$Letters),
  Significance = as.character(letter_element$treat$Letters),
  row.names = NULL
)
sig.out
summary_stats.2 <- inner_join(summary_stats,sig.out, join_by(treat == treat_desc)) %>% 
  mutate(analysis.type = paste0(variable,"_Yr_", analysis.yr ))
print(summary_stats.2)

### If you needed to modify the treat names replacing - with _ change it back

summary_stats.2 <- summary_stats.2 %>%
  mutate(treat = gsub("_", "-", treat))


################################################################################
###############            Write to file               #########################

write.csv(summary_stats.2, 
          paste0(headDir,'/10.Analysis/',analysis.yr,'/Processing_Jackie/Stats_pt_sampling/',
                 variable,'_summary_stats_strip.csv'),
          row.names = FALSE)

rm(summary_stats.2, sig.out, anova, tukey, tukey_results)





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%% By Zone Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Step 6) Compute summary statistics by zone

str(df)
#df <- df %>% rename(zone=  cluster3 )

### The analysis code doesn't like "-" so here we are replacing with "_"
# names(df)
# new_names_temp <- df %>% 
#   distinct(treat, .keep_all = TRUE) %>% 
#   select(treat, treat_id, treat_desc)
# df <- df %>%
#   rename(treat_original = treat) %>%
#   mutate(treat = gsub("-", "_", treat_original))




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

# summary_stats.2 <- summary_stats.2 %>%
#   mutate(treat = gsub("_", "-", treat))

write.csv(summary_stats.2, 
          paste0(headDir,'/10.Analysis/',analysis.yr,'/Processing_Jackie/Stats_pt_sampling/',
                 variable,'_summary_stats_strip_zones.csv'),
          row.names = FALSE)


