
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
library(rstatix)

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

# site_number <- "5.Walpeup_Gums"
# site_name <- "Walpeup_Gums"

#site_number <- "6.Crystal_Brook_Randals"
#site_name <- "Crystal_Brook_Randals"




dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.yr <- "25"


metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"


################################################################################
########################    Read in metadata info file names and path ##########
################################################################################

# file_path_details <- readxl::read_excel(
#   paste0(metadata_path,metadata_file_name),
#   sheet = "location of file and details") %>% 
#   filter(Site == site_number)

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
#variable <- "Biomass_flowering" #This is sometimes called biomass, or biomass at flowering 4.Peak_Biomass
#variable <- "Biomass_maturity" # Maturity_biomass
#variable <- "Grain yield" # 
#variable <- "Thousand grain weight" # 
variable <- "Harvest index" # 
#variable <- "Protein"


str(merged_pt_sampling)

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
# new_names_temp <- df %>% 
#   distinct(treat, .keep_all = TRUE) %>% 
#   dplyr::select(treat, treat_id, treat_desc)
# df <- df %>%
#   rename(treat_original = treat) %>%
#   mutate(treat = gsub("-", "_", treat_original))


str(df)

## At Mervs they moved the control strip but never collected any samples in new control, so its NA
unique(df$treat)
df <- df %>% dplyr::mutate(
  treat = case_when(
    is.na(treat) ~ "C",
    .default = treat
  ))
unique(df$treat)

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
str(df)
unique(df$treat)



control_group <- df %>% filter(treat == "C")
### Note at GU only one sample was collected in B strip - so it removed from analysis
#df <- df %>% dplyr::filter(treat != "B")


t_test_results <- df %>%
  filter(treat != "C") %>%
  group_by(treat) %>%
  do(tidy(t.test(target.variable ~ treat, 
                 data = rbind(control_group, .)))) %>%
  ungroup() %>%
  mutate(
    adj_p_value = p.adjust(p.value, method = "bonferroni"),
    significance = ifelse(adj_p_value <= 0.1, "Significant", "Not Significant") 
  )#90% CI

# Build letter display
letters_display <- t_test_results %>%
  select(treat, significance) %>%
  mutate(group = ifelse(significance == "Significant", "b", "a")) %>%
  # Add control back in - it always gets "a" as the reference
  bind_rows(data.frame(treat = "C", significance = "Control", group = "a"))


letters_display

results_table <- summary_stats %>%
  left_join(letters_display %>% select(treat, group), by = "treat") %>%
  left_join(t_test_results %>% select(treat, adj_p_value, significance), by = "treat") %>%
  mutate(
    adj_p_value = ifelse(is.na(adj_p_value), "-", round(adj_p_value, 3)),
    group = ifelse(is.na(group), "a", group)
  ) %>%
  rename(Significance = group) %>%
  select(treat, n_valid, mean, sd, median, Q1, Q3, min, max, Significance, adj_p_value)

results_table

results_table <- results_table %>% 
mutate(analysis.type = paste0(variable,"_Yr_", analysis.yr ))



### If you needed to modify the treat names replacing - with _ change it back

# results_table <- results_table %>%
#   mutate(treat = gsub("_", "-", treat))


################################################################################
###############            Write to file               #########################

write.csv(results_table, 
          paste0(headDir,'/10.Analysis/',analysis.yr,'/Processing_Jackie/Stats_pt_sampling/',
                 variable,'_summary_stats_strip_t_test.csv'),
          row.names = FALSE)






