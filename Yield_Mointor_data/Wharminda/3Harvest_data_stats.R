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

# site_number <- "3.Wynarka_Mervs_West"
# site_name <- "Wynarka_Mervs_West"

site_number <- "4.Wharminda"
site_name <- "Wharminda"

# site_number <- "5.Walpeup_Gums"
# site_name <- "Walpeup_Gums"



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



seasons <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "seasons") %>% 
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


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%% General Stats - Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
## Step 4) Compute summary statistics for whole of field
df <- Yld_ladders_df %>% 
  rename(target.variable = mean_yld)
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
    n_total = n(),                      # Total number of rows
    n_valid = sum(!is.na(target.variable)),     # Count of non-NA values
    n_na = sum(is.na(target.variable))  
  )
summary_stats
str(df)
unique(df$treat)

control_group <- df %>% filter(treat == "C")


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



write.csv(results_table,paste0(headDir,'/10.Analysis/25/',analysis.type,
                                 '/Harvest_summary_strips_stats.csv'))

################################################################################

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%% By Zone Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################



## Compute summary statistics by zone
df <- df %>% rename(zone = mean_zone)
# Get unique gridcodes
zone <- unique(df$zone)
## at Merv the zone have 4 zone, but only 3 exist in the trial area
#zone <- c(1,2,4)

# Store results in lists
ttest_list <- list()
sig_letters_list <- list()



for(gc in zone) {
  df_subset <- df %>% filter(zone == gc)
  control_group <- df_subset %>% filter(treat == "C")
  
  # Control vs each treatment t-tests
  ttest_df <- df_subset %>%
    filter(treat != "C") %>%
    group_by(treat) %>%
    do(tidy(t.test(
      c(control_group$target.variable, .$target.variable) ~ 
        c(rep("C", nrow(control_group)), rep(.$treat[1], nrow(.))),
    ))) %>%
    ungroup() %>%
    mutate(
      adj_p_value = p.adjust(p.value, method = "bonferroni"),
      zone = gc,
      treat2 = "C"  # reference group
    )
  
  cat("\n=== Control vs Treatment t-tests for Zone", gc, "===\n")
  print(ttest_df %>% select(treat, adj_p_value))
  
  ttest_list[[as.character(gc)]] <- ttest_df
  
  # Build significance letters (relative to control)
  sig_letters <- ttest_df %>%
    select(treat, adj_p_value) %>%
    mutate(group = ifelse(adj_p_value <= 0.1, "b", "a")) %>%
    bind_rows(data.frame(treat = "C", adj_p_value = NA, group = "a")) %>%
    mutate(zone = gc)
  
  sig_letters_list[[as.character(gc)]] <- data.frame(
    zone = gc,
    treat = sig_letters$treat,
    Significance = sig_letters$group
  )
}

  



# Combine results
ttest_results <- bind_rows(ttest_list)
sig_letters_all <- bind_rows(sig_letters_list)

print("Pairwise t-test Results:")
print(ttest_results)
print("Significance Letters:")
print(sig_letters_all)


####################################################################
# Summary statistics by zone and treatment
summary_stats_zone <- df %>%
  st_drop_geometry() %>%
  group_by(zone, treat, treat_desc) %>%
  summarise(
    mean   = mean(target.variable, na.rm = TRUE),
    median = median(target.variable, na.rm = TRUE),
    sd     = sd(target.variable, na.rm = TRUE),
    Q1     = quantile(target.variable, 0.25, na.rm = TRUE),
    Q3     = quantile(target.variable, 0.75, na.rm = TRUE),
    n      = n(),
    .groups = "drop"
  )

# Join with significance letters
summary_stats.2 <- summary_stats_zone %>%
  inner_join(sig_letters_all, by = c("zone", "treat"))



summary_stats.2






write.csv(summary_stats.2, 
          paste0(headDir,'/10.Analysis/',analysis.yr,'/Harvest/',
                 'Harvest_summary_strips_zones_stats.csv'),
          row.names = FALSE)






























