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

crs_used <- 7850
################################################################################
########################    Read in metadata info file names and path ##########
################################################################################



seasons <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "seasons") %>% 
  filter(Site == site_number)


# Load the file 
Yld_ladders_df <- read_csv(
  paste0(headDir, "/8.Yield_Data/Processed/",
         "Yld_data_av_to_ladder.csv"))

# Define control label
Yld_ladders_df <- Yld_ladders_df %>%
  dplyr::mutate(treat_desc = case_when(
    treat_desc == "Control (-Tillage -Lime)" ~ "Control",
    .default = as.character(treat_desc)))

# Split by year
Yld_ladders_2020 <- Yld_ladders_df %>% filter(year == 2020)
Yld_ladders_2021 <- Yld_ladders_df %>% filter(year == 2021)
Yld_ladders_2022 <- Yld_ladders_df %>% filter(year == 2022)
Yld_ladders_2023 <- Yld_ladders_df %>% filter(year == 2023)


# Function
harvest_summary_stats <- function(dat, yr) {
  #dat <- Yld_ladders_2020 # for testing /debugging
  
  ## Step 1) Define variables
  variable <- "mean_yld"
  control.name <- "Control"
  
  ## Step 4) Compute summary statistics
  df <- dat %>%
    rename(target.variable = mean_yld)
  
  summary_stats <- df %>%
    group_by(treat) %>%
    summarize(
      mean       = mean(target.variable, na.rm = TRUE),
      sd         = sd(target.variable, na.rm = TRUE),
      min        = min(target.variable, na.rm = TRUE),
      max        = max(target.variable, na.rm = TRUE),
      median     = median(target.variable, na.rm = TRUE),
      Q1         = quantile(target.variable, 0.25, na.rm = TRUE),
      Q3         = quantile(target.variable, 0.75, na.rm = TRUE),
      n_total    = n(),
      n_valid    = sum(!is.na(target.variable)),
      n_na       = sum(is.na(target.variable))
    )
  
  control_group <- df %>% filter(treat == "C")
  
  t_test_results <- df %>%
    filter(treat != "C") %>%
    group_by(treat) %>%
    do(tidy(t.test(target.variable ~ treat,
                   data = rbind(control_group, .)))) %>%
    ungroup() %>%
    mutate(
      adj_p_value  = p.adjust(p.value, method = "bonferroni"),
      significance = ifelse(adj_p_value <= 0.1, "Significant", "Not Significant")
    )
  
  letters_display <- t_test_results %>%
    select(treat, significance) %>%
    mutate(group = ifelse(significance == "Significant", "b", "a")) %>%
    bind_rows(data.frame(treat = "C", significance = "Control", group = "a"))
  
  results_table <- summary_stats %>%
    left_join(letters_display %>% select(treat, group), by = "treat") %>%
    left_join(t_test_results %>% select(treat, adj_p_value, significance), by = "treat") %>%
    mutate(
      adj_p_value = ifelse(is.na(adj_p_value), "-", round(adj_p_value, 3)),
      group       = ifelse(is.na(group), "a", group)
    ) %>%
    rename(Significance = group) %>%
    select(treat, n_valid, mean, sd, median, Q1, Q3, min, max, Significance, adj_p_value) %>%
    mutate(analysis.type = paste0(variable, "_Yr_", yr))
  
  print(results_table)
  
  write.csv(results_table,
            paste0(headDir, '/10.Analysis/', analysis.type,
                   '/Harvest_summary_strips_stats_', yr, '.csv'),
            row.names = FALSE)
}

###############################################################################
# Run for each year
harvest_summary_stats(Yld_ladders_2020, 2020)
harvest_summary_stats(Yld_ladders_2021, 2021)
harvest_summary_stats(Yld_ladders_2022, 2022)
harvest_summary_stats(Yld_ladders_2023, 2023)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%% By Zone Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################

# Do this once after splitting if target.variable isn't already in the data
Yld_ladders_2020 <- Yld_ladders_2020 %>% rename(target.variable = mean_yld)
Yld_ladders_2021 <- Yld_ladders_2021 %>% rename(target.variable = mean_yld)
Yld_ladders_2022 <- Yld_ladders_2022 %>% rename(target.variable = mean_yld)
Yld_ladders_2023 <- Yld_ladders_2023 %>% rename(target.variable = mean_yld)

## Something is wrong with 2023
unique(Yld_ladders_2023$mean_zone)
Yld_ladders_2023


Yld_ladders_2023 %>%
  st_drop_geometry() %>%
  group_by(mean_zone, treat) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(mean_zone, treat) %>%
  print(n = Inf)  # show all rows


#   zone treat     n
#      1 C         6
#      1 PP        3
#      2 C        13
#      2 PP        1
#      3 C        40
#      3 PP       78
#      4 C        47
#      4 PP       22

# Function
harvest_zone_stats <- function(dat, yr) {
  
  dat <- Yld_ladders_2023 # for testing /debugging
  
  df <- dat %>% rename(zone = mean_zone)
  zone <- unique(df$zone)
  zone <- c(1,3,4) # we dont have much zone 2 in strip 1 and 4 are slim too
  
  ttest_list       <- list()
  sig_letters_list <- list()
  
  for(gc in zone) {
    df_subset     <- df %>% filter(zone == gc)
    control_group <- df_subset %>% filter(treat == "C")
    
    ttest_df <- df_subset %>%
      filter(treat != "C") %>%
      group_by(treat) %>%
      do(tidy(t.test(
        c(control_group$target.variable, .$target.variable) ~
          c(rep("C", nrow(control_group)), rep(.$treat[1], nrow(.)))
      ))) %>%
      ungroup() %>%
      mutate(
        adj_p_value = p.adjust(p.value, method = "bonferroni"),
        zone  = gc,
        treat2 = "C"
      )
    
    cat("\n=== Zone", gc, "| Year", yr, "===\n")
    print(ttest_df %>% select(treat, adj_p_value))
    
    ttest_list[[as.character(gc)]] <- ttest_df
    
    sig_letters <- ttest_df %>%
      select(treat, adj_p_value) %>%
      mutate(group = ifelse(adj_p_value <= 0.1, "b", "a")) %>%
      bind_rows(data.frame(treat = "C", adj_p_value = NA, group = "a")) %>%
      mutate(zone = gc)
    
    sig_letters_list[[as.character(gc)]] <- data.frame(
      zone         = gc,
      treat        = sig_letters$treat,
      Significance = sig_letters$group
    )
  }
  
  ttest_results  <- bind_rows(ttest_list)
  sig_letters_all <- bind_rows(sig_letters_list)
  
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
  
  summary_stats.2 <- summary_stats_zone %>%
    inner_join(sig_letters_all, by = c("zone", "treat")) %>%
    mutate(year = yr)
  
  print(summary_stats.2)
  
  write.csv(summary_stats.2,
            paste0(headDir, '/10.Analysis/', 'Harvest/',
                   'Harvest_summary_strips_zones_stats_', yr, '.csv'),
            row.names = FALSE)
}

###############################################################################
# Run for each year
# (assumes Yld_ladders_20xx already split and df/target.variable rename done upstream)
harvest_zone_stats(Yld_ladders_2020, 2020)
harvest_zone_stats(Yld_ladders_2021, 2021)
harvest_zone_stats(Yld_ladders_2022, 2022)
harvest_zone_stats(Yld_ladders_2023, 2023)


