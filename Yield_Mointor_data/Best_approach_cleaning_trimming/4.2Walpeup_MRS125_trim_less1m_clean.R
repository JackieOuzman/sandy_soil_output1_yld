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

subfolder1 <- "testing_cleaning_trimming_etc"
subfolder2 <- "clean_trim_per_strip"

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

harvest_files_options2 <- list.files(path =
                     paste0(headDir, "/", 
                            "10.Analysis/",
                            analysis.yr,"/",
                            analysis.type,"/",
                            subfolder1,"/",
                            subfolder2,"/"
                            ), 
                   pattern = ".xls")
harvest_file_path <- paste0(headDir, "/", 
                             "10.Analysis/",
                             analysis.yr,"/",
                             analysis.type,"/",
                             subfolder1,"/",
                             subfolder2,"/")

harvest_files_options2
harvest_file_path


# Load the file 





harvest_raw <- st_read(paste0(harvest_file_path, harvest_files_options2)) 
 
str(harvest_raw)

#############
## work out which clm to use 

harvest_raw
names(harvest_raw)

harvest_raw %>%
  select(VRYIELDMAS, WetMass, Moisture, DRYMATTER, treat_id) %>% 
  summary()

#Looks like VRYIELDMAS, WetMass are almost the same... use VRYIELDMAS





# Drop geometry and convert to data frame
harvest_raw_df <- harvest_raw
##subset the data
names(harvest_raw_df)
harvest_raw_df <- harvest_raw_df %>% 
  select(VRYIELDMAS, "treat"  ,    "treat_desc" ,"gridcode", "treat_id", "POINT_X", "POINT_Y")
names(harvest_raw_df)

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


## cleaning and trimming

rm(harvest_raw)
str(harvest_raw_df)

harvest_raw_df <- harvest_raw_df %>% rename("target.variable" = VRYIELDMAS)

harvest_raw_df <- harvest_raw_df %>% filter(target.variable > 0)


# Calculate bounds and filter for each treatment
data.clean <- harvest_raw_df %>%
  #group_by(treat_id) %>% if you want the buffered seperate
  group_by(treat_desc) %>%
  mutate(
    Q1 = quantile(target.variable, 0.25),
    Q3 = quantile(target.variable, 0.75),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 2 * IQR,
    upper_bound = Q3 + 2 * IQR
  ) %>%
  filter(target.variable >= lower_bound & target.variable <= upper_bound) %>%
  select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound) %>%
  ungroup()

data.clean
treat_id_treat_names <- data.clean %>% distinct(treat_id, treat_desc)
data.clean <- left_join(data.clean, treat_id_treat_names)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%% General Stats - Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
## Step 4) Compute summary statistics for whole of field

df <- data.clean
unique(harvest_raw_df$treat_desc)
unique(harvest_raw_df$treat)
names(df)
str(df)

#df <-  dplyr::rename(df, target.variable = VRYIELDMAS )
str(df)



summary_stats <- df %>%
  group_by(treat) %>%
  summarize(
    mean = mean(target.variable, na.rm = TRUE),
    sd = sd(target.variable, na.rm = TRUE),
    min = min(target.variable, na.rm = TRUE),
    max = max(target.variable, na.rm = TRUE),
    median = median(target.variable, na.rm = TRUE),
    target.variable = n()
  )





# Create control group
control_group <- df %>%
  filter(treat == "C")

# Run t-tests
t_test_results <- df %>%
  filter(treat != "C") %>%
  group_by(treat, treat_desc) %>%
  do({
    treatment_data <- .$target.variable
    control_data <- control_group$target.variable
    tidy(t.test(treatment_data, control_data))
  }) %>%
  ungroup() %>%
  mutate(
    adj_p_value = p.adjust(p.value, method = "bonferroni"),
    significance = ifelse(adj_p_value <= 0.1, "Significant", "Not Significant")
  )




print(t_test_results)
print(summary_stats)

# the names return an error


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
list_treatments <- df %>% dplyr::distinct(treat, .keep_all = TRUE) %>% select(-target.variable,-gridcode )


summary_stats.2 <- left_join(summary_stats.2, list_treatments)


write.csv(summary_stats.2,
          paste0(headDir,'/10.Analysis/25/',analysis.type,
                 "/",subfolder1,
                 "/",subfolder2,
                 '/summary-stats-whole-pdk.csv'))

write.csv(df,
          paste0(headDir,'/10.Analysis/25/',analysis.type,
                 "/",subfolder1,
                 "/",subfolder2,
                 '/clean_trim.csv'))

################################################################################
## Step 5) Make a ggplot

# Compute summary statistics (median, 25th, and 75th percentiles)
#mean, instead of median
str(df)

summary_stats <- df %>%
  #group_by( treat_id) %>% # if you want the buffers sep
  group_by( treat_desc) %>%
  summarise(
    mean = mean(target.variable, na.rm = TRUE),
    Q1 = quantile(target.variable, 0.25, na.rm = TRUE),
    Q3 = quantile(target.variable, 0.75, na.rm = TRUE)
  )

summary_stats
summary_stats.2
summary_stats <- left_join(summary_stats, summary_stats.2)

# Create the bar plot
summary_stats

site.bar.plot <- ggplot(summary_stats, aes(x = treat_desc, y = mean, fill = treat_desc)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black") +
  geom_text(aes(label = Significance, y = Q3),   # Add significance letters
            vjust = -0.5,                         # Position above error bars
            size = 6,                             # Text size
            fontface = "bold") +                  # Make bold
  labs(
    title = "Crop Yield by Treatment",
    #subtitle = "No cleaning or trimming",
    x = NULL,
    y = "Yield (t/ha)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Print the plot
site.bar.plot




ggsave(paste0(headDir,'/10.Analysis/25/',analysis.type,
                 "/",subfolder1,
                 "/",subfolder2,
                 '/summary-plot-whole-pdk.png'), site.bar.plot)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%% By Zone Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
## Step 6) Compute summary statistics by zone
control_group <- df %>%
  filter(treat == "C")
str(control_group)
control_group <- control_group %>% rename(zone=  gridcode)


str(df)
df <- df %>% rename(zone=  gridcode)


# Run t-tests comparing each treatment to control within each gridcode
t_test_results_zone <- df %>%
  filter(treat != "C") %>%
  group_by(zone , treat, treat_desc) %>%
  do({
    # Get control data for this zone
    control_data <- filter(control_group, zone == unique(.$zone))$target.variable
    # Get treatment data for this zone
    treatment_data <- .$target.variable
    
    # Run t-test if both groups have data
    if(length(control_data) > 0 && length(treatment_data) > 0) {
      tidy(t.test(treatment_data, control_data))
    } else {
      data.frame(estimate = NA, p.value = NA, statistic = NA)
    }
  }) %>%
  ungroup() %>%
  mutate(
    adj_p_value = p.adjust(p.value, method = "bonferroni"),
    significance = ifelse(adj_p_value <= 0.1, "Significant", "Not Significant")
  )

print(t_test_results)


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





################################################################################
## Step 7) Plot by Zone




str(summary_stats.2)
summary_stats.2 <- summary_stats.2 %>% 
  mutate(zone_label = case_when(
    zone ==1 ~ "1.Swale",
    zone ==2 ~ "2.Dune",
    zone ==3 ~ "3.Transition",
  ))

#### Up to here need to add the letters and fix up the names of treatments

zone.bar.plot_zone <- summary_stats.2 %>%
  ggplot(aes(x = treat_desc, y = mean, fill = treat_desc)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black") +
  geom_text(aes(label = Significance, y = Q3),   # Add significance letters
            vjust = -0.5,
            size = 6,
            fontface = "bold") +
  labs(
    title = "Crop Yield by Treatment and Zone",
    x = NULL,
    y = "Yield (t/ha)",
    fill = "Treatment"
  ) +
  facet_wrap(. ~ zone_label, scales = "free_x") +
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
    legend.position = "bottom",
    legend.box = "vertical",
    legend.justification = "center"
  ) +
  guides(
    fill = guide_legend(title.position = "top", title.hjust = 0.5)
  )

zone.bar.plot_zone


ggsave(paste0(headDir,'/10.Analysis/25/',analysis.type,
              "/",subfolder1,
              "/",subfolder2,
              '/summary-plot-byzone.png'), zone.bar.plot_zone)





























