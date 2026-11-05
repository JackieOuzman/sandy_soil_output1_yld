# 02_Presowing_soil_summary.R
# Reads the stacked depth-group file, summarises to zone means per depth group,
# and writes one CSV for the paddock reports (all sites).

library(tidyverse)

soil_dir    <- "H:/Output-1/0.Site-info/soil_N_H20_data/"
report_year <- 2026
n_low  <- 40    # kg N/ha: below this = Low
n_high <- 70    # kg N/ha: above this = High

grp <- read.csv(paste0(soil_dir, "All_sites_2024_2026_N_and_H2O_depth_groups.csv")) %>%
  filter(year == report_year)

# 1. zone means per depth group
by_depth <- grp %>%
  group_by(site_number, site, year, sampling_timing, zone, zone_description, depth_group) %>%
  summarise(soil_water_mm = mean(soil_water_mm, na.rm = TRUE),
            min_n_kg_ha   = mean(min_n_kg_ha,   na.rm = TRUE),
            .groups = "drop")

# 2. points sampled and sampling date per zone
points <- grp %>%
  group_by(site_number, zone) %>%
  summarise(points = n_distinct(sample_id),
            sampling_date = as.character(min(as.Date(sampling_date))),
            .groups = "drop")

# 3. whole-profile totals and N class (NA if a depth group is missing)
totals <- by_depth %>%
  group_by(site_number, zone) %>%
  summarise(n_depths    = n(),
            water_total = sum(soil_water_mm),
            N_total     = round(sum(min_n_kg_ha), 0),   # NEW: classify on the whole number shown in the report
            .groups = "drop") %>%
  mutate(water_total = ifelse(n_depths == 3, water_total, NA),
         N_total     = ifelse(n_depths == 3, N_total, NA),
         N_class = case_when(is.na(N_total)     ~ NA_character_,
                             N_total < n_low    ~ "Low",
                             N_total <= n_high  ~ "Medium",
                             TRUE               ~ "High")) %>%
  select(-n_depths)

out <- by_depth %>%
  left_join(points, by = c("site_number", "zone")) %>%
  left_join(totals, by = c("site_number", "zone"))

# NEW: all 2026 script outputs go to one folder
out_dir <- "H:/Output-1/Jackie notes processing etc/R_markdown_reports/2026_outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(out,
          file.path(out_dir, paste0("Presowing_summary_", report_year, ".csv")),
          row.names = FALSE)

# quick check: one line per site and zone
as.data.frame(out %>%
                distinct(site_number, sampling_timing, zone, zone_description,
                         points, sampling_date, water_total, N_total, N_class))

