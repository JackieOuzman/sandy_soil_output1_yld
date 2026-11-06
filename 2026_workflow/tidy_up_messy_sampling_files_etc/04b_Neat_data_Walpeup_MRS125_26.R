# 04b_Neat_data_Walpeup_MRS125_26.R
library(tidyverse)
library(readxl)
library(writexl)
library(sf)

# ---- A. SETTINGS ----
root      <- "H:/Output-1"
site_dir  <- "1.Walpeup_MRS125"
site_name <- "Walpeup_MRS125"
yy        <- "26"
epsg      <- 7854

bio_file <- file.path(root, site_dir, "7.In_Season_data", yy, "Biomass", "MRS125_Biomass_2026.xlsx")   # check this path
bio_date <- "2026-07-16"                                                                              # to confirm with Enqi
shp_dir  <- file.path(root, site_dir, "4.Sampling", "2.InSeason", yy, "shapefile_neat_jackie")
data_dir <- file.path(root, site_dir, "7.In_Season_data", yy, "data_neat_jackie")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# ---- B. BIOMASS AT FLOWERING ----
bio_wide <- read_excel(bio_file, sheet = 1) %>%
  setNames(c("pt_id", "treat", "treat_desc", "cluster3", "dry_weight_g", "biomass_t_ha_sheet",
             "rows_cut", "n_cuts", "length_cut_m", "row_spacing_m")) %>%
  filter(!is.na(pt_id)) %>%
  mutate(pt_id = as.integer(pt_id), event = "Biomass_flowering", year = 2000 + as.integer(yy),
         date_collected = as.Date(bio_date),
         cut_area_m2 = rows_cut * length_cut_m * row_spacing_m * n_cuts) %>%
  select(-treat, -treat_desc, -cluster3)

# checks: weight / cut area against the sheet's own t/ha, and pt_ids against the shapefile
message("flowering t/ha max difference vs sheet: ",
        round(max(abs(bio_wide$dry_weight_g / (bio_wide$cut_area_m2 * 100) - bio_wide$biomass_t_ha_sheet)), 6))
shp <- st_read(file.path(shp_dir, paste0(site_name, "_Biomass_flowering_", yy, "_pts_", epsg, ".shp")), quiet = TRUE)
stopifnot(setequal(bio_wide$pt_id, shp$pt_id))

# long format, kg/ha
bio_long <- bio_wide %>%
  mutate(Biomass_flowering = dry_weight_g / cut_area_m2 * 10) %>%        # g/m2 x 10 = kg/ha
  select(pt_id, event, year, date_collected, rows_cut, length_cut_m, row_spacing_m, cut_area_m2, Biomass_flowering) %>%
  pivot_longer(Biomass_flowering, names_to = "variable", values_to = "value") %>%
  mutate(units = "kg/ha") %>%
  arrange(pt_id)

write_xlsx(list(data = bio_long, raw_weights = bio_wide),
           file.path(data_dir, paste0(site_name, "_Biomass_flowering_", yy, ".xlsx")))

# ---- C. COMBINE ALL 2026 EVENTS INTO ONE FILE, ONE SHEET ----
files <- list.files(data_dir, full.names = TRUE,
                    pattern = paste0("^", site_name, "_(Establishment|Biomass_flowering|Biomass_harvest)_", yy, "\\.xlsx$"))
all_data <- map(files, ~ read_excel(.x, sheet = "data")) %>%
  bind_rows() %>%
  mutate(site = site_name, date_collected = as.Date(date_collected),
         comment = case_when(
           event == "Biomass_flowering" ~ "Date 16 Jul 2026 confirmed by Enqi. Same locations as 2025 flowering plan. Dry weight assumed net of bag.",
           TRUE ~ "")) %>%
  select(site, year, event, pt_id, date_collected, variable, value, units,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2, comment)
stopifnot(!anyDuplicated(all_data[c("event", "pt_id", "variable")]))
message("combined rows: ", nrow(all_data), " (expect 24)")
write_xlsx(list(data = all_data), file.path(data_dir, paste0(site_name, "_All_sampling_results_", yy, ".xlsx")))

