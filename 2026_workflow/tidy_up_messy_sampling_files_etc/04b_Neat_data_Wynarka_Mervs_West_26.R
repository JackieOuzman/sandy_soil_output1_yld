# 04b_Neat_data_Wynarka_Mervs_West_26.R
# Cleans the 2026 data for site 3 (Establishment from the shapefile counts, Biomass at flowering from Excel; no Harvest data yet).
library(tidyverse)
library(readxl)
library(writexl)
library(sf)

# ---- A. SETTINGS ----
root      <- "H:/Output-1"
site_dir  <- "3.Wynarka_Mervs_West"
site_name <- "Wynarka_Mervs_West"
yy        <- "26"
epsg      <- 7854

samp_dir <- file.path(root, site_dir, "4.Sampling", "2.InSeason", yy)
shp_dir  <- file.path(samp_dir, "shapefile_neat_jackie")                            # neat shapefiles (from 04a)
data_dir <- file.path(root, site_dir, "7.In_Season_data", yy, "data_neat_jackie")   # neat Excel files go here
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# xlsx_in is relative to the SITE folder. Establishment: 4 row counts x 0.5 m, spacing 0.3 m ASSUMED (same as 2025).
# Biomass_flowering: 2 rows x 0.5 m (Enqi), spacing 0.3 m.
events <- tribble(
  ~event,              ~date,        ~xlsx_in,                                                                  ~rows_cut, ~length_cut_m, ~row_spacing_m,
  "Establishment",     "2026-06-03", "7.In_Season_data/26/Emergence/Wynarka_Mervs_Emergence_2026.shp",           4,         0.5,           0.3,    # counts are inside the shapefile
  "Biomass_flowering", "2026-09-10", "7.In_Season_data/26/Biomass/Wynarka_Mervs_West_Biomass_2026.xlsx",         2,         0.5,           0.3
)

for (p in events$xlsx_in)
  if (!file.exists(file.path(root, site_dir, p))) stop("File not found: ", file.path(root, site_dir, p))

# ---- B. FUNCTIONS ----
xl_path <- function(i) file.path(root, site_dir, events$xlsx_in[i])

add_meta <- function(df, i) {
  df %>% mutate(event = events$event[i], year = 2000 + as.integer(yy),
                date_collected = as.Date(events$date[i]),
                rows_cut = events$rows_cut[i], length_cut_m = events$length_cut_m[i],
                row_spacing_m = events$row_spacing_m[i],
                cut_area_m2 = rows_cut * length_cut_m * row_spacing_m)
}

# ---- C. BUILD EACH EVENT ----
dat   <- list()
extra <- list()

# --- Establishment: counts typed into the shapefile (row1 to row4); pt_id = fid ---
est_shp <- st_read(xl_path(1), quiet = TRUE) %>% st_drop_geometry()
stopifnot(all(est_shp$visited == "Yes"))
counts <- as.matrix(est_shp[, c("row1", "row2", "row3", "row4")])
stopifnot(is.numeric(counts), !anyNA(counts))

est_wide <- est_shp %>%
  transmute(pt_id = as.integer(fid), treat, treat_desc, rep, notes_sheet = notes,
            count_row1 = row1, count_row2 = row2, count_row3 = row3, count_row4 = row4) %>%
  mutate(count_total  = rowSums(counts),
         count_mean   = rowMeans(counts),
         count_sd     = sqrt(rowMeans((counts - count_mean)^2)),     # population SD, same as the other sites
         count_cv_pct = ifelse(count_mean > 0, count_sd / count_mean * 100, NA),
         Establishment      = count_mean / (events$row_spacing_m[1] * events$length_cut_m[1]),   # plants per m2
         `Establishment CV` = count_cv_pct) %>%
  add_meta(1)

dat$Establishment <- est_wide %>%
  select(pt_id, event, year, date_collected, Establishment, `Establishment CV`,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(c(Establishment, `Establishment CV`), names_to = "variable", values_to = "value") %>%
  mutate(units = ifelse(variable == "Establishment", "plants_m2", "%"), .after = value) %>%
  arrange(pt_id, match(variable, c("Establishment", "Establishment CV")))
extra$Establishment <- list(raw_counts = est_wide)

# --- Biomass at flowering: sheet 'WYN West', net dry weight (g) per point (Plot = pt_id, 2025 ids) ---
flo_wide <- read_excel(xl_path(2), sheet = "WYN West") %>%
  select(pt_id = Plot, bag_g = `Bag Wt`, dry_wt_inc_bag_g = `Dry Wt (w/ Bag)`,
         net_biomass_g = `Dry wt`, biomass_kg_ha_sheet = `dry biomass kg/ha`) %>%
  filter(!is.na(pt_id)) %>%
  mutate(pt_id = as.integer(pt_id)) %>%
  add_meta(2)

message("flowering: net weight = weight with bag - bag, max difference: ",
        round(max(abs(flo_wide$dry_wt_inc_bag_g - flo_wide$bag_g - flo_wide$net_biomass_g)), 6))

dat$Biomass_flowering <- flo_wide %>%
  mutate(Biomass_flowering = net_biomass_g / cut_area_m2 * 10) %>%     # g/m2 x 10 = kg/ha
  select(pt_id, event, year, date_collected, Biomass_flowering,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(Biomass_flowering, names_to = "variable", values_to = "value") %>%
  mutate(units = "kg/ha", .after = value) %>%
  arrange(pt_id)
extra$Biomass_flowering <- list(raw_weights = flo_wide)

# the sheet's own kg/ha assumed ONE row (0.15 m2), ours uses 2 rows (0.3 m2), so the ratio should be 2
message("flowering: sheet kg/ha divided by ours (expect 2): ",
        paste(unique(round(flo_wide$biomass_kg_ha_sheet / (flo_wide$net_biomass_g / flo_wide$cut_area_m2 * 10), 3)), collapse = ", "))

# ---- D. CHECKS ----
shp_ids <- function(ev) st_read(file.path(shp_dir, paste0(site_name, "_", ev, "_", yy, "_pts_", epsg, ".shp")),
                                quiet = TRUE)$pt_id
for (ev in names(dat)) stopifnot(setequal(dat[[ev]]$pt_id, shp_ids(ev)))      # pt_ids match the shapefile

# ---- E. WRITE AND SUMMARISE ----
for (ev in names(dat)) {
  f <- file.path(data_dir, paste0(site_name, "_", ev, "_", yy, ".xlsx"))
  write_xlsx(c(list(data = dat[[ev]]), extra[[ev]]), f)
  message("written: ", f)
}

tibble(Site = site_name, Year = 2000 + as.integer(yy), Event = names(dat),
       Date_collected = events$date, n_rows = map_int(dat, nrow),
       n_points = map_int(dat, ~ n_distinct(.x$pt_id)),
       xlsx_target = paste0(site_name, "_", names(dat), "_", yy, ".xlsx"))

# ---- F. COMBINE ALL 2026 EVENTS INTO ONE FILE, ONE SHEET ----
files <- list.files(data_dir, full.names = TRUE,
                    pattern = paste0("^", site_name, "_(Establishment|Biomass_flowering|Harvest)_", yy, "\\.xlsx$"))

all_data <- map(files, ~ read_excel(.x, sheet = "data")) %>%
  bind_rows() %>%
  mutate(site = site_name, date_collected = as.Date(date_collected),
         comment = case_when(
           event == "Biomass_flowering" ~ "Cut = 2 rows x 0.5 m x 0.3 m = 0.3 m2 (Enqi). The sheet's own kg/ha assumed 1 row (0.15 m2) so is 2x higher. Same points as the 2025 flowering plan (2025 ids)",
           event == "Establishment" & variable == "Establishment"
           ~ "pt_id = fid (not the 2025 points). Count length 0.5 m and row spacing 0.3 m ASSUMED (same as 2025), to confirm",
           TRUE ~ "")) %>%
  select(site, year, event, pt_id, date_collected, variable, value, units,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2, comment)

stopifnot(!anyDuplicated(all_data[c("event", "pt_id", "variable")]))
message("combined rows: ", nrow(all_data), " (expect 220 = 192 + 28). No Harvest data yet.")

write_xlsx(list(data = all_data),
           file.path(data_dir, paste0(site_name, "_All_sampling_results_", yy, ".xlsx")))