# 04b_Neat_data_Crystal_Brook_Brians_House_25.R
# Cleans the 2025 Excel data files for site 2: long format, one 'data' sheet per event,
# standard column names, date_collected and cut size added, pt_ids checked against the neat shapefiles.
library(tidyverse)
library(readxl)
library(writexl)
library(sf)

# ---- A. SETTINGS ----
root      <- "H:/Output-1"
site_dir  <- "2.Crystal_Brook_Brians_House"
site_name <- "Crystal_Brook_Brians_House"
yy        <- "25"
epsg      <- 7854

samp_dir <- file.path(root, site_dir, "4.Sampling", "2.InSeason", yy)
shp_dir  <- file.path(samp_dir, "shapefile_neat_jackie")                            # neat shapefiles (from 04a)
data_dir <- file.path(root, site_dir, "7.In_Season_data", yy, "data_neat_jackie")   # neat Excel files go here
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# xlsx_in is relative to the SITE folder (hard coded, files are in different places)
# Establishment date: sheet says 3 July, the shapefile name says 10 July. TO CONFIRM
events <- tribble(
  ~event,              ~date,        ~xlsx_in,                                                                                     ~rows_cut, ~length_cut_m, ~row_spacing_m,
  "Establishment",     "2025-07-03", "7.In_Season_data/25/Emergence/ACTUAL/Cleaned_data/CB_BH_Emerg_data.xlsx",                    4,         0.5,           0.3,
  "Biomass_flowering", "2025-09-17", "7.In_Season_data/25/Biomass/CSIRO Sands 2.0 DATA ENTRY 2025.xlsx",                           4,         0.5,           0.3,
  "Harvest",           "2025-11-11", "7.In_Season_data/25/Maturity/Crystal_Brook_BHO_Maturity_Biomass_2025.xlsx",                  4,         0.5,           0.3     # path ASSUMED, check
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
extra <- list()     # extra sheets to write alongside 'data'

# --- Establishment: calculated here from the four row counts (sheet: Emerg_data) ---
est_raw <- read_excel(xl_path(1), sheet = "Emerg_data")
counts  <- as.matrix(est_raw[, c("Row1", "Row2", "Row3", "Row4")])

est_wide <- est_raw %>%
  select(pt_id, Row1:Row4) %>%
  rename(count_row1 = Row1, count_row2 = Row2, count_row3 = Row3, count_row4 = Row4) %>%
  mutate(pt_id        = as.integer(pt_id),
         count_total  = rowSums(counts),
         count_mean   = rowMeans(counts),
         count_sd     = sqrt(rowMeans((counts - count_mean)^2)),     # population SD, same as the sheet
         count_cv_pct = ifelse(count_mean > 0, count_sd / count_mean * 100, NA),
         Establishment      = count_mean / (events$row_spacing_m[1] * events$length_cut_m[1]),   # plants per m2
         `Establishment CV` = count_cv_pct) %>%
  add_meta(1)

# check against the sheet's own plants/m2 and CV
message("establishment plants/m2 max difference vs sheet: ",
        round(max(abs(est_wide$Establishment - est_raw$`plants/m²`)), 6))
message("establishment CV max difference vs sheet: ",
        round(max(abs(est_wide$`Establishment CV` - est_raw$cv)), 6))

dat$Establishment <- est_wide %>%
  select(pt_id, event, year, date_collected, Establishment, `Establishment CV`,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(c(Establishment, `Establishment CV`), names_to = "variable", values_to = "value") %>%
  mutate(units = ifelse(variable == "Establishment", "plants_m2", "%"), .after = value) %>%
  arrange(pt_id, match(variable, c("Establishment", "Establishment CV")))
extra$Establishment <- list(raw_counts = est_wide)

# --- Biomass at flowering: sheet 'Output 1P' of the shared workbook, Brians House rows only ---
# cols: 1 Paddock | 2 pt_id | 3 Dry Net Weight (g) 250917 | 4 Cut area | 5 Dry weight (kg/ha)
flo_wide <- read_excel(xl_path(2), sheet = "Output 1P", col_names = FALSE) %>%
  select(1:5) %>%
  setNames(c("paddock", "pt_id", "net_biomass_g", "cut_area_m2_sheet", "biomass_kg_ha_sheet")) %>%
  filter(paddock == "Brians House") %>%
  mutate(across(-paddock, as.numeric), pt_id = as.integer(pt_id)) %>%
  select(-paddock) %>%
  add_meta(2) %>%
  mutate(note = case_when(
    pt_id == 33 ~ "EXCLUDED from the data sheet: weight 5258 g is about 10x the others (87,641 kg/ha), probably a decimal slip",
    pt_id == 11 ~ "Plan point was 111, data entered as 11: probably a typo, NOT changed",
    TRUE ~ ""))

message("flowering kg/ha max difference vs sheet: ",
        round(max(abs(flo_wide$net_biomass_g / flo_wide$cut_area_m2 * 10 - flo_wide$biomass_kg_ha_sheet)), 6))

dat$Biomass_flowering <- flo_wide %>%
  filter(pt_id != 33) %>%
  mutate(Biomass_flowering = net_biomass_g / cut_area_m2 * 10) %>%     # g/m2 x 10 = kg/ha
  select(pt_id, event, year, date_collected, Biomass_flowering,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(Biomass_flowering, names_to = "variable", values_to = "value") %>%
  mutate(units = "kg/ha", .after = value) %>%
  arrange(pt_id)
extra$Biomass_flowering <- list(raw_weights = flo_wide)

# --- Harvest: 'Jackie' sheet (one row per point; no unit columns, so units are set here) ---
jk <- read_excel(xl_path(3), sheet = "Jackie")
har_units <- c("Biomass_maturity" = "kg/ha", "Grain yield" = "kg/ha",
               "Thousand grain weight" = "g", "Harvest index" = "%", "Protein" = "%")
stopifnot(all(names(har_units) %in% names(jk)))

har_long <- jk %>%
  mutate(pt_id = as.integer(pt_id)) %>%
  pivot_longer(all_of(names(har_units)), names_to = "variable", values_to = "value") %>%
  mutate(units = unname(har_units[variable]))

# raw weights: first 17 columns of the Wts sheet (header on row 4)
har_raw <- read_excel(xl_path(3), sheet = 1, skip = 3) %>%
  select(1:17) %>%
  setNames(c("pt_id", "location", "site", "treat_desc", "zone", "linear_m_cut", "sample_wt_inc_bag_g",
             "bag_g", "net_biomass_g", "area_cut_m2", "biomass_t_ha", "grain_g", "grain_yield_t_ha",
             "harvest_index", "tgw_g", "moisture_pct", "protein_pct")) %>%
  filter(!is.na(suppressWarnings(as.numeric(pt_id)))) %>%
  mutate(pt_id = as.integer(pt_id)) %>%
  select(-location, -site, -treat_desc, -zone)

dat$Harvest <- har_long %>%
  add_meta(3) %>%
  select(pt_id, event, year, date_collected, variable, value, units,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  arrange(pt_id, match(variable, names(har_units)))
extra$Harvest <- list(raw_data = har_raw)

# check: biomass and grain kg/ha recalculated from the raw weights vs the Jackie sheet
chk <- har_raw %>%
  transmute(pt_id, bm_raw = net_biomass_g / dat$Harvest$cut_area_m2[1] * 10,
            gy_raw = grain_g       / dat$Harvest$cut_area_m2[1] * 10) %>%
  left_join(har_long %>% filter(variable == "Biomass_maturity") %>% select(pt_id, bm = value), by = "pt_id") %>%
  left_join(har_long %>% filter(variable == "Grain yield")      %>% select(pt_id, gy = value), by = "pt_id")
message("harvest biomass kg/ha max difference vs Jackie sheet: ", round(max(abs(chk$bm_raw - chk$bm), na.rm = TRUE), 4))
message("harvest grain kg/ha max difference vs Jackie sheet: ",   round(max(abs(chk$gy_raw - chk$gy), na.rm = TRUE), 4))

# ---- D. CHECKS ----
shp_ids <- function(ev) st_read(file.path(shp_dir, paste0(site_name, "_", ev, "_", yy, "_pts_", epsg, ".shp")),
                                quiet = TRUE)$pt_id
stopifnot(setequal(dat$Establishment$pt_id, shp_ids("Establishment")))     # 120 points
stopifnot(setequal(dat$Harvest$pt_id,       shp_ids("Harvest")))           # 120 points
# flowering: the data ids do not exactly match the plan (11 instead of 111, 33 excluded), so check against Establishment
stopifnot(all(dat$Biomass_flowering$pt_id %in% shp_ids("Establishment")))
message("flowering: in data, not in plan = ",   paste(setdiff(dat$Biomass_flowering$pt_id, shp_ids("Biomass_flowering")), collapse = ", "),
        " | in plan, not in data = ", paste(setdiff(shp_ids("Biomass_flowering"), dat$Biomass_flowering$pt_id), collapse = ", "))

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

# ---- F. COMBINE ALL 2025 EVENTS INTO ONE FILE, ONE SHEET ----
files <- list.files(data_dir, full.names = TRUE,
                    pattern = paste0("^", site_name, "_(Establishment|Biomass_flowering|Harvest)_", yy, "\\.xlsx$"))

all_data <- map(files, ~ read_excel(.x, sheet = "data")) %>%
  bind_rows() %>%
  mutate(site = site_name, date_collected = as.Date(date_collected),
         comment = case_when(
           event == "Establishment" & pt_id == 77 & variable == "Establishment"
           ~ "Row 2 count = 180 (next highest count is 115): possible typo, not checked",
           event == "Biomass_flowering" & pt_id == 11    ~ "Plan point was 111, data entered as 11: probably a typo, NOT changed",
           event == "Harvest" & pt_id == 20 & variable %in% c("Grain yield", "Harvest index")
           ~ "Missing: grain spilled",
           event == "Harvest" & pt_id == 100 & variable == "Harvest index"
           ~ "Harvest index 75% (364.7 g grain from 485.4 g): very high, not checked",
           TRUE ~ "")) %>%
  select(site, year, event, pt_id, date_collected, variable, value, units,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2, comment)

stopifnot(!anyDuplicated(all_data[c("event", "pt_id", "variable")]))
message("combined rows: ", nrow(all_data), " (expect 859 = 240 + 19 + 600)")

write_xlsx(list(data = all_data),
           file.path(data_dir, paste0(site_name, "_All_sampling_results_", yy, ".xlsx")))
