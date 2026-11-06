# 04b_Neat_data_Walpeup_MRS125_25.R
# Cleans the 2025 Excel data files: one 'data' sheet per event, standard column names,
# date_collected and method columns added, pt_ids checked against the neat shapefiles.
library(tidyverse)
library(readxl)
library(writexl)
library(sf)

# ---- A. SETTINGS ----
root      <- "H:/Output-1"
site_dir  <- "1.Walpeup_MRS125"
site_name <- "Walpeup_MRS125"
yy        <- "25"
epsg      <- 7854
keep_treatment_cols <- FALSE        # TRUE = keep treat, treat_desc, cluster3 columns

samp_dir <- file.path(root, site_dir, "4.Sampling", "2.InSeason", yy)               # where the old files are
shp_dir  <- file.path(samp_dir, "shapefile_neat_jackie")                            # neat shapefiles (from 04a)
data_dir <- file.path(root, site_dir, "7.In_Season_data", yy, "data_neat_jackie")   # neat Excel files go here
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# xlsx_in is relative to samp_dir. skip = rows above the header row.
events <- tribble(
  ~event,              ~date,        ~xlsx_in,                                                  ~sheet,   ~skip, ~rows_cut, ~length_cut_m, ~row_spacing_m,
  "Establishment",     "2025-05-19", "Emergence/ACTUAL/Walpeup_MRS125_Emergence_20250519.xlsx", "Sheet1", 0,     4,         0.5,           0.30,
  "Biomass_flowering", "2025-09-22", "Biomass/MRS125_Gums_Flowering_Biomass_22_09_2025.xlsx",   "Sheet1", 1,     4,         1.0,           0.30,     # length 1 m = what the worksheet maths uses; TO CONFIRM with Enqi
  "Harvest",   "2025-11-26", "Maturity/MRS125_Harvest_Index_Worksheet_2025.xlsx",       "Sheet1", 0,     4,         0.5,           0.3048
)

# ---- B. FUNCTIONS ----
rd <- function(i) read_excel(file.path(samp_dir, events$xlsx_in[i]),
                             sheet = events$sheet[i], skip = events$skip[i])

add_meta <- function(df, i) {
  df %>% mutate(event = events$event[i], year = 2000 + as.integer(yy),
                date_collected = as.Date(events$date[i]),
                rows_cut = events$rows_cut[i], length_cut_m = events$length_cut_m[i],
                row_spacing_m = events$row_spacing_m[i],
                cut_area_m2 = rows_cut * length_cut_m * row_spacing_m)
}

drop_treat <- function(df) {
  if (keep_treatment_cols) df else df %>% select(-any_of(c("treat", "treat_desc", "cluster3", "order")))
}

# ---- C. BUILD EACH EVENT ----
dat <- list()

# Establishment: everything is calculated here from the four row counts
est_raw <- rd(1)
counts  <- as.matrix(est_raw[, c("Row1", "Row2", "Row3", "Row4")])

dat$Establishment <- est_raw %>%
  select(pt_id, any_of(c("treat", "treat_desc", "cluster3")), Row1:Row4, any_of("notes")) %>%
  rename(count_row1 = Row1, count_row2 = Row2, count_row3 = Row3, count_row4 = Row4) %>%
  mutate(pt_id        = as.integer(pt_id),
         count_total  = rowSums(counts),
         count_mean   = rowMeans(counts),
         count_sd     = sqrt(rowMeans((counts - count_mean)^2)),     # population SD, same as STDEV.P in your sheet
         count_cv_pct = ifelse(count_mean > 0, count_sd / count_mean * 100, NA),   # blank when no plants
         Establishment      = count_mean / (events$row_spacing_m[1] * events$length_cut_m[1]),   # plants per m2
         `Establishment CV` = count_cv_pct,
         units = "plants_m2") %>%
  drop_treat() %>% add_meta(1)

# if the sheet already has its own plants_m2 column, compare it with ours
if ("plants_m2" %in% names(est_raw))
  message("establishment plants_m2 max difference vs sheet: ",
          round(max(abs(dat$Establishment$Establishment - est_raw$plants_m2)), 6))

# NEW: long format. One row per point for Establishment and one for Establishment CV, each with units
est_wide <- dat$Establishment                      # keep the wide table for the raw_counts sheet
dat$Establishment <- est_wide %>%
  select(pt_id, event, year, date_collected, Establishment, `Establishment CV`,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(c(Establishment, `Establishment CV`), names_to = "variable", values_to = "value") %>%
  mutate(units = ifelse(variable == "Establishment", "plants_m2", "%"), .after = value) %>%
  arrange(pt_id, match(variable, c("Establishment", "Establishment CV")))
extra <- list(Establishment = list(raw_counts = est_wide))     # extra sheets to write alongside 'data'
# Biomass at flowering: first sheet = MRS125 only (the Gums data is on its own sheet)
# Works on the old layout (header row 2) and the new one (header row 4):
# find the header row by looking for 'pt_id', then take the first 8 columns.
flo_raw <- read_excel(file.path(samp_dir, events$xlsx_in[2]), sheet = 1, col_names = FALSE)
h <- which(flo_raw[[2]] == "pt_id")[1]
flo_wide <- flo_raw[(h + 1):nrow(flo_raw), 1:8] %>%
  setNames(c("order", "pt_id", "treat", "treat_desc", "dried_biomass_g", "bag_g",
             "net_biomass_g", "biomass_t_ha_sheet")) %>%
  filter(!is.na(suppressWarnings(as.numeric(pt_id)))) %>%
  mutate(across(c(dried_biomass_g, bag_g, net_biomass_g, biomass_t_ha_sheet), as.numeric),
         pt_id = as.integer(pt_id)) %>%
  drop_treat() %>% add_meta(2)

# check: net weight / cut area against the sheet's own t/ha
message("flowering t/ha max difference vs sheet: ",
        round(max(abs(flo_wide$net_biomass_g / (flo_wide$cut_area_m2 * 100) - flo_wide$biomass_t_ha_sheet)), 6))

# long format: Biomass_flowering in kg/ha, with units
dat$Biomass_flowering <- flo_wide %>%
  mutate(Biomass_flowering = net_biomass_g / cut_area_m2 * 10) %>%     # g/m2 x 10 = kg/ha
  select(pt_id, event, year, date_collected, Biomass_flowering,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(Biomass_flowering, names_to = "variable", values_to = "value") %>%
  mutate(units = "kg/ha", .after = value) %>%
  arrange(pt_id)
extra$Biomass_flowering <- list(raw_weights = flo_wide)               # second sheet in the workbook

# Harvest: use the 'Jackie' tab (one row per point; point 28 = mean of 28.1 and 28.2)
har_path <- file.path(root, site_dir, "7.In_Season_data", yy, "6.Maturity_Biomass",
                      "MRS125_Harvest_Index_Worksheet_2025.xlsx")     # NEW: the updated workbook (path from the metadata sheet)
if (!"Jackie" %in% excel_sheets(har_path))
  stop("No 'Jackie' sheet in ", har_path, ". Point xlsx_in (section A) at the updated workbook.")

# Jackie tab columns: 2 pt_id | 4 Biomass_maturity, 5 units | 7 Grain yield, 8 units |
#                     10 Thousand grain weight, 11 units | 13 Harvest index, 14 units
jk <- read_excel(har_path, sheet = "Jackie", .name_repair = "minimal")
long_one <- function(val, unit) tibble(pt_id = as.integer(jk[[2]]), variable = names(jk)[val],
                                       value = as.numeric(jk[[val]]), units = as.character(jk[[unit]]))
har_long <- bind_rows(long_one(4, 5), long_one(7, 8), long_one(10, 11), long_one(13, 14)) %>%
  filter(!is.na(pt_id))

# raw tab: every row from Sheet1, including 28.1, 28.2 and the 28 mean row
har_raw <- read_excel(har_path, sheet = "Sheet1", skip = 1) %>%
  setNames(c("project", "location", "paddock", "point", "dry_biomass_g", "bag_g",
             "net_biomass_g", "dry_biomass_t_ha", "grain_g", "grain_bag_g", "net_grain_g",
             "grain_yield_t_ha", "g_per_250", "tgw_g", "harvest_index_pct", "notes")) %>%
  filter(!is.na(point)) %>%
  select(-project, -location, -paddock) %>%
  mutate(point = as.numeric(point))

dat$Harvest <- har_long %>%
  add_meta(3) %>%
  select(pt_id, event, year, date_collected, variable, value, units,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  arrange(pt_id, match(variable, c("Biomass_maturity", "Grain yield", "Thousand grain weight", "Harvest index")))
extra$Harvest <- list(raw_data = har_raw)

# check: biomass and grain kg/ha recalculated from the raw weights vs the Jackie tab
chk <- har_raw %>% filter(point %% 1 == 0) %>%
  transmute(pt_id = as.integer(point),
            bm_raw = net_biomass_g / dat$Harvest$cut_area_m2[1] * 10,
            gy_raw = net_grain_g   / dat$Harvest$cut_area_m2[1] * 10) %>%
  left_join(har_long %>% filter(variable == "Biomass_maturity") %>% select(pt_id, bm = value), by = "pt_id") %>%
  left_join(har_long %>% filter(variable == "Grain yield")      %>% select(pt_id, gy = value), by = "pt_id")
message("harvest biomass kg/ha max difference vs Jackie tab: ", round(max(abs(chk$bm_raw - chk$bm)), 4))
message("harvest grain kg/ha max difference vs Jackie tab: ",   round(max(abs(chk$gy_raw - chk$gy)), 4))

# ---- D. CHECKS ----
shp_ids <- function(ev) st_read(file.path(shp_dir, paste0(site_name, "_", ev, "_", yy, "_pts_", epsg, ".shp")),
                                quiet = TRUE)$pt_id
for (ev in names(dat)) stopifnot(setequal(dat[[ev]]$pt_id, shp_ids(ev)))      # pt_ids match the shapefile

# t/ha checks: flowering is now checked in section C; harvest is checked below
# harvest checks are now done in section C

# ---- E. WRITE AND SUMMARISE ----
for (ev in names(dat)) {
  f <- file.path(data_dir, paste0(site_name, "_", ev, "_", yy, ".xlsx"))
  write_xlsx(c(list(data = dat[[ev]]), extra[[ev]]), f)     # NEW: adds the raw_counts sheet for Establishment
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
           variable == "Establishment" & !is.na(value) & value == 0 ~ "Zero plants in all 4 rows: true zero, confirmed by Enqi (dry conditions and wind erosion on deep sand)",
           variable == "Establishment CV" & is.na(value)            ~ "CV not defined: no plants counted",
           event == "Harvest" & pt_id == 28                         ~ "Point 28 = mean of samples 28.1 and 28.2",
           event == "Biomass_flowering"                             ~ "ASSUMED cut = 4 rows x 1 m (worksheet formula and cut area). Enqi recalls 4 rows x 0.5 m for cereals, but that gives ~14 t/ha (too high). Value would double if 0.5 m.",
           TRUE ~ "")) %>%
  select(site, year, event, pt_id, date_collected, variable, value, units,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2, comment)

stopifnot(!anyDuplicated(all_data[c("event", "pt_id", "variable")]))
message("combined rows: ", nrow(all_data), " (expect 312)")

write_xlsx(list(data = all_data),
           file.path(data_dir, paste0(site_name, "_All_sampling_results_", yy, ".xlsx")))

