# 04b_Neat_data_Wharminda_Woodys_25.R
# Cleans the 2025 Excel data files: one 'data' sheet per event (long format), kg/ha for all biomass and grain,
# raw sheets kept alongside, pt_ids checked against the neat shapefiles.
library(tidyverse)
library(readxl)
library(writexl)
library(sf)

# ---- A. SETTINGS ----
root      <- "H:/Output-1"
site_dir  <- "4.Wharminda_Woodys"
site_name <- "Wharminda_Woodys"
yy        <- "25"
epsg      <- 7853

shp_dir  <- file.path(root, site_dir, "4.Sampling", "2.InSeason", yy, "shapefile_neat_jackie")   # neat shapefiles (from 04a)
data_dir <- file.path(root, site_dir, "7.In_Season_data", yy, "data_neat_jackie")                # neat Excel files go here
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# xlsx_in is relative to the SITE folder. Flowering and harvest were cut with a 0.5 m2 quadrat (no rows),
# so rows_cut and length_cut_m are NA and the cut area is given directly.
events <- tribble(
  ~event,              ~date,        ~xlsx_in,                                                               ~sheet,                 ~rows_cut, ~length_cut_m, ~row_spacing_m, ~cut_area_m2,
  "Establishment",     "2025-07-10", "7.In_Season_data/25/Emergence/Cleaned_Data/Wharminda_Emerg_7854.xlsx",  "Wharminda_Emerg_7854", 4,         0.5,           0.30,           0.6,
  "Biomass_flowering", "2025-10-13", "7.In_Season_data/25/5.Biomass/Wharminda_Output1_Biomass_points_131025.xlsx", "Sheet1",          NA,        NA,            0.30,           0.5,
  "Harvest",           "2025-12-08", "7.In_Season_data/25/Maturity/Whar_O1_Harvest_Biomass_.081225.xlsx",     "Sands_Output1",        NA,        NA,            0.30,           0.5
)

# ---- B. FUNCTIONS ----
get_path <- function(i) file.path(root, site_dir, events$xlsx_in[i])

add_meta <- function(df, i) {
  df %>% mutate(event = events$event[i], year = 2000 + as.integer(yy),
                date_collected = as.Date(events$date[i]),
                rows_cut = events$rows_cut[i], length_cut_m = events$length_cut_m[i],
                row_spacing_m = events$row_spacing_m[i], cut_area_m2 = events$cut_area_m2[i])
}

# ---- C. BUILD EACH EVENT ----
dat <- list(); extra <- list()

# Establishment: calculated from the four row counts
est_raw <- read_excel(get_path(1), sheet = events$sheet[1])
counts  <- as.matrix(est_raw[, c("Row1", "Row2", "Row3", "Row4")])

est_wide <- est_raw %>%
  select(pt_id, any_of(c("treat_desc")), Row1:Row4) %>%
  rename(count_row1 = Row1, count_row2 = Row2, count_row3 = Row3, count_row4 = Row4) %>%
  mutate(pt_id        = as.integer(pt_id),
         count_total  = rowSums(counts),
         count_mean   = rowMeans(counts),
         count_sd     = sqrt(rowMeans((counts - count_mean)^2)),     # population SD
         count_cv_pct = ifelse(count_mean > 0, count_sd / count_mean * 100, NA),
         Establishment      = count_mean / (events$row_spacing_m[1] * events$length_cut_m[1]),   # plants per m2
         `Establishment CV` = count_cv_pct) %>%
  select(-any_of("treat_desc")) %>% add_meta(1)

message("establishment plants_m2 max difference vs sheet: ",
        round(max(abs(est_wide$Establishment - est_raw$plants_m2)), 6))

dat$Establishment <- est_wide %>%
  select(pt_id, event, year, date_collected, Establishment, `Establishment CV`,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(c(Establishment, `Establishment CV`), names_to = "variable", values_to = "value") %>%
  mutate(units = ifelse(variable == "Establishment", "plants_m2", "%"), .after = value) %>%
  arrange(pt_id, match(variable, c("Establishment", "Establishment CV")))
extra$Establishment <- list(raw_counts = est_wide)

# Biomass at flowering: raw sheet = header on row 4, take the columns by position
flo_wide <- read_excel(get_path(2), sheet = events$sheet[2], skip = 3, .name_repair = "unique_quiet")[, c(1, 2, 3, 4, 5, 6, 9)] %>%
  setNames(c("sample_no", "pt_id", "treat_desc", "growth_stage", "wet_weight_g", "dry_weight_g", "ndvi")) %>%
  filter(!is.na(suppressWarnings(as.numeric(pt_id)))) %>%
  mutate(across(c(wet_weight_g, dry_weight_g, ndvi), as.numeric), pt_id = as.integer(pt_id)) %>%
  select(-treat_desc) %>% add_meta(2)

dat$Biomass_flowering <- flo_wide %>%
  mutate(Biomass_flowering = dry_weight_g / cut_area_m2 * 10) %>%     # g/m2 x 10 = kg/ha
  select(pt_id, event, year, date_collected, Biomass_flowering, rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(Biomass_flowering, names_to = "variable", values_to = "value") %>%
  mutate(units = "kg/ha", .after = value) %>%
  arrange(pt_id)
extra$Biomass_flowering <- list(raw_weights = flo_wide)

# check against your 'Jackie' sheet (kg/ha)
jk_flo <- read_excel(get_path(2), sheet = "Jackie") %>% select(pt_id, jackie_kg_ha = Biomass_flowering)
message("flowering kg/ha max difference vs Jackie sheet: ",
        round(max(abs(dat$Biomass_flowering$value - jk_flo$jackie_kg_ha[match(dat$Biomass_flowering$pt_id, jk_flo$pt_id)])), 4))

# Harvest: 'Jackie' sheet gives kg/ha, kg/ha and %; Sample # is taken as the pt_id
jk <- read_excel(get_path(3), sheet = "Jackie") %>% rename_with(str_trim) %>%
  mutate(pt_id = as.integer(pt_id))
if (all(is.na(jk$`Thousand grain weight`))) message("Thousand grain weight is blank for all points, left out")

har_long <- jk %>%
  select(pt_id, Biomass_maturity, `Grain yield`, `Harvest index`) %>%
  pivot_longer(-pt_id, names_to = "variable", values_to = "value") %>%
  mutate(units = ifelse(variable == "Harvest index", "%", "kg/ha"))

har_raw <- read_excel(get_path(3), sheet = events$sheet[3], skip = 4, .name_repair = "unique_quiet")[, 1:8] %>%
  setNames(c("site", "sample_no", "biomass_g", "grain_g", "area_cut_m2", "biomass_t_ha_sheet", "grain_t_ha_sheet", "harvest_index_sheet")) %>%
  filter(!is.na(suppressWarnings(as.numeric(sample_no)))) %>%
  mutate(across(-site, as.numeric))

dat$Harvest <- har_long %>%
  add_meta(3) %>%
  select(pt_id, event, year, date_collected, variable, value, units, rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  arrange(pt_id, match(variable, c("Biomass_maturity", "Grain yield", "Harvest index")))
extra$Harvest <- list(raw_data = har_raw)

# check: kg/ha recalculated from the raw weights vs the Jackie sheet
chk <- har_raw %>% transmute(pt_id = as.integer(sample_no),
                             bm_raw = biomass_g / events$cut_area_m2[3] * 10,
                             gy_raw = grain_g   / events$cut_area_m2[3] * 10) %>%
  left_join(har_long %>% filter(variable == "Biomass_maturity") %>% select(pt_id, bm = value), by = "pt_id") %>%
  left_join(har_long %>% filter(variable == "Grain yield")      %>% select(pt_id, gy = value), by = "pt_id")
message("harvest biomass kg/ha max difference vs Jackie sheet: ", round(max(abs(chk$bm_raw - chk$bm)), 4))
message("harvest grain kg/ha max difference vs Jackie sheet: ",   round(max(abs(chk$gy_raw - chk$gy)), 4))

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

# ---- F. COMBINE ALL 2025 EVENTS INTO ONE FILE, ONE SHEET ----
files <- list.files(data_dir, full.names = TRUE,
                    pattern = paste0("^", site_name, "_(Establishment|Biomass_flowering|Harvest)_", yy, "\\.xlsx$"))

all_data <- map(files, ~ read_excel(.x, sheet = "data")) %>%
  bind_rows() %>%
  mutate(site = site_name, date_collected = as.Date(date_collected),
         comment = case_when(
           variable == "Establishment" & !is.na(value) & value == 0 ~ "Zero plants in all 4 rows (as recorded)",
           variable == "Establishment CV" & is.na(value)            ~ "CV not defined: no plants counted",
           event == "Biomass_flowering"                             ~ "Cut = 0.5 m2 quadrat (sampling notes); dry weight / 0.5 x 10 = kg/ha",
           event == "Harvest"                                       ~ "Cut = 0.5 m2 quadrat; broadcast sown so no row-length calculation; sample # in the sheet taken as pt_id",
           TRUE ~ "")) %>%
  select(site, year, event, pt_id, date_collected, variable, value, units,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2, comment)

stopifnot(!anyDuplicated(all_data[c("event", "pt_id", "variable")]))
message("combined rows: ", nrow(all_data), " (expect 581)")

write_xlsx(list(data = all_data),
           file.path(data_dir, paste0(site_name, "_All_sampling_results_", yy, ".xlsx")))