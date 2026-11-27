# 04b_Neat_data_Walpeup_Gums_25.R
# Cleans the 2025 Excel data files: one 'data' sheet per event (long format), kg/ha for all biomass and grain,
# raw sheets kept alongside, pt_ids checked against the neat shapefiles.
library(tidyverse)
library(readxl)
library(writexl)
library(sf)

# ---- A. SETTINGS ----
root      <- "H:/Output-1"
site_dir  <- "5.Walpeup_Gums"
site_name <- "Walpeup_Gums"
yy        <- "25"
epsg      <- 7854

shp_dir  <- file.path(root, site_dir, "4.Sampling", "2.InSeason", yy, "shapefile_neat_jackie")   # neat shapefiles (from 04a)
data_dir <- file.path(root, site_dir, "7.In_Season_data", yy, "data_neat_jackie")                # neat Excel files go here
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# xlsx_in is relative to the SITE folder. sheet = the raw data tab.
# Establishment: 4 rows x 0.5 m count length, 0.3 m spacing (from the sheet).
# Flowering: 4 rows x 1 m x 0.3 m = 1.2 m2 (from the sheet; to confirm, 0.5 m would double the values).
# Harvest: 4 rows x 0.5 m x 0.3 m = 0.6 m2 (2 m of row at 0.3 m).
events <- tribble(
  ~event,              ~date,        ~xlsx_in,                                                                           ~sheet,   ~rows_cut, ~length_cut_m, ~row_spacing_m, ~cut_area_m2,
  "Establishment",     "2025-05-19", "7.In_Season_data/25/Emergence/ACTUAL/Walpeup_Gums_Emergence_20250519.xlsx",         "Sheet1", 4,         0.5,           0.30,           0.6,
  "Biomass_flowering", "2025-09-22", "7.In_Season_data/25/Biomass/MRS125_Gums_Flowering_Biomass_22_09_2025.xlsx",          "Gums",   4,         1.0,           0.30,           1.2,
  "Harvest",           "2025-11-26", "7.In_Season_data/25/Maturity/Gums_Harvest_Biomass 2025.xlsx",                       "Sheet1", 4,         0.5,           0.30,           0.6
)

# ---- B. FUNCTIONS ----
get_path <- function(i) file.path(root, site_dir, events$xlsx_in[i])

add_meta <- function(df, i) {
  df %>% mutate(event = events$event[i], year = 2000 + as.integer(yy),
                date_collected = as.Date(events$date[i]),
                rows_cut = events$rows_cut[i], length_cut_m = events$length_cut_m[i],
                row_spacing_m = events$row_spacing_m[i], cut_area_m2 = events$cut_area_m2[i])
}

num <- function(x) suppressWarnings(as.numeric(x))     # text such as "NA" becomes NA

dat <- list(); extra <- list()

# ---- C. ESTABLISHMENT ----
est_raw <- read_excel(get_path(1), sheet = events$sheet[1]) %>%
  mutate(across(Row1:Row4, num))
counts <- as.matrix(est_raw[, c("Row1", "Row2", "Row3", "Row4")])

# points with a blank count (the mean uses the rows that were counted, as in the sheet)
est_raw %>% filter(if_any(Row1:Row4, is.na)) %>% select(pt_id, Row1:Row4, notes) %>% print()
n_rows <- rowSums(!is.na(counts))

est_wide <- est_raw %>%
  select(pt_id, Row1:Row4, any_of("notes")) %>%
  rename(count_row1 = Row1, count_row2 = Row2, count_row3 = Row3, count_row4 = Row4) %>%
  mutate(pt_id        = as.integer(pt_id),
         count_total  = rowSums(counts, na.rm = TRUE),
         count_mean   = count_total / n_rows,
         count_sd     = sqrt(rowSums((counts - count_mean)^2, na.rm = TRUE) / n_rows),     # population SD (the Jackie tab uses the sample SD)
         count_cv_pct = ifelse(count_mean > 0, count_sd / count_mean * 100, NA),
         Establishment      = count_mean / (events$row_spacing_m[1] * events$length_cut_m[1]),   # plants per m2
         `Establishment CV` = count_cv_pct) %>%
  add_meta(1)

message("establishment plants_m2 max difference vs sheet: ",
        round(max(abs(est_wide$Establishment - num(est_raw$plants_m2))), 6))

dat$Establishment <- est_wide %>%
  select(pt_id, event, year, date_collected, Establishment, `Establishment CV`,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(c(Establishment, `Establishment CV`), names_to = "variable", values_to = "value") %>%
  mutate(units = ifelse(variable == "Establishment", "plants_m2", "%"), .after = value) %>%
  arrange(pt_id, match(variable, c("Establishment", "Establishment CV")))
extra$Establishment <- list(raw_counts = est_wide)

# ---- D. BIOMASS AT FLOWERING (the 'Gums' tab; the MRS125 tab is site 1) ----
flo_raw <- read_excel(get_path(2), sheet = events$sheet[2], col_names = FALSE)
h <- which(flo_raw[[2]] == "pt_id")[1]                  # header row = the row with 'pt_id' in column 2
flo_wide <- flo_raw[(h + 1):nrow(flo_raw), 1:8] %>%
  setNames(c("order", "pt_id", "treat", "treat_desc", "dried_biomass_g", "bag_g",
             "net_biomass_g", "biomass_t_ha_sheet")) %>%
  filter(!is.na(num(pt_id))) %>%
  mutate(across(c(dried_biomass_g, bag_g, net_biomass_g, biomass_t_ha_sheet), num),
         pt_id = as.integer(pt_id)) %>%
  select(-order, -treat, -treat_desc) %>% add_meta(2)

message("flowering t/ha max difference vs sheet: ",
        round(max(abs(flo_wide$net_biomass_g / (flo_wide$cut_area_m2 * 100) - flo_wide$biomass_t_ha_sheet)), 6))

dat$Biomass_flowering <- flo_wide %>%
  mutate(Biomass_flowering = net_biomass_g / cut_area_m2 * 10) %>%     # g/m2 x 10 = kg/ha
  select(pt_id, event, year, date_collected, Biomass_flowering, rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(Biomass_flowering, names_to = "variable", values_to = "value") %>%
  mutate(units = "kg/ha", .after = value) %>%
  arrange(pt_id)
extra$Biomass_flowering <- list(raw_weights = flo_wide)

# check against your 'Jackie' tab (kg/ha)
jk_flo <- read_excel(get_path(2), sheet = "Jackie") %>% select(pt_id, jackie_kg_ha = Biomass_flowering)
message("flowering kg/ha max difference vs Jackie tab: ",
        round(max(abs(dat$Biomass_flowering$value - jk_flo$jackie_kg_ha[match(dat$Biomass_flowering$pt_id, jk_flo$pt_id)])), 4))

# ---- E. HARVEST ('Jackie' tab: kg/ha, kg/ha and %) ----
jk <- read_excel(get_path(3), sheet = "Jackie") %>% rename_with(str_trim) %>%
  mutate(pt_id = as.integer(pt_id), across(-pt_id, num))
message("Thousand grain weight non-zero values: ", sum(jk$`Thousand grain weight` > 0, na.rm = TRUE), " (0 = not measured, left out)")

har_long <- jk %>%
  select(pt_id, Biomass_maturity, `Grain yield`, `Harvest index`) %>%
  pivot_longer(-pt_id, names_to = "variable", values_to = "value") %>%
  mutate(units = ifelse(variable == "Harvest index", "%", "kg/ha"))

# raw tab: the weights as entered (header on row 4)
har_raw <- read_excel(get_path(3), sheet = events$sheet[3], skip = 3, .name_repair = "unique_quiet")[, 1:17] %>%
  setNames(c("project", "location", "paddock", "point", "dry_biomass_g", "bag_g", "net_biomass_g",
             "dry_biomass_t_ha", "grain_g", "grain_bag_g", "net_grain_g", "grain_yield_t_ha",
             "harvest_index_pct", "notes", "g_per_250", "tgw_g", "protein")) %>%
  filter(!is.na(num(point))) %>%
  mutate(across(c(point, dry_biomass_g:harvest_index_pct, g_per_250, tgw_g, protein), num))

dat$Harvest <- har_long %>%
  add_meta(3) %>%
  select(pt_id, event, year, date_collected, variable, value, units, rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  arrange(pt_id, match(variable, c("Biomass_maturity", "Grain yield", "Harvest index")))
extra$Harvest <- list(raw_data = har_raw)

# check: kg/ha recalculated from the raw net weights vs the Jackie tab
chk <- har_raw %>% transmute(pt_id = as.integer(point),
                             bm_raw = net_biomass_g / events$cut_area_m2[3] * 10,
                             gy_raw = net_grain_g   / events$cut_area_m2[3] * 10) %>%
  left_join(har_long %>% filter(variable == "Biomass_maturity") %>% select(pt_id, bm = value), by = "pt_id") %>%
  left_join(har_long %>% filter(variable == "Grain yield")      %>% select(pt_id, gy = value), by = "pt_id")
message("harvest biomass kg/ha max difference vs Jackie tab: ", round(max(abs(chk$bm_raw - chk$bm), na.rm = TRUE), 4))
message("harvest grain kg/ha max difference vs Jackie tab: ",   round(max(abs(chk$gy_raw - chk$gy), na.rm = TRUE), 4))
message("harvest points with no weights: ", paste(chk$pt_id[is.na(chk$bm_raw)], collapse = ", "))     # expect 97

# ---- F. CHECKS ----
shp_ids <- function(ev) st_read(file.path(shp_dir, paste0(site_name, "_", ev, "_", yy, "_pts_", epsg, ".shp")),
                                quiet = TRUE)$pt_id
for (ev in names(dat)) stopifnot(setequal(dat[[ev]]$pt_id, shp_ids(ev)))      # pt_ids match the shapefile

# ---- G. WRITE AND SUMMARISE ----
for (ev in names(dat)) {
  f <- file.path(data_dir, paste0(site_name, "_", ev, "_", yy, ".xlsx"))
  write_xlsx(c(list(data = dat[[ev]]), extra[[ev]]), f)
  message("written: ", f)
}

tibble(Site = site_name, Year = 2000 + as.integer(yy), Event = names(dat),
       Date_collected = events$date, n_rows = map_int(dat, nrow),
       n_points = map_int(dat, ~ n_distinct(.x$pt_id)),
       xlsx_target = paste0(site_name, "_", names(dat), "_", yy, ".xlsx"))

# ---- H. COMBINE ALL 2025 EVENTS INTO ONE FILE, ONE SHEET ----
files <- list.files(data_dir, full.names = TRUE,
                    pattern = paste0("^", site_name, "_(Establishment|Biomass_flowering|Harvest)_", yy, "\\.xlsx$"))

all_data <- map(files, ~ read_excel(.x, sheet = "data")) %>%
  bind_rows() %>%
  mutate(site = site_name, date_collected = as.Date(date_collected),
         comment = case_when(
           event == "Establishment" & pt_id == 75                   ~ "Row4 blank in the xlsx (0 in the shapefile): mean of the 3 counted rows used, as in the sheet",
           event == "Establishment" & pt_id == 4                    ~ "Sown around seep, different direction to rip (sheet note)",
           event == "Establishment" & pt_id == 6                    ~ "Possible double sowing (sheet note)",
           variable == "Establishment" & !is.na(value) & value == 0 ~ "Zero plants in all 4 rows (as recorded)",
           variable == "Establishment CV" & is.na(value)            ~ "CV not defined: no plants counted",
           event == "Biomass_flowering"                             ~ "Cut = 4 rows x 1 m x 0.3 m = 1.2 m2 (sheet). To confirm: 0.5 m would double the values. Plausible against harvest biomass at the same points.",
           event == "Harvest" & pt_id == 97                         ~ "Missing sample (sheet note): no weights",
           event == "Harvest"                                       ~ "Cut = 4 rows x 0.5 m x 0.3 m = 0.6 m2. Thousand grain weight not measured (zeros in the sheet), left out. Copy used = 7.In_Season_data (the 4.Sampling copy uses 0.3048 m spacing).",
           TRUE ~ "")) %>%
  select(site, year, event, pt_id, date_collected, variable, value, units,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2, comment)

stopifnot(!anyDuplicated(all_data[c("event", "pt_id", "variable")]))
message("combined rows: ", nrow(all_data), " (expect 561)")

write_xlsx(list(data = all_data),
           file.path(data_dir, paste0(site_name, "_All_sampling_results_", yy, ".xlsx")))