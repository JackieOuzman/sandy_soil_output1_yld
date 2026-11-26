# 04b_Neat_data_Wynarka_Mervs_West_25.R
# Cleans the 2025 Excel data files for site 3: long format, one 'data' sheet per event,
# standard column names, date_collected and cut size added, pt_ids checked against the neat shapefiles.
library(tidyverse)
library(readxl)
library(writexl)
library(sf)

# ---- A. SETTINGS ----
root      <- "H:/Output-1"
site_dir  <- "3.Wynarka_Mervs_West"
site_name <- "Wynarka_Mervs_West"
yy        <- "25"
epsg      <- 7854

samp_dir <- file.path(root, site_dir, "4.Sampling", "2.InSeason", yy)
shp_dir  <- file.path(samp_dir, "shapefile_neat_jackie")                            # neat shapefiles (from 04a)
data_dir <- file.path(root, site_dir, "7.In_Season_data", yy, "data_neat_jackie")   # neat Excel files go here
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# xlsx_in is relative to the SITE folder. Cut = 2 linear m (4 rows x 0.5 m); row spacing 0.3 m (TBC, one sheet had 0.28)
events <- tribble(
  ~event,              ~date,        ~xlsx_in,                                                                    ~rows_cut, ~length_cut_m, ~row_spacing_m,
  "Establishment",     "2025-07-16", "7.In_Season_data/25/Emergence/ACTUAL/Cleaned_data/Wyn_MW_Est_data.xlsx",    4,         0.5,           0.3,
  "Biomass_flowering", "2025-09-23", "7.In_Season_data/25/Biomass/Actual/Wynarka_Biomass_2025.xlsx",              4,         0.5,           0.3,
  "Harvest",           "2025-12-03", "7.In_Season_data/25/Maturity/Maturity_harvest_index.xlsx",                  4,         0.5,           0.3
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

# --- Establishment: calculated from the four row counts (first sheet) ---
est_raw <- read_excel(xl_path(1), sheet = 1)
stopifnot(all(est_raw$row_spacing_m == events$row_spacing_m[1]), all(est_raw$count_length_m == events$length_cut_m[1]))
counts  <- as.matrix(est_raw[, c("Row1", "Row2", "Row3", "Row4")])

est_wide <- est_raw %>%
  select(pt_id, Row1:Row4, cv_sheet = cv, sd_sheet = sd) %>%
  rename(count_row1 = Row1, count_row2 = Row2, count_row3 = Row3, count_row4 = Row4) %>%
  mutate(pt_id        = as.integer(pt_id),
         count_total  = rowSums(counts),
         count_mean   = rowMeans(counts),
         count_sd     = sqrt(rowMeans((counts - count_mean)^2)),     # population SD, same as sites 1 and 2 (the sheet used the sample SD)
         count_cv_pct = ifelse(count_mean > 0, count_sd / count_mean * 100, NA),
         Establishment      = count_mean / (events$row_spacing_m[1] * events$length_cut_m[1]),   # plants per m2
         `Establishment CV` = count_cv_pct) %>%
  add_meta(1)

message("establishment plants/m2 max difference vs sheet: ",
        round(max(abs(est_wide$Establishment - est_raw$plants_m2)), 6))
message("establishment SD (sample, n-1) max difference vs sheet sd: ",
        round(max(abs(est_wide$count_sd * sqrt(4 / 3) - est_wide$sd_sheet)), 6))

dat$Establishment <- est_wide %>%
  select(pt_id, event, year, date_collected, Establishment, `Establishment CV`,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(c(Establishment, `Establishment CV`), names_to = "variable", values_to = "value") %>%
  mutate(units = ifelse(variable == "Establishment", "plants_m2", "%"), .after = value) %>%
  arrange(pt_id, match(variable, c("Establishment", "Establishment CV")))
extra$Establishment <- list(raw_counts = est_wide)     # includes the sheet's own sample-SD CV (cv_sheet)

# --- Biomass at flowering: Sheet1 (header on row 3), net dry weight excluding the bag ---
flo_wide <- read_excel(xl_path(2), sheet = "Sheet1", skip = 2) %>%
  select(1:8) %>%
  setNames(c("pt_id", "treat_desc", "zone", "date_sheet", "linear_m_sampled", "bag_g",
             "dry_wt_inc_bag_g", "net_biomass_g")) %>%
  filter(!is.na(suppressWarnings(as.numeric(pt_id)))) %>%
  mutate(pt_id = as.integer(pt_id)) %>%
  select(pt_id, bag_g, dry_wt_inc_bag_g, net_biomass_g) %>%
  add_meta(2)

dat$Biomass_flowering <- flo_wide %>%
  mutate(Biomass_flowering = net_biomass_g / cut_area_m2 * 10) %>%     # g/m2 x 10 = kg/ha
  select(pt_id, event, year, date_collected, Biomass_flowering,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(Biomass_flowering, names_to = "variable", values_to = "value") %>%
  mutate(units = "kg/ha", .after = value) %>%
  arrange(pt_id)
extra$Biomass_flowering <- list(raw_weights = flo_wide)

# check against the Jackie sheet kg/ha
flo_jk <- read_excel(xl_path(2), sheet = "Jackie")
chk_f  <- dat$Biomass_flowering %>% left_join(flo_jk, by = "pt_id")
message("flowering kg/ha max difference vs Jackie sheet: ", round(max(abs(chk_f$value - chk_f$Biomass_flowering)), 6))

# --- Harvest: 'Jackie' sheet (one row per point, already kg/ha; no unit columns, so units are set here) ---
jk <- read_excel(xl_path(3), sheet = "Jackie")
har_units <- c("Biomass_maturity" = "kg/ha", "Grain yield" = "kg/ha",
               "Thousand grain weight" = "g", "Harvest index" = "%", "Protein" = "%")
stopifnot(all(names(har_units) %in% names(jk)))

har_long <- jk %>%
  mutate(pt_id = as.integer(pt_id)) %>%
  pivot_longer(all_of(names(har_units)), names_to = "variable", values_to = "value") %>%
  mutate(units = unname(har_units[variable]))

# raw weights from Sheet1 (header on row 4)
har_raw <- read_excel(xl_path(3), sheet = "Sheet1", skip = 3, .name_repair = "unique_quiet") %>%     # NEW: renames the second pt_id so select() works
  select(1:16) %>%
  setNames(c("pt_id", "treat_desc", "zone", "date_sampled", "cut_linear_m", "bag_wt_g_a", "pt_id_b", "bag_g",
             "dry_wt_inc_bag_g", "net_biomass_g", "biomass_t_ha_sheet", "grain_g", "grain_yield_t_ha_sheet",
             "tgw_g", "moisture_pct", "protein_pct")) %>%
  filter(!is.na(suppressWarnings(as.numeric(pt_id)))) %>%
  mutate(pt_id = as.integer(pt_id), moisture_pct = suppressWarnings(as.numeric(moisture_pct))) %>%
  select(pt_id, bag_g, dry_wt_inc_bag_g, net_biomass_g, grain_g, tgw_g, moisture_pct, protein_pct)

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
           event == "Harvest" & pt_id == 98                    ~ "No sample taken",
           event == "Harvest" & pt_id == 40 & variable == "Protein" ~ "Protein missing; biomass very low (935 kg/ha), harvest index 60.6%, not checked",
           event == "Harvest" & pt_id == 40 & variable == "Biomass_maturity" ~ "Very low biomass (935 kg/ha), harvest index 60.6%, not checked",
           event == "Biomass_flowering"                        ~ "Cut = 2 linear m (4 rows x 0.5 m) x 0.3 m spacing = 0.6 m2 (the sheet's own t/ha column used 0.28 m spacing, not used). Spacing 0.3 m to confirm",
           event == "Establishment" & variable == "Establishment CV"
           ~ "CV from population SD (n), same as sites 1 and 2; the sheet's own CV used the sample SD (n-1)",
           TRUE ~ "")) %>%
  select(site, year, event, pt_id, date_collected, variable, value, units,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2, comment)

stopifnot(!anyDuplicated(all_data[c("event", "pt_id", "variable")]))
message("combined rows: ", nrow(all_data), " (expect 1099 = 306 + 28 + 765)")

write_xlsx(list(data = all_data),
           file.path(data_dir, paste0(site_name, "_All_sampling_results_", yy, ".xlsx")))
