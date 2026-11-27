# 04b_Neat_data_Walpeup_Gums_26.R
# 2026 Establishment (from the four row counts) and Biomass at flowering, long format, kg/ha.
# No Harvest yet (no data at 27 Nov 2026).
library(tidyverse)
library(readxl)
library(writexl)
library(sf)

# ---- A. SETTINGS ----
root      <- "H:/Output-1"
site_dir  <- "5.Walpeup_Gums"
site_name <- "Walpeup_Gums"
yy        <- "26"
epsg      <- 7854

shp_dir  <- file.path(root, site_dir, "4.Sampling", "2.InSeason", yy, "shapefile_neat_jackie")
data_dir <- file.path(root, site_dir, "7.In_Season_data", yy, "data_neat_jackie")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# xlsx_in is relative to the SITE folder.
# Establishment: 4 rows x 0.5 m count length, 0.3 m spacing (count length assumed, spacing from the sheet note at point 1).
# Flowering: 4 rows x 0.5 m x 0.3 m = 0.6 m2 (sheet).
events <- tribble(
  ~event,              ~date,        ~xlsx_in,                                                                              ~sheet,   ~rows_cut, ~length_cut_m, ~row_spacing_m, ~cut_area_m2,
  "Establishment",     "2026-05-20", "7.In_Season_data/26/Emergence/ACTUAL/Cleaned/Walpeup_Gums_Emergence_2026_Cleaned.xlsx", "Sheet1", 4,         0.5,           0.30,           0.6,
  "Biomass_flowering", "2026-09-16", "7.In_Season_data/26/Biomass/Walpeup_Gums_Biomass_2026.xlsx",                           "Sheet1", 4,         0.5,           0.30,           0.6
)

get_path <- function(i) file.path(root, site_dir, events$xlsx_in[i])

add_meta <- function(df, i) {
  df %>% mutate(event = events$event[i], year = 2000 + as.integer(yy),
                date_collected = as.Date(events$date[i]),
                rows_cut = events$rows_cut[i], length_cut_m = events$length_cut_m[i],
                row_spacing_m = events$row_spacing_m[i], cut_area_m2 = events$cut_area_m2[i])
}

dat <- list(); extra <- list()

# ---- B. ESTABLISHMENT ----
# only the counts, notes and pt_id are used. The X, Y and treat columns in this xlsx do not match the shapefile.
est_raw <- read_excel(get_path(1), sheet = events$sheet[1])

# the counts can hold the text "NA": show those cells, then convert to numbers
est_raw %>% filter(if_any(Row1:Row4, ~ is.na(suppressWarnings(as.numeric(.x))))) %>%
  select(pt_id, Row1:Row4, notes) %>% print()
est_raw <- est_raw %>% mutate(across(Row1:Row4, ~ suppressWarnings(as.numeric(.x))))
counts  <- as.matrix(est_raw[, c("Row1", "Row2", "Row3", "Row4")])

est_wide <- est_raw %>%
  select(pt_id, Row1:Row4, any_of("notes")) %>%
  rename(count_row1 = Row1, count_row2 = Row2, count_row3 = Row3, count_row4 = Row4) %>%
  mutate(pt_id        = as.integer(pt_id),
         count_total  = rowSums(counts),
         count_mean   = rowMeans(counts),
         count_sd     = sqrt(rowMeans((counts - count_mean)^2)),     # population SD
         count_cv_pct = ifelse(count_mean > 0, count_sd / count_mean * 100, NA),
         Establishment      = count_mean / (events$row_spacing_m[1] * events$length_cut_m[1]),   # plants per m2
         `Establishment CV` = count_cv_pct) %>%
  add_meta(1)

# the sheet's own Total column against ours
message("establishment total max difference vs sheet: ",
        round(max(abs(est_wide$count_total - suppressWarnings(as.numeric(est_raw$Total))), na.rm = TRUE), 6))

dat$Establishment <- est_wide %>%
  select(pt_id, event, year, date_collected, Establishment, `Establishment CV`,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(c(Establishment, `Establishment CV`), names_to = "variable", values_to = "value") %>%
  mutate(units = ifelse(variable == "Establishment", "plants_m2", "%"), .after = value) %>%
  arrange(pt_id, match(variable, c("Establishment", "Establishment CV")))
extra$Establishment <- list(raw_counts = est_wide)

# ---- C. BIOMASS AT FLOWERING ----
flo_wide <- read_excel(get_path(2), sheet = events$sheet[2], .name_repair = "unique_quiet")[, 1:7] %>%
  setNames(c("pt_id", "treat_desc", "treat", "cluster3", "dry_weight_g", "biomass_t_ha_sheet", "notes")) %>%
  filter(!is.na(pt_id)) %>%
  mutate(across(c(dry_weight_g, biomass_t_ha_sheet), as.numeric), pt_id = as.integer(pt_id)) %>%
  select(-treat_desc, -treat, -cluster3) %>% add_meta(2)

message("flowering t/ha max difference vs sheet: ",
        round(max(abs(flo_wide$dry_weight_g / (flo_wide$cut_area_m2 * 100) - flo_wide$biomass_t_ha_sheet)), 6))

dat$Biomass_flowering <- flo_wide %>%
  mutate(Biomass_flowering = dry_weight_g / cut_area_m2 * 10) %>%     # g/m2 x 10 = kg/ha
  select(pt_id, event, year, date_collected, Biomass_flowering, rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(Biomass_flowering, names_to = "variable", values_to = "value") %>%
  mutate(units = "kg/ha", .after = value) %>%
  arrange(pt_id)
extra$Biomass_flowering <- list(raw_weights = flo_wide)

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
           event == "Establishment" & pt_id == 4                    ~ "Point 4 skipped (sheet note 'Skiped'): counts NA",
           event == "Establishment" & pt_id %in% c(5, 6)            ~ "Double sown (sheet note). Count length 0.5 m assumed",
           event == "Establishment" & pt_id == 21                   ~ "Sampled 4 m to the east of the point because of a wheel track (sheet note). Count length 0.5 m assumed",
           variable == "Establishment" & !is.na(value) & value == 0 ~ "Zero plants in all 4 rows (as recorded)",
           variable == "Establishment CV" & is.na(value)            ~ "CV not defined: no plants counted",
           event == "Establishment"                                 ~ "Count length 0.5 m and spacing 0.3 m assumed. Counts by pt_id; the xlsx X, Y and treat columns were not used (they do not match the shapefile)",
           event == "Biomass_flowering"                             ~ "Cut = 4 x 0.5 m rows at 0.3 m = 0.6 m2 (sheet note). Dry weight assumed net of bag. Same 21 points as in the Establishment shapefile. No harvest data yet.",
           TRUE ~ "")) %>%
  select(site, year, event, pt_id, date_collected, variable, value, units,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2, comment)

stopifnot(!anyDuplicated(all_data[c("event", "pt_id", "variable")]))
message("combined rows: ", nrow(all_data), " (expect 237)")

write_xlsx(list(data = all_data),
           file.path(data_dir, paste0(site_name, "_All_sampling_results_", yy, ".xlsx")))