# 04b_Neat_data_Crystal_Brook_Brians_House_26.R
# Cleans the 2026 Excel data files for site 2 (Establishment and Biomass at flowering; no Harvest data yet).
library(tidyverse)
library(readxl)
library(writexl)
library(sf)

# ---- A. SETTINGS ----
root      <- "H:/Output-1"
site_dir  <- "2.Crystal_Brook_Brians_House"
site_name <- "Crystal_Brook_Brians_House"
yy        <- "26"
epsg      <- 7854

samp_dir <- file.path(root, site_dir, "4.Sampling", "2.InSeason", yy)
shp_dir  <- file.path(samp_dir, "shapefile_neat_jackie")                            # neat shapefiles (from 04a)
data_dir <- file.path(root, site_dir, "7.In_Season_data", yy, "data_neat_jackie")   # neat Excel files go here
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# xlsx_in is relative to the SITE folder. Cut sizes: 4 rows x 0.5 m (Enqi). Establishment count length ASSUMED 0.5 m
events <- tribble(
  ~event,              ~date,        ~xlsx_in,                                                                ~rows_cut, ~length_cut_m, ~row_spacing_m,
  "Establishment",     "2026-06-11", "7.In_Season_data/26/Emergence/BHO_Emergence_2026.xlsx",                 4,         0.5,           0.3,
  "Biomass_flowering", "2026-09-09", "7.In_Season_data/26/Biomass/Crystal_Brook_BHO_Biomass_2026.xlsx",      4,         0.5,           0.3
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

# --- Establishment: calculated from the four row counts (Sheet1). Point 3 is out of strip: no counts ---
est_raw <- read_excel(xl_path(1), sheet = "Sheet1")
# NEW: show any count cells that are not numbers, then convert the columns to numbers
est_raw %>% filter(if_any(c(Row1, Row2, Row3, Row4, Total), ~ !is.na(.x) & is.na(suppressWarnings(as.numeric(.x))))) %>%
  select(pt_id, Row1:Total, notes) %>% print()
est_raw <- est_raw %>% mutate(across(c(Row1, Row2, Row3, Row4, Total), ~ suppressWarnings(as.numeric(.x))))
counts  <- as.matrix(est_raw[, c("Row1", "Row2", "Row3", "Row4")])

est_wide <- est_raw %>%
  select(pt_id, Row1:Row4, Total, notes) %>%
  rename(count_row1 = Row1, count_row2 = Row2, count_row3 = Row3, count_row4 = Row4,
         count_total_sheet = Total, notes_sheet = notes) %>%
  mutate(pt_id        = as.integer(pt_id),
         count_total  = rowSums(counts),
         count_mean   = rowMeans(counts),
         count_sd     = sqrt(rowMeans((counts - count_mean)^2)),     # population SD
         count_cv_pct = ifelse(count_mean > 0, count_sd / count_mean * 100, NA),
         Establishment      = count_mean / (events$row_spacing_m[1] * events$length_cut_m[1]),   # plants per m2
         `Establishment CV` = count_cv_pct) %>%
  add_meta(1)

message("establishment count total max difference vs sheet Total: ",
        round(max(abs(est_wide$count_total - est_wide$count_total_sheet), na.rm = TRUE), 6))
message("establishment points with no counts: ", paste(est_wide$pt_id[is.na(est_wide$count_mean)], collapse = ", "))

dat$Establishment <- est_wide %>%
  select(pt_id, event, year, date_collected, Establishment, `Establishment CV`,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
  pivot_longer(c(Establishment, `Establishment CV`), names_to = "variable", values_to = "value") %>%
  mutate(units = ifelse(variable == "Establishment", "plants_m2", "%"), .after = value) %>%
  arrange(pt_id, match(variable, c("Establishment", "Establishment CV")))
extra$Establishment <- list(raw_counts = est_wide)     # includes the notes column (grass scores 0-9)

# --- Biomass at flowering: net dry weight (g) per point (Sheet1) ---
flo_wide <- read_excel(xl_path(2), sheet = "Sheet1") %>%
  select(pt_id, net_biomass_g = `Net Dry Weight`, any_of("notes")) %>%
  filter(!is.na(pt_id)) %>%
  mutate(pt_id = as.integer(pt_id), net_biomass_g = as.numeric(net_biomass_g)) %>%
  add_meta(2)

dat$Biomass_flowering <- flo_wide %>%
  mutate(Biomass_flowering = net_biomass_g / cut_area_m2 * 10) %>%     # g/m2 x 10 = kg/ha
  select(pt_id, event, year, date_collected, Biomass_flowering,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2) %>%
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
           event == "Establishment" & pt_id == 3          ~ "Out of strip: no plants counted (sheet note 'Outofstrip')",
           event == "Biomass_flowering" & pt_id == 3      ~ "Establishment sheet says point 3 was out of strip; weight 87 g is the lowest of the 20, not checked",
           event == "Establishment" & variable == "Establishment"
           ~ "Count length ASSUMED 0.5 m and row spacing 0.3 m (same as 2025), to confirm",
           TRUE ~ "")) %>%
  select(site, year, event, pt_id, date_collected, variable, value, units,
         rows_cut, length_cut_m, row_spacing_m, cut_area_m2, comment)

stopifnot(!anyDuplicated(all_data[c("event", "pt_id", "variable")]))
message("combined rows: ", nrow(all_data), " (expect 260 = 240 + 20). No Harvest data yet.")

write_xlsx(list(data = all_data),
           file.path(data_dir, paste0(site_name, "_All_sampling_results_", yy, ".xlsx")))
