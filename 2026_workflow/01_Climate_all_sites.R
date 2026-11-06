################################################################################
### 01_Climate_all_sites.R
### CHUNK 1: Download SILO patch-point files and tidy (4 stations cover all 8 sites)
################################################################################

library(tidyverse)
library(SWTools)

download_met <- TRUE    # set FALSE to re-use the files already in climate_dir

out_dir <- "H:/Output-1/Jackie notes processing etc/R_markdown_reports/2026_outputs"
stopifnot(dir.exists(out_dir))
climate_dir <- file.path(out_dir, "climate")
if (!dir.exists(climate_dir)) { dir.create(climate_dir); message("Created folder: ", climate_dir) }

# 4 stations cover the 8 sites (from the site_station list in 03_Maps_for_report.R)
stations <- tibble(
  Name      = c("Walpeup", "Crystal Brook", "Wynarka", "Wharminda"),
  Number    = c("76065",   "21016",         "25006",   "18113"),
  Latitude  = c(-35.14,    -33.35,          -35.09,    -33.97),
  Longitude = c(142.02,    138.21,          139.90,    136.25),
  State     = c("VIC",     "SA",            "SA",      "SA"),
  Sites     = c("1, 5",    "2, 6",          "3, 8",    "4, 7"))

# ---- Download (enddate left out, so SILO defaults to yesterday) ---------------
if (download_met) {
  for (num in stations$Number) {
    tryCatch({
      SILODownload(SiteList = num, username = "jackie.ouzman@csiro.au", password = "gui",
                   path = climate_dir, startdate = "19000101", ssl = FALSE)
      message("downloaded: ", num)
    }, error = function(e) message("DOWNLOAD FAILED: ", num, ": ", conditionMessage(e)))
  }
}

# ---- Tidy one station's file (same steps as 01_01) ---------------------------
tidy_met <- function(num) {
  f <- file.path(climate_dir, paste0(num, ".txt"))
  stopifnot(file.exists(f))
  
  df <- read_table(f, skip = 51,
                   col_types = cols(Date2 = col_date(format = "%d-%m-%Y"),
                                    Day = col_number(), Radn = col_number(),
                                    T.Max = col_number(), T.Min = col_number(),
                                    Rain = col_number(), Evap = col_number(), VP = col_number()))
  
  df <- df %>%
    filter(Date != "(yyyymmdd)") %>%          # the units row (it gives a harmless parse warning)
    select(Date, Day, Date2, T.Max, T.Min, Rain, Evap, Radn) %>%
    rename(day_of_year = Day, Date_string = Date, Date = Date2) %>%
    mutate(year = year(Date), month = month(Date), day_of_month = day(Date),
           month_name = month(Date, label = TRUE), Number = num) %>%
    left_join(stations %>% select(Name, Number, Latitude, Longitude, State), by = "Number")
  
  write_csv(df, file.path(climate_dir, paste0("Neater_file_", num, ".csv")))
  df
}

met <- map(set_names(stations$Number), tidy_met)

# ---- CHECK: last date in each file, and any missing days ---------------------
met_summary <- map_dfr(met, ~ tibble(Name = unique(.x$Name),
                                     first_date = min(.x$Date),
                                     last_date  = max(.x$Date),
                                     n_days     = nrow(.x)),
                       .id = "Number") %>%
  mutate(missing_days = as.integer(last_date - first_date) + 1 - n_days)
met_summary


# NEW: everything from here down is new (chunk 2)
################################################################################
### CHUNK 2: Seasons, decile tables and plot for each station
################################################################################

# Season rules (same as 01_02):
#   GS     = 1 Apr to 14 Oct (15 Oct itself is "other", as in the old script)
#   summer = 1 Jan to 31 Mar plus 1 Nov to 31 Dec of the same calendar year
GS_defined_as     <- "1/4 to 15/10"
summer_defined_as <- "1/11 to 1/4"
data_start <- as.Date("1900-01-01")
data_end   <- as.Date("2026-12-31")   # left as is: the 2026 summer is partial, see Data_to

# decile table: max = 10th to 100th percentile of the yearly sums, min = previous max + 0.1
make_decile_table <- function(sums, prefix) {
  tab <- tibble(max = as.numeric(quantile(sums, seq(0.1, 1, 0.1))),
                names = paste0("decile_", 1:10)) %>%
    mutate(min = lag(max + 0.1))
  tab$min[1] <- min(sums)
  tab %>%
    select(names, min, max) %>%
    set_names(paste0(prefix, c("_deciles_names", "_decile_min_rain", "_decile_max_rain")))
}

# which decile does each yearly sum fall in (same rounding as the old case_when)
assign_decile <- function(x, tab) {
  mins <- round(tab[[2]], 1); maxs <- round(tab[[3]], 1)
  map_chr(round(x, 1), function(v) {
    i <- which(v >= mins & v <= maxs)[1]
    if (is.na(i)) "other" else paste0("decile_", i)
  })
}

run_station <- function(num) {
  df <- met[[num]] %>%
    mutate(md = month * 100 + day_of_month,
           season = case_when(md >= 401 & md < 1015 ~ "gs",
                              md >= 1101 | md < 401  ~ "summer",
                              TRUE                   ~ "other"),
           season_GS     = if_else(season == "gs", "gs", "other"),             # NEW: the report reads this column
           season_summer = if_else(season == "summer", "summer", "other")) %>% # NEW: the report reads this column
    select(-md)
  df_all_date <- df
  df <- df %>% filter(between(Date, data_start, data_end))
  
  station_name <- unique(df$Name)
  data_to      <- max(df$Date)
  years_inc    <- paste0(year(min(df$Date)), " to ", year(max(df$Date)))
  
  season_rain <- df %>%
    filter(season != "other") %>%
    group_by(year, season) %>%
    summarise(sum_rain_season = sum(Rain, na.rm = TRUE), .groups = "drop")
  gs_rain     <- season_rain %>% filter(season == "gs")
  summer_rain <- season_rain %>% filter(season == "summer")
  
  add_info <- function(tab) {
    tab %>%
      mutate(Station_name = station_name, Station_number = num,
             Years_included = years_inc, GS_period = GS_defined_as,
             summer_period = summer_defined_as, Data_to = as.character(data_to)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      as.data.frame()
  }
  
  gs_tab     <- make_decile_table(gs_rain$sum_rain_season, "GS")
  summer_tab <- make_decile_table(summer_rain$sum_rain_season, "summer")
  GS_decile_table_df     <- add_info(gs_tab)
  summer_decile_table_df <- add_info(summer_tab)
  
  # year with decile and rain for both seasons (deciles assigned with the unrounded tables)
  Year_with_Deciles <- gs_rain %>%
    transmute(year, gs_sum_rain = sum_rain_season,
              gs_decile = assign_decile(sum_rain_season, gs_tab)) %>%
    left_join(summer_rain %>%
                transmute(year, summer_sum_rain = sum_rain_season,
                          summer_decile = assign_decile(sum_rain_season, summer_tab)),
              by = "year") %>%
    mutate(Station_name = station_name, Station_number = num,
           GS_period = GS_defined_as, summer_period = summer_defined_as,
           Years_included = years_inc, Data_to = as.character(data_to)) %>%
    select(year, Station_name, Station_number, gs_decile, gs_sum_rain,
           summer_decile, summer_sum_rain, GS_period, summer_period, Years_included, Data_to)
  
  p <- ggplot(season_rain, aes(x = year, y = sum_rain_season, fill = season)) +
    geom_col() +
    labs(title = "Sum of rain per year",
         subtitle = paste0("Station name: ", station_name),
         caption = paste0("Years included: ", years_inc, ". GS defined as: ", GS_defined_as,
                          ". Data to: ", data_to),
         y = "sum of rain")
  
  # same file names as before, saved in the climate folder
  write_csv(GS_decile_table_df,     file.path(climate_dir, paste0("GS_decile_table_df_", num, ".csv")))
  write_csv(summer_decile_table_df, file.path(climate_dir, paste0("summer_decile_table_df_", num, ".csv")))
  write_csv(Year_with_Deciles,      file.path(climate_dir, paste0("Year_with_Deciles_", num, ".csv")))
  write_csv(df_all_date,            file.path(climate_dir, paste0("Daily_met_file_season types_", num, ".csv")))
  ggsave(plot = p, filename = file.path(climate_dir, paste0("plot1_sum_rain_per_yr_summer_GS", num, ".png")),
         width = 20, height = 12, units = "cm")
  
  message("done: ", station_name, " (", num, ")")
  Year_with_Deciles
}

results <- map(set_names(stations$Number), run_station)

# CHECK: the last two years for each station
bind_rows(results) %>%
  filter(year >= 2025) %>%
  select(Station_name, year, gs_sum_rain, gs_decile, summer_sum_rain, summer_decile, Data_to)
