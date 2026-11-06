################################################################################
### 05_Point_sampling_results.R
### SECTION 1: Setup, read combined long files, join locations, strips, zones
################################################################################

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(purrr)
library(sf)
library(ggplot2)
# ---- Site lookup (one row per site) -------------------------------------------
site_lookup <- data.frame(                                                  
  id = 1:8,                                                                 
  site_number = c("1.Walpeup_MRS125", "2.Crystal_Brook_Brians_House",        
                  "3.Wynarka_Mervs_West", "4.Wharminda_Woodys",              
                  "5.Walpeup_Gums", "6.Crystal_Brook_Randals",               
                  "7.Wharminda_Bonanza", "8.Wynarka_Tanks"),                 
  site_name   = c("Walpeup_MRS125", "Crystal_Brook_Brians_House",            
                  "Wynarka_Mervs_West", "Wharminda_Woodys",                  
                  "Walpeup_Gums", "Crystal_Brook_Randals",                   
                  "Wharminda_Bonanza", "Wynarka_Tanks"),                     
  zone_field  = c("gridcode", "cluster", "fcl_mdl", "fcl_mdl",               
                  "cluster3", "cluster", "cluster", "zone"),                 
  epsg        = c(7854, 7854, 7854, 7853, 7854, 7854, 7853, 7854),          #  (Wharminda = 7853)
  stringsAsFactors = FALSE                                                  
)                                                                            






# ---- Choose the site: change this ONE number (1 to 8) ------------------------
site_id <- 1                                                                 

site        <- site_lookup[site_lookup$id == site_id, ]                      
stopifnot(nrow(site) == 1)                                                  
site_number <- site$site_number                                              
site_name   <- site$site_name                                                
zone_field  <- site$zone_field                                               
epsg        <- site$epsg                                                     

years       <- c("25", "26")
join_zones  <- TRUE          # zones are not used by the t-tests/plots, set FALSE to skip

# ---- Paths ------------------------------------------------------------------
root          <- "H:/Output-1/"     # <- check: assumes H: maps to the old work/Output-1 folder
headDir       <- paste0(root, site_number)
metadata_file <- paste0(root, "0.Site-info/names of treatments per site 2025 metadata and other info.xlsx")

# ---- Read results and locations (all years, all events) ---------------------
results <- map_dfr(years, function(yy) {
  read_excel(file.path(headDir, "7.In_Season_data", yy, "data_neat_jackie",
                       paste0(site_name, "_All_sampling_results_", yy, ".xlsx")),
             sheet = "data")
})

locs <- map_dfr(years, function(yy) {
  read_csv(file.path(headDir, "4.Sampling/2.InSeason", yy, "shapefile_neat_jackie",
                     paste0(site_name, "_All_sampling_locations_", yy, "_", epsg, ".csv")),
           show_col_types = FALSE)
}) %>%
  dplyr::select(year, event, pt_id, x, y, loc_comment = comment)

# ---- Attach coordinates -----------------------------------------------------
pts <- results %>% left_join(locs, by = c("year", "event", "pt_id"))

# CHECK: both should be 0
sum(is.na(pts$x))
nrow(pts) - nrow(results)

# ---- Make spatial ------------------------------------------------------------
pts_sf <- st_as_sf(pts, coords = c("x", "y"), crs = epsg, remove = FALSE)

# ---- Strips (treatment) and zones, paths come from the metadata workbook -----
file_loc <- read_excel(metadata_file, sheet = "file location etc") %>%
  filter(Site == site_number)

strips <- st_read(paste0(headDir, file_loc %>% filter(variable == "trial.plan") %>% pull("file path")),
                  quiet = TRUE) %>%
  st_transform(epsg)

# STOP AND CHECK: which columns does the strip file have? I am assuming "treat"
names(strips)

strips <- strips %>%
  dplyr::select(any_of(c("treat", "id", "POLY_AREA"))) %>%
  rename(any_of(c(treat_id = "id", strip_ha = "POLY_AREA")))

pts_sf <- st_join(pts_sf, strips)

if (join_zones) {
  zones <- st_read(paste0(headDir, file_loc %>% filter(variable == "location of zone shp") %>% pull("file path")),
                   quiet = TRUE) %>%
    st_transform(epsg)
  names(zones)                      # STOP AND CHECK: zone_field must be one of these
  zones <- zones %>%
    dplyr::select(zone = all_of(zone_field), any_of(c(zone_ha = "POLY_AREA")))
  pts_sf <- st_join(pts_sf, zones)
}

# CHECK: a point on a strip edge can match twice, so this should still be 0
nrow(pts_sf) - nrow(results)
# CHECK: points with no treatment (the old scripts set NA to "C" at Mervs only)
pts_sf %>% st_drop_geometry() %>% count(year, event, treat) %>% print(n = Inf)

# ---- Treatment names, order and colours -------------------------------------
strip_names <- read_excel(metadata_file, sheet = "treatment names") %>%
  filter(Site == site_number) %>%
  dplyr::select(treat, `Treatment Name`, `Order in Paddock`, Hex)

pts_df <- pts_sf %>%
  st_drop_geometry() %>%
  left_join(strip_names, by = "treat")

str(pts_df)


################################################################################
### SECTION 2: Strip means and t-tests vs control (all years and variables)
################################################################################

library(broom)

control_code <- "C"
alpha        <- 0.10      # same 90% threshold as before

out_dir <- "H:/Output-1/Jackie notes processing etc/R_markdown_reports/2026_outputs"
stopifnot(dir.exists(out_dir))   # errors if the folder is missing, so nothing is created by accident

run_ttests <- function(d) {
  ctrl <- d %>% filter(treat == control_code) %>% pull(value)
  
  summ <- d %>%
    group_by(treat) %>%
    summarise(n_valid = n(), mean = mean(value), sd = sd(value), median = median(value),
              Q1 = quantile(value, 0.25), Q3 = quantile(value, 0.75),
              min = min(value), max = max(value), .groups = "drop")
  
  if (length(ctrl) < 2) {            # no usable control, so no tests
    return(summ %>% mutate(adj_p_value = NA_real_, Significance = NA_character_))
  }
  
  tt <- d %>%
    filter(treat != control_code) %>%
    group_by(treat) %>%
    filter(n() >= 2) %>%             # a strip with 1 sample cannot be tested (the old "GU B strip" issue)
    group_modify(~ tryCatch(tidy(t.test(.x$value, ctrl)),
                            error = function(e) tibble(p.value = NA_real_))) %>%
    ungroup() %>%
    mutate(adj_p_value = p.adjust(p.value, method = "bonferroni"))
  
  summ %>%
    left_join(tt %>% dplyr::select(treat, adj_p_value), by = "treat") %>%
    mutate(Significance = case_when(treat == control_code ~ "a",
                                    is.na(adj_p_value)    ~ NA_character_,
                                    adj_p_value <= alpha  ~ "b",
                                    TRUE                  ~ "a"))
}

# points outside any strip have no treatment, so they are dropped here (count shown)
sum(is.na(pts_df$treat))

stats_all <- pts_df %>%
  filter(!is.na(value), !is.na(treat)) %>%
  group_by(year, event, variable, units) %>%
  group_modify(~ run_ttests(.x)) %>%
  ungroup() %>%
  left_join(strip_names, by = "treat") %>%
  mutate(analysis.type = paste0(variable, "_Yr_", substr(year, 3, 4)))

print(stats_all %>% dplyr::select(year, variable, treat, n_valid, mean, Significance, adj_p_value),
      n = Inf)

write_csv(stats_all, file.path(out_dir, paste0(site_name, "_pt_sampling_stats_T_test.csv")))

# ---- Flag the unresolved items so they are not forgotten ----------------------
pts_df %>% filter(variable == "Harvest index", value > 50) %>%
  dplyr::select(year, pt_id, treat, value) %>% print(n = Inf)       # the 14 points above 50%
pts_df %>% filter(!is.na(comment)) %>% count(year, event, variable)  # rows carrying a comment


################################################################################
### SECTION 3: Bar plots (mean, Q1-Q3, significance letter vs control)
################################################################################

make_plot <- function(d) {
  d <- d %>% mutate(`Treatment Name` = trimws(`Treatment Name`),
                    `Treatment Name` = reorder(`Treatment Name`, `Order in Paddock`),
                    treat            = reorder(treat, `Order in Paddock`))
  cols <- setNames(d$Hex, as.character(d$`Treatment Name`))
  
  ggplot(d, aes(x = treat, y = mean, fill = `Treatment Name`)) +
    geom_col(alpha = 0.7) +
    geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, colour = "black") +
    geom_text(aes(label = Significance, y = Q3), vjust = -0.5, size = 5, fontface = "bold") +
    labs(caption = "Treatment compared to the control using t-tests with Bonferroni-adjusted p-values, (p \u2264 0.10)",
         x = NULL, fill = NULL,
         y = paste0(unique(d$variable), "\n", unique(d$units))) +
    scale_fill_manual(values = cols) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    theme_minimal() +
    theme(text = element_text(size = 16), axis.title = element_text(size = 16),
          axis.text = element_text(size = 16), plot.caption = element_text(size = 10),
          legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
}

stats_all %>%
  group_split(year, variable) %>%
  walk(function(d) {
    p <- make_plot(d)
    ggsave(file.path(out_dir,
                     paste0(site_name, "_pt_sampling_", d$year[1], "_", gsub(" ", "_", d$variable[1]), "_T_test_plot.png")),
           p, width = 8, height = 6)
  })

