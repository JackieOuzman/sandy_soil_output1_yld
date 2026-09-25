# =============================================================================
# Title:    Site maps – strip trials, sampling locations & site overview
# Author:   Jackie Ouzman
# Created:  2026-09-25
# Purpose:  Recreate ArcMap figures in R: (1) treatment strips over soil zones,
#           (2) plant sampling locations by timing, (3) regional site map
# Inputs:   Paddock boundary, zones, treatment strips, sampling points
#           (shapefile / .gdb); site coordinates
# Outputs:  PNG/PDF maps per site
# Notes:    Site names on regional map are mapped to nearest met station
#           to protect landholder privacy
# =============================================================================

# ---- Libraries ----
#install.packages("ggnewscale")
library(sf)
library(tidyverse)
library(ggspatial)
library(patchwork)
library(ggnewscale)

# ---- Site settings ----
site_report <- "Walpeup"
site_number <- "1.Walpeup_MRS125"
site_name   <- "Walpeup_MRS125"
analysis.yr <- "25"

dir     <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

# ---- Metadata: file locations ----
metadata_path      <- paste0(dir, "/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

file_path_details_1 <- readxl::read_excel(
  paste0(metadata_path, metadata_file_name),
  sheet = "file location etc") %>% 
  filter(Site == site_number)

# ---- Metadata: helper to build full file paths ----
get_path <- function(var) {
  p <- file_path_details_1 %>% 
    filter(variable == var) %>% 
    pull(`file path`)
  p <- gsub("\\\\", "/", p)          # backslashes -> forward slashes
  p <- sub("^/", "", p)              # drop leading slash
  file.path(headDir, p)
}

# ---- Metadata: site-specific zone field (metadata value is unreliable) ----
zone_field <- case_when(
  site_number == "1.Walpeup_MRS125"             ~ "gridcode",
  site_number == "2.Crystal_Brook_Brians_House" ~ "cluster",
  site_number == "3.Wynarka_Mervs_West"         ~ "fcl_mdl",
  site_number == "4.Wharminda_Woodys"           ~ "fcl_mdl",
  site_number == "5.Walpeup_Gums"               ~ "cluster3",
  site_number == "6.Crystal_Brook_Randals"      ~ "cluster",
  site_number == "7.Wharminda_Bonanza"          ~ "cluster", #"DN"
  site_number == "8.Wynarka_Tanks"              ~ "zone",
  TRUE ~ NA_character_
)
if (is.na(zone_field)) stop("No zone field defined for site: ", site_number)

# ---- Read spatial layers ----
boundary <- st_read(get_path("boundary_shapefile"),   quiet = TRUE)
zones    <- st_read(get_path("location of zone shp"), quiet = TRUE)
strips   <- st_read(get_path("trial.plan"),           quiet = TRUE)

# checks (run as needed)
# names(strips); head(st_drop_geometry(strips)); names(zones)

# ---- Treatments: labels & colours ----
treat_meta <- readxl::read_excel(
  paste0(metadata_path, metadata_file_name),
  sheet = "treatment names") %>% 
  filter(Site == site_number) %>% 
  arrange(`Order in Paddock`) %>% 
  mutate(label = case_when(
    treat == "C" ~ "Control (-Tillage -Lime)",
    !is.na(`Amedment rate`) ~ paste0(`Treatment Name`, " (",
                                     readr::parse_number(`Amedment rate`), "t)"),
    TRUE ~ `Treatment Name`))

treat_cols <- c(setNames(treat_meta$Hex, treat_meta$label), "Buffer" = "white")

# ---- Treatments: join to strips ----
strips <- strips %>% 
  left_join(treat_meta %>% select(treat, label), by = "treat") %>% 
  mutate(label = if_else(grepl("^buff", treat_desc, ignore.case = TRUE),
                         "Buffer", label),
         label = factor(label, levels = names(treat_cols)))

count(st_drop_geometry(strips), treat, treat_desc, label)

# ---- Zones: labels & colours ----
zone_meta <- readxl::read_excel(
  paste0(metadata_path, metadata_file_name),
  sheet = "zone_details") %>% 
  filter(Site == site_number)

zone_cols <- setNames(zone_meta$`Hex Code`, zone_meta$`zone label names`)

zones <- zones %>% 
  rename(zone_id = all_of(zone_field)) %>% 
  mutate(zone = factor(as.character(zone_id),
                       levels = as.character(zone_meta$`zone names`),
                       labels = zone_meta$`zone label names`))

count(st_drop_geometry(zones), zone_id, zone)

# ---- Map 1: treatment strips over zones ----
map_strips <- ggplot() +
  # zones underneath
  geom_sf(data = zones, aes(fill = zone), colour = NA) +
  scale_fill_manual(values = zone_cols, name = "Zones",
                    guide = guide_legend(order = 2)) +
  new_scale_fill() +
  # treatment strips on top
  geom_sf(data = strips, aes(fill = label), colour = "black", linewidth = 0.2) +
  scale_fill_manual(values = treat_cols, name = NULL,
                    guide = guide_legend(order = 1)) +
  # rotated strip labels (skip buffers)
  geom_sf_text(data = filter(strips, label != "Buffer"),
               aes(label = label), angle = 90, size = 2.5) +
  # paddock outline
  geom_sf(data = boundary, fill = NA, colour = "black", linewidth = 0.3) +
  # scale bar & north arrow
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  theme(legend.position = "right")

map_strips
