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
#install.packages("ozmaps")
library(sf)
library(tidyverse)
library(ggspatial)
library(patchwork)
library(ggnewscale)
library(ozmaps)

# ---- Site settings ----
# site_report <- "Walpeup"
# site_number <- "1.Walpeup_MRS125"
# site_name   <- "Walpeup_MRS125"

# BON
# site_report <- "Wharminda"
# site_number <- "7.Wharminda_Bonanza"
# site_name   <- "Wharminda_Bonanza"

# TAN
site_report <- "Wynarka"
site_number <- "8.Wynarka_Tanks"
site_name   <- "Wynarka_Tanks"

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
  mutate(`Treatment Name` = str_trim(`Treatment Name`)) %>% 
  distinct(treat, .keep_all = TRUE) %>%
  mutate(label = case_when(
    treat == "C" & any(!is.na(`Amedment rate`)) ~ "Control (-Tillage -Lime)",
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

# ---- Treatments: label angle from strip orientation ----
strip_angle <- function(geom) {
  rect <- st_minimum_rotated_rectangle(geom)
  xy   <- st_coordinates(rect)[1:3, 1:2]
  e1   <- xy[2, ] - xy[1, ]
  e2   <- xy[3, ] - xy[2, ]
  e    <- if (sum(e1^2) > sum(e2^2)) e1 else e2   # long edge of the strip
  a    <- atan2(e[2], e[1]) * 180 / pi
  if (a >  90) a <- a - 180                        # keep text right way up
  if (a < -90) a <- a + 180
  a
}

strips$label_angle <- sapply(st_geometry(strips), strip_angle)

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
                    na.value = "#D9D9D9",
                    labels = function(x) ifelse(is.na(x), "4 = not described", x),
                    guide = guide_legend(order = 2)) +
  new_scale_fill() +
  # treatment strips on top
  geom_sf(data = strips, aes(fill = label), colour = "black", linewidth = 0.2) +
  scale_fill_manual(values = treat_cols, name = NULL,
                    guide = guide_legend(order = 1)) +
  # strip labels, angled to match strip direction (skip buffers)
  geom_sf_text(data = filter(strips, label != "Buffer"),
               aes(label = label, angle = label_angle), size = 2.5) +
  # paddock outline
  geom_sf(data = boundary, fill = NA, colour = "black", linewidth = 0.3) +
  # scale bar & north arrow
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  theme(legend.position = "right") +
  # caption – BON only; delete or edit for other sites
  #labs(caption = "Note: zone shapefile contains zones 1–4; no description recorded for zone 4, and no zone raster in FINAL folder to check against.") +
  #theme(plot.caption = element_text(hjust = 0, size = 8))
  # caption – TAN only; delete or edit for other sites
  labs(caption = "Note: zone shapefile contains zones 1–3; no description recorded for zones, and no details in zone raster in FINAL folder.") +
  theme(plot.caption = element_text(hjust = 0, size = 8))

map_strips


# ---- Map 1: save ----
map_dir <- file.path(headDir, "5.Trial_Plan/FINAL-Trial-Plan/GIS/PNG")
dir.create(map_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(map_dir, "Strips Jackie.png"), map_strips,
       width = 29.7, height = 21, units = "cm", dpi = 300)


# ---- Map 2: zones within paddock boundary ----
map_zones <- ggplot() +
  geom_sf(data = zones, aes(fill = zone), colour = NA) +
  scale_fill_manual(values = zone_cols, name = "Zones",
                    na.value = "#D9D9D9",
                    labels = function(x) ifelse(is.na(x), "4 = not described", x)) +
  geom_sf(data = boundary, fill = NA, colour = "black", linewidth = 0.3) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  theme(legend.position = "right") +
  # caption – BON only; delete or edit for other sites
  #labs(caption = "Note: zone shapefile contains zones 1–4; no description recorded for zone 4, and no zone raster in FINAL folder to check against.") +
  #theme(plot.caption = element_text(hjust = 0, size = 8))
  # caption – TAN only; delete or edit for other sites
  labs(caption = "Note: zone shapefile contains zones 1–3; no description recorded for zones, and no details in zone raster in FINAL folder.") +
  theme(plot.caption = element_text(hjust = 0, size = 8))

map_zones

# ---- Map 2: save ----
zone_map_dir <- file.path(headDir, "3.Covariates/6.Clusters_Zones/FINAL")

ggsave(file.path(zone_map_dir, "Zone Jackie.png"), map_zones,
       width = 29.7, height = 21, units = "cm", dpi = 300)


# ---- Map 3: met station locations ----
met_file <- paste0(dir, "/work/Output-1/0.Site-info/met location of paddocksV2.csv")
met_raw  <- read_csv(met_file, show_col_types = FALSE)

this_met <- met_raw %>% filter(`site name` == site_number) %>% pull(`met station`)
if (length(this_met) == 0) stop("Site not in met location file: ", site_number)

met_all <- met_raw %>% 
  filter(Project_stream == "output1") %>% 
  distinct(`met station`, .keep_all = TRUE) %>% 
  mutate(current = `met station` == this_met) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4283) %>% 
  st_transform(3112)

met_main <- met_all %>% filter(`met station` != "Riverdale")   # WA site – inset only

states  <- ozmap_states %>% st_transform(3112)
main_bb <- st_bbox(c(xmin = 135.2, xmax = 143.3, ymin = -38.0, ymax = -32.2),
                   crs = st_crs(4283)) %>% 
  st_as_sfc() %>% 
  st_transform(3112) %>% 
  st_bbox()           


# ---- Map 3: reference towns ----
towns <- data.frame(
  town = c("Adelaide", "Mildura", "Mount Gambier"),
  lat  = c(-34.93, -34.19, -37.83),
  lon  = c(138.60, 142.16, 140.78)
) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4283) %>% 
  st_transform(3112)

# ---- Map 3: main map ----
map_main <- ggplot() +
  geom_sf(data = states, fill = "grey96", colour = "grey60", linewidth = 0.3) +
  # towns
  geom_sf(data = towns, shape = 21, fill = "white", colour = "grey30", size = 1.8) +
  geom_sf_text(data = towns, aes(label = town),
               nudge_y = -15000, colour = "grey30", fontface = "italic", size = 3) +
  # met stations
  geom_sf(data = met_main, aes(colour = current), size = 3) +
  scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "red"), guide = "none") +
  geom_sf_text(data = met_main, aes(label = `met station`),
               nudge_y = 18000, fontface = "bold", size = 3.5) +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = main_bb[c("xmin", "xmax")], ylim = main_bb[c("ymin", "ymax")],
           expand = FALSE) +
  labs(caption = "Site names that are displayed are mapped to closest met station to protect landholders' privacy.") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#DCEBF5", colour = "black"),
        plot.caption = element_text(hjust = 0, face = "bold", size = 9))

# ---- Map 3: Australia inset ----
map_inset <- ggplot() +
  geom_sf(data = states, fill = "white", colour = "black", linewidth = 0.2) +
  geom_sf(data = met_all, size = 2) +
  annotation_custom(grid::textGrob("All project sites", x = 0.05, y = 0.1,
                                   hjust = 0, gp = grid::gpar(fontsize = 11))) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#BEE3F8", colour = "black"))

map_location <- map_main +
  inset_element(map_inset, left = 0.01, bottom = 0.01, right = 0.3, top = 0.45,
                align_to = "panel")

map_location

# ---- Map 3: save ----
loc_map_dir <- file.path(headDir, "1.Paddock_Boundary")

ggsave(file.path(loc_map_dir, "site location map.png"), map_location,
       width = 29.7, height = 21, units = "cm", dpi = 300)
