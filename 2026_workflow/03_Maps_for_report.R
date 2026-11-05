# 03_Maps_for_report.R  -- Part 1: zones map + strips map (R replacement for ArcMap)
library(tidyverse)
library(sf)
library(readxl)
library(ggspatial)
library(ggnewscale)   # NEW: lets the strips map have two fill legends (zones + treatments)
library(ozmaps)       # NEW: state outlines for the location map (no basemap tiles)
library(patchwork)    # NEW: Australia inset on the location map


root    <- "H:/Output-1"
meta    <- file.path(root, "0.Site-info", "names of treatments per site 2025 metadata and other info.xlsx")
out_dir <- "H:/Output-1/Jackie notes processing etc/R_markdown_reports/2026_outputs"
map_dir <- file.path(out_dir, "maps")
dir.create(map_dir, showWarnings = FALSE, recursive = TRUE)

run_ids <- 8        # NEW: start with Walpeup; change to 1:8 once it looks right

sites <- tibble(
  id          = 1:8,
  site_number = c("1.Walpeup_MRS125", "2.Crystal_Brook_Brians_House", "3.Wynarka_Mervs_West",
                  "4.Wharminda_Woodys", "5.Walpeup_Gums", "6.Crystal_Brook_Randals",
                  "7.Wharminda_Bonanza", "8.Wynarka_Tanks"),
  site_name   = c("Walpeup_MRS125", "Crystal_Brook_Brians_House", "Wynarka_Mervs_West",
                  "Wharminda_Woodys", "Walpeup_Gums", "Crystal_Brook_Randals",
                  "Wharminda_Bonanza", "Wynarka_Tanks"),
  zone_field  = c("gridcode", "cluster", "fcl_mdl", "fcl_mdl", "cluster3", "cluster", "cluster", "zone")
)

file_loc <- read_excel(meta, sheet = "file location etc")
zone_det <- read_excel(meta, sheet = "zone_details")
treat_nm <- read_excel(meta, sheet = "treatment names")

get_shp <- function(site_number, var) {
  p <- file_loc %>% filter(Site == site_number, variable == var) %>% pull(`file path`)
  paste0(root, "/", site_number, p[1])
}

zone_lookup <- function(site_number) {
  zl <- zone_det %>%
    filter(Site == site_number) %>%
    transmute(zone  = as.integer(`zone names`),
              name  = trimws(sub("^[0-9]+\\s*=\\s*", "", `zone label names`)),
              hex   = `Hex Code`,
              label = paste0(zone, ": ", name))
  ord <- match(zl$name, c("Dune", "Transition", "Swale"))
  if (!anyNA(ord)) zl[order(ord), ] else zl[order(zl$zone), ]
}

map_theme <- list(
  scale_y_continuous(expand = expansion(mult = c(0.16, 0.02))),   
  annotation_north_arrow(location = "bl", which_north = "true",    
                         pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"),
                         height = unit(1.1, "cm"), width = unit(1.1, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 8)),
  annotation_scale(location = "bl", width_hint = 0.2,              
                   pad_x = unit(1.8, "cm"), pad_y = unit(0.45, "cm")),
  theme_void(),
  theme(legend.position = "right",
        plot.title = element_text(size = 11, face = "bold"),
        plot.margin = margin(5, 20, 5, 5))                         
)

# ---- 1. zones map ----
make_zones_map <- function(site_number, site_name, zone_field) {
  zl    <- zone_lookup(site_number)
  zones <- st_read(get_shp(site_number, "location of zone shp"), quiet = TRUE) %>%
    mutate(zone = as.integer(.data[[zone_field]])) %>%
    left_join(zl, by = "zone")
  
  p <- ggplot(zones) +
    geom_sf(aes(fill = label), colour = "grey25", linewidth = 0.3) +
    scale_fill_manual(values = setNames(zl$hex, zl$label), breaks = zl$label,
                      name = "Zone", na.value = "grey85") +
    map_theme
  ggsave(file.path(map_dir, paste0(site_name, "_zones_map.png")), p,
         width = 8, height = 5, dpi = 200, bg = "white")   # NEW: wider
}

# ---- 2. strips map (strip labels run along the strips, zone boundaries dashed) ----
make_strips_map <- function(site_number, site_name, zone_field) {
  tn     <- treat_nm %>% filter(Site == site_number) %>%
    transmute(treat_desc = `Treatment Name`, hex = Hex, ord = `Order in Paddock`) %>%
    arrange(ord)
  strips <- st_read(get_shp(site_number, "trial.plan"), quiet = TRUE)
  zl     <- zone_lookup(site_number)                                  # NEW
  zones  <- st_read(get_shp(site_number, "location of zone shp"), quiet = TRUE) %>%
    st_transform(st_crs(strips)) %>%
    mutate(zone = as.integer(.data[[zone_field]])) %>%      # NEW
    left_join(zl, by = "zone")                              # NEW
  
  # label position + angle along each strip's long axis
  lab <- strips %>% filter(treat_desc != "Buffer") %>%
    rowwise() %>%
    mutate(
      cen   = list(st_coordinates(st_centroid(geometry))),
      rect  = list(st_coordinates(st_minimum_rotated_rectangle(geometry))[, 1:2]),
      s1    = sqrt(sum((rect[2, ] - rect[1, ])^2)),
      s2    = sqrt(sum((rect[3, ] - rect[2, ])^2)),
      dv    = list(if (s1 >= s2) rect[2, ] - rect[1, ] else rect[3, ] - rect[2, ]),
      angle = atan2(dv[2], dv[1]) * 180 / pi,
      x     = cen[1], y = cen[2]
    ) %>%
    ungroup() %>% st_drop_geometry() %>%
    mutate(angle = ifelse(angle > 90, angle - 180, ifelse(angle <= -90, angle + 180, angle))) %>%
    select(treat_desc, x, y, angle)
  
  cols <- c(setNames(tn$hex, tn$treat_desc), Buffer = "#FFFFFF")
  
  p <- ggplot() +
    geom_sf(data = zones, aes(fill = label), colour = NA, alpha = 0.45) +           # NEW: zones coloured behind the strips
    scale_fill_manual(values = setNames(zl$hex, zl$label), breaks = zl$label,
                      name = "Zones", na.value = "grey85",
                      guide = guide_legend(order = 2)) +
    new_scale_fill() +                                                  # NEW: second, separate fill legend
    geom_sf(data = strips, aes(fill = treat_desc), colour = "grey30", linewidth = 0.2) +
    geom_text(data = lab, aes(x = x, y = y, label = treat_desc, angle = angle), size = 2.6) +
    scale_fill_manual(values = cols, breaks = tn$treat_desc, name = "Treatment",
                      guide = guide_legend(order = 1)) +
    map_theme
  ggsave(file.path(map_dir, paste0(site_name, "_strips_map.png")), p,
         width = 8, height = 5, dpi = 200, bg = "white")
}

# ---- 3. sampling points map (pre-sowing soil cores; plant sampling points to be added) ----
report_year <- 2026
soil_dir    <- "H:/Output-1/0.Site-info/soil_N_H20_data/"
# plant_shp <- NULL    # later: path to the plant sampling points shapefile (Establishment / Biomass / Maturity)

make_sampling_map <- function(site_number, site_name, zone_field) {
  tn     <- treat_nm %>% filter(Site == site_number) %>%
    transmute(treat_desc = `Treatment Name`, hex = Hex, ord = `Order in Paddock`) %>%
    arrange(ord)
  strips <- st_read(get_shp(site_number, "trial.plan"), quiet = TRUE)
  zl     <- zone_lookup(site_number)
  zones  <- st_read(get_shp(site_number, "location of zone shp"), quiet = TRUE) %>%
    st_transform(st_crs(strips)) %>%
    mutate(zone = as.integer(.data[[zone_field]])) %>%
    left_join(zl, by = "zone")
  cols   <- c(setNames(tn$hex, tn$treat_desc), Buffer = "#FFFFFF")
  
  # soil coring points: .env$ stops the column being compared with itself
  d <- read.csv(paste0(soil_dir, "All_sites_2024_2026_N_and_H2O_profile.csv")) %>%
    filter(site_number == .env$site_number, year == report_year) %>%
    distinct(sample_id, .keep_all = TRUE)
  crs_in <- if (all(abs(d$longitude) <= 180)) 4326 else unique(d$epsg)[1]
  pts <- st_as_sf(d, coords = c("longitude", "latitude"), crs = crs_in) %>%
    st_transform(st_crs(strips))
  
  p <- ggplot() +
    geom_sf(data = zones, aes(fill = label), colour = NA, alpha = 0.3) +
    scale_fill_manual(values = setNames(zl$hex, zl$label), breaks = zl$label,
                      name = "Zones", na.value = "grey85",
                      guide = guide_legend(order = 2)) +
    new_scale_fill() +
    geom_sf(data = strips, aes(fill = treat_desc), colour = "grey30", linewidth = 0.2, alpha = 0.8) +
    scale_fill_manual(values = cols, breaks = tn$treat_desc, name = "Treatment",
                      guide = guide_legend(order = 1)) +
    geom_sf(data = pts, aes(shape = "Pre-sowing soil core"), size = 1.8, colour = "black", fill = "white") +
    scale_shape_manual(values = c("Pre-sowing soil core" = 21), name = "Sampling points",
                       guide = guide_legend(order = 3)) +
    map_theme
  ggsave(file.path(map_dir, paste0(site_name, "_sampling_map.png")), p,
         width = 8, height = 5, dpi = 200, bg = "white")
}

# ---- 4. site location map (met-station town only, with Australia inset) ----
towns <- tibble(station = c("Walpeup", "Crystal Brook", "Wynarka", "Wharminda"),
                lat     = c(-35.14, -33.35, -35.09, -33.97),
                lon     = c(142.02, 138.21, 139.90, 136.25))
site_station <- c("Walpeup", "Crystal Brook", "Wynarka", "Wharminda",
                  "Walpeup", "Crystal Brook", "Wharminda", "Wynarka")

make_location_map <- function(site_number, site_name, zone_field) {
  this_station <- site_station[match(site_number, sites$site_number)]
  states <- ozmap_states
  tw     <- st_as_sf(towns, coords = c("lon", "lat"), crs = 4326) %>% st_transform(st_crs(states)) %>%
    mutate(focus = station == this_station)
  xl <- c(134, 144); yl <- c(-36.8, -32.5)
  
  main <- ggplot() +
    geom_sf(data = states, fill = "grey95", colour = "grey55", linewidth = 0.3) +
    geom_sf(data = tw %>% filter(!focus), size = 2, colour = "grey30") +
    geom_sf(data = tw %>% filter(focus),  size = 4, colour = "#C0392B") +
    geom_sf_text(data = tw, aes(label = station, fontface = ifelse(focus, "bold", "plain")),
                 nudge_y = 0.2, size = 3.2) +
    coord_sf(xlim = xl, ylim = yl, expand = FALSE) +
    annotation_north_arrow(location = "bl", which_north = "true",              # NEW: north arrow above the scale bar
                           pad_x = unit(0.4, "cm"), pad_y = unit(1.0, "cm"),
                           height = unit(1.1, "cm"), width = unit(1.1, "cm"),
                           style = north_arrow_fancy_orienteering(text_size = 8)) +
    annotation_scale(location = "bl", width_hint = 0.25) +
    theme_void() +
    theme(panel.border = element_rect(colour = "grey40", fill = NA))
  
  inset <- ggplot() +
    geom_sf(data = states, fill = "grey90", colour = "grey55", linewidth = 0.2) +
    annotate("rect", xmin = xl[1], xmax = xl[2], ymin = yl[1], ymax = yl[2],
             fill = NA, colour = "#C0392B", linewidth = 0.6) +
    theme_void()
  
  p <- main + inset_element(inset, left = 0.70, bottom = 0.62, right = 0.99, top = 0.99)
  ggsave(file.path(map_dir, paste0(site_name, "_location_map.png")), p,
         width = 7, height = 5, dpi = 200, bg = "white")
}

# ---- run ----
for (i in run_ids) {
  s <- sites[i, ]
  for (f in c("make_zones_map", "make_strips_map", "make_sampling_map", "make_location_map")) {
    tryCatch({
      get(f)(s$site_number, s$site_name, s$zone_field)
      message("done: ", f, " - ", s$site_name)
    }, error = function(e) message("SKIPPED: ", f, " - ", s$site_name, ": ", conditionMessage(e)))
  }
}

