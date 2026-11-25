# 04a_Neat_shapefiles_Wynarka_Mervs_West_26.R
# Copies the sampling points shapefiles into one neat folder:
# correct projection, standard columns, standard names. Shapefiles only.
library(tidyverse)
library(sf)
library(ggspatial)

# ---- A. SETTINGS (change per site / year) ----
root      <- "H:/Output-1"
site_dir  <- "3.Wynarka_Mervs_West"
site_name <- "Wynarka_Mervs_West"
yy        <- "26"
epsg      <- 7854                      # Walpeup, Crystal Brook, Wynarka = 7854; Wharminda = 7853

samp_dir <- file.path(root, site_dir, "4.Sampling", "2.InSeason", yy)
out_dir  <- file.path(samp_dir, "shapefile_neat_jackie")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# one row per event. shp_in is relative to the SITE folder (root/site_dir).
# NA = no points file of its own; use the points from the event named in copy_from
# NOTE (25 Nov 2026): 2026 Establishment points are NOT the same places as 2025 (control strip moved west), ids are different each year.
#   Establishment uses the 7.In_Season_data file (visited = Yes, counts typed in): 96 pts = 8 treatments x 12, fid 0-97 (95, 96 missing)
#   Biomass_flowering 2026 = the same 28 points as the 2025 flowering plan (checked: 28 of 28 ids match), so the 2025 shapefile is reused.     # NEW
events <- tribble(
  ~event,                ~date,        ~shp_in,                                                                                                       ~copy_from,
  "Pre_season_Sampling", "2026-03-24",           "4.Sampling/2.InSeason/26/1.Pre_plant/Soil/Centered/WYN_MER_PrePlant_transects_Actual_Centered_7854.shp",       NA,   # 30 pts, soil date still to find
  "Establishment",       "2026-06-03", "7.In_Season_data/26/Emergence/Wynarka_Mervs_Emergence_2026.shp",                                              NA,   # 96 pts, EPSG 32754, converted to 7854
  "Biomass_flowering",   "2026-09-10", "7.In_Season_data/25/Biomass/Actual/Cleaned/Wynarka_Biomass_2025_7854.shp",                                    NA    # NEW: 2025 flowering plan, same 28 points
)

# ---- B. FUNCTIONS ----
# find the id column whatever it is called, and stop if there isn't one
get_pt_id <- function(x) {
  cand <- intersect(c("pt_id", "PT_ID", "Pt_id", "id", "Id", "ID", "Point", "point", "fid", "FID"), names(x))
  if (length(cand) == 0) stop("No id column found. Columns are: ", paste(names(x), collapse = ", "))
  as.integer(x[[cand[1]]])
}

read_pts <- function(shp_in, event, date) {
  x <- st_read(file.path(root, site_dir, shp_in), quiet = TRUE)     # shp_in is the path from the site folder
  message("\n", event, ": columns = ", paste(names(x), collapse = ", "))
  message(event, ": CRS in = ", st_crs(x)$input, " | n points = ", nrow(x))
  if (is.na(st_crs(x))) stop(event, ": shapefile has no CRS (.prj missing)")
  n_in <- nrow(x)
  x <- st_cast(st_zm(x), "POINT", warn = FALSE)     # MULTIPOINT and POINT files both become POINT
  stopifnot(nrow(x) == n_in)                        # one point per row
  x <- st_transform(x, epsg)
  tibble(pt_id = get_pt_id(x), event = event, year = 2000 + as.integer(yy),
         date_coll = as.Date(date)) %>%     # shapefile names max 10 characters
    st_sf(geometry = st_geometry(x))
}

# ---- C. BUILD THE POINTS, THEN WRITE ----
pts <- list()
for (i in seq_len(nrow(events))) {
  ev <- events$event[i]
  if (!is.na(events$shp_in[i])) {
    pts[[ev]] <- read_pts(events$shp_in[i], ev, events$date[i])
  } else {
    pts[[ev]] <- pts[[events$copy_from[i]]] %>%
      mutate(event = ev, date_coll = as.Date(events$date[i]))
  }
}

for (ev in names(pts)) {
  f <- file.path(out_dir, paste0(site_name, "_", ev, "_", yy, "_pts_", epsg, ".shp"))
  st_write(pts[[ev]], f, delete_layer = TRUE, quiet = TRUE)
  message("written: ", f)
}

# ---- D. CHECKS AND SUMMARY ----
for (ev in names(pts)) {
  stopifnot(!anyDuplicated(pts[[ev]]$pt_id))
  stopifnot(st_crs(pts[[ev]])$epsg == epsg)
}
# NEW: for this site the 2026 flowering points are the 2025 points (ids up to 145), so they are NOT inside the 2026 Establishment ids (fid 0-97). Report only.
if ("Biomass_flowering" %in% names(pts))
  message("flowering ids not in the Establishment ids: ", sum(!pts$Biomass_flowering$pt_id %in% pts$Establishment$pt_id),
          " of ", nrow(pts$Biomass_flowering), " (expected for this site)")

tibble(Site = site_name, Year = 2000 + as.integer(yy), Event = names(pts),
       Date_collected = events$date, n_points = map_int(pts, nrow),
       shp_target = paste0(site_name, "_", names(pts), "_", yy, "_pts_", epsg, ".shp"))

# ---- E. ALL SAMPLING LOCATIONS IN ONE FILE (shapefile + csv) ----
csv_dir <- out_dir        # change this if you want the csv somewhere other than the shapefile folder

all_pts <- bind_rows(pts) %>%
  mutate(x = st_coordinates(geometry)[, 1],
         y = st_coordinates(geometry)[, 2],
         comment = case_when(
           event == "Establishment"     ~ "pt_id = fid from the shapefile (0-97, 95 and 96 missing); not the same points as 2025",
           event == "Biomass_flowering" ~ "Same 28 points as the 2025 flowering plan (2025 ids); not the 2026 Establishment points",     # NEW
           TRUE ~ ""))

all_name <- paste0(site_name, "_All_sampling_locations_", yy, "_", epsg)

st_write(all_pts, file.path(out_dir, paste0(all_name, ".shp")), delete_layer = TRUE, quiet = TRUE)

all_pts %>% st_drop_geometry() %>% mutate(epsg = epsg) %>%
  write_csv(file.path(csv_dir, paste0(all_name, ".csv")))

all_pts %>% st_drop_geometry() %>% count(event, name = "n_rows")     # expect Establishment 96, Biomass_flowering 28, Pre_season_Sampling 30


# 04c_Check_plot_neat_shapefiles.R  -- quick visual check of what 04a wrote

neat_dir <- out_dir      # the neat folder set in section A of this script
files    <- list.files(neat_dir, pattern = "_pts_.*\\.shp$", full.names = TRUE)
pts      <- map(files, st_read, quiet = TRUE) %>% bind_rows()

# the counts to compare with your ArcMap map
pts %>% st_drop_geometry() %>% count(event, name = "n_points")

# legend text with the number of samples and the date for each event
ev_codes  <- c("Pre_season_Sampling", "Establishment", "Biomass_flowering", "Harvest")
ev_names  <- c("Pre-season soil", "Establishment", "Biomass at flowering", "Harvest")
ev_names  <- ev_names[ev_codes %in% pts$event]     # keep only the events that exist this year
ev_codes  <- ev_codes[ev_codes %in% pts$event]

summ <- pts %>% st_drop_geometry() %>% group_by(event) %>%
  summarise(n = n_distinct(pt_id), date = format(first(date_coll), "%d %b %Y"), .groups = "drop")
date_txt   <- summ$date[match(ev_codes, summ$event)]
leg_labels <- setNames(paste0(ev_names, "\n n = ", summ$n[match(ev_codes, summ$event)],
                              " points", ifelse(is.na(date_txt), "", paste0(", ", date_txt))), ev_codes)

pts_plot <- pts %>% mutate(event = factor(event, levels = ev_codes)) %>% arrange(desc(event))

p <- ggplot() +
  geom_sf(data = pts_plot, aes(shape = event, size = event, colour = event), stroke = 0.7) +
  scale_shape_manual(values  = c(Pre_season_Sampling = 18, Establishment = 16, Biomass_flowering = 17, Harvest = 0),
                     labels = leg_labels, name = "Sampling event") +
  scale_size_manual(values   = c(Pre_season_Sampling = 3, Establishment = 1.8, Biomass_flowering = 3, Harvest = 5),
                    labels = leg_labels, name = "Sampling event") +
  scale_colour_manual(values = c(Pre_season_Sampling = "darkorange", Establishment = "black", Biomass_flowering = "blue", Harvest = "grey20"),
                      labels = leg_labels, name = "Sampling event") +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.45))) +
  scale_y_continuous(expand = expansion(mult = c(0.06, 0.03))) +
  coord_sf(clip = "off") +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(-3.5, "cm"), pad_y = unit(1.6, "cm"),
                         height = unit(1.2, "cm"), width = unit(1.2, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 8)) +
  annotation_scale(location = "br", width_hint = 0.3,
                   pad_x = unit(-4.5, "cm"), pad_y = unit(0.6, "cm")) +
  labs(title = paste0(str_replace_all(site_name, "_", " "), " 20", yy, ": sampling locations"),
       subtitle = paste0(nrow(pts), " sampling locations in total"),
       caption = paste0("No harvest data yet. Map made ", format(Sys.Date(), "%d %b %Y"))) +     # NEW: harvest note as for site 2
  theme_void() +
  theme(legend.position = "right",
        legend.justification = "top",
        legend.key.height = unit(1.1, "cm"),
        legend.key.width = unit(0.9, "cm"),
        legend.title = element_text(face = "bold", size = 9),
        legend.text = element_text(size = 8, lineheight = 1.1),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 8, colour = "grey30", hjust = 1),
        plot.caption.position = "plot",
        plot.margin = margin(8, 8, 8, 8))
p

ggsave(file.path(neat_dir, paste0(site_name, "_sampling_locations_", yy, ".png")), p,
       width = 7, height = 8, dpi = 200, bg = "white")
