# 04a_Neat_shapefiles_Crystal_Brook_Brians_House_26.R
# Copies the sampling points shapefiles into one neat folder:
# correct projection, standard columns, standard names. Shapefiles only.
library(tidyverse)
library(sf)
library(ggspatial)

# ---- A. SETTINGS (change per site / year) ----
root      <- "H:/Output-1"
site_dir  <- "2.Crystal_Brook_Brians_House"
site_name <- "Crystal_Brook_Brians_House"
yy        <- "26"
epsg      <- 7854                      # Walpeup, Crystal Brook, Wynarka = 7854; Wharminda = 7853

samp_dir <- file.path(root, site_dir, "4.Sampling", "2.InSeason", yy)
out_dir  <- file.path(samp_dir, "shapefile_neat_jackie")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# one row per event. shp_in is relative to the SITE folder (root/site_dir).
# NA = no points file of its own; use the points from the event named in copy_from
# NOTE (25 Nov 2026): 2026 Harvest has no shapefile. 7.In_Season_data/26/Harvest/BHO_Harvest_2026.xlsx     # NEW
#   has X, Y (EPSG 7854) for 120 pts, identical to the Establishment points (within 1 mm), so when     # NEW
#   harvest data arrives add this row to the table below:     # NEW
#   "Harvest", "<date>", NA, "Establishment"     # NEW
#   The xlsx has no date and no cut size (rows, length, spacing), those need to come from Enqi.     # NEW
events <- tribble(
  ~event,                ~date,        ~shp_in,                                                                                                          ~copy_from,
  "Pre_season_Sampling", NA,           "4.Sampling/2.InSeason/26/1.Pre_plant/Soil/Centered/CRY_BHO_Pre_season_Soil_Sampling_Actual_Centered_7854.shp",   NA,   # soil date still to find
  "Establishment",       "2026-06-11", "4.Sampling/2.InSeason/26/Emergence/Actual/BHO_Emergence_2026.shp",                                               NA,   # merged 120 pts, EPSG 7854
  "Biomass_flowering",   "2026-09-09", "7.In_Season_data/25/Biomass/Sampling_Plan_1/BHO_sampled_points_7854.shp",                                        NA    # ASSUMED same 20 pts as the 2025 plan
)

# ---- B. FUNCTIONS ----
# find the id column whatever it is called, and stop if there isn't one
get_pt_id <- function(x) {
  cand <- intersect(c("pt_id", "PT_ID", "Pt_id", "id", "Id", "ID", "Point", "point"), names(x))
  if (length(cand) == 0) stop("No id column found. Columns are: ", paste(names(x), collapse = ", "))
  as.integer(x[[cand[1]]])
}

read_pts <- function(shp_in, event, date) {
  x <- st_read(file.path(root, site_dir, shp_in), quiet = TRUE)     # shp_in is the path from the site folder
  message("\n", event, ": columns = ", paste(names(x), collapse = ", "))
  message(event, ": CRS in = ", st_crs(x)$input, " | n points = ", nrow(x))
  if (is.na(st_crs(x))) stop(event, ": shapefile has no CRS (.prj missing)")
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
# flowering points should be within the establishment points
stopifnot(all(pts$Biomass_flowering$pt_id %in% pts$Establishment$pt_id))

tibble(Site = site_name, Year = 2000 + as.integer(yy), Event = names(pts),
       Date_collected = events$date, n_points = map_int(pts, nrow),
       shp_target = paste0(site_name, "_", names(pts), "_", yy, "_pts_", epsg, ".shp"))

# ---- E. ALL SAMPLING LOCATIONS IN ONE FILE (shapefile + csv) ----
csv_dir <- out_dir        # change this if you want the csv somewhere other than the shapefile folder

all_pts <- bind_rows(pts) %>%
  mutate(x = st_coordinates(geometry)[, 1],
         y = st_coordinates(geometry)[, 2],
         comment = ifelse(event == "Establishment",
                          paste0("No harvest data yet (checked ", format(Sys.Date(), "%d %b %Y"), ")"), ""))     # NEW

all_name <- paste0(site_name, "_All_sampling_locations_", yy, "_", epsg)

st_write(all_pts, file.path(out_dir, paste0(all_name, ".shp")), delete_layer = TRUE, quiet = TRUE)

all_pts %>% st_drop_geometry() %>% mutate(epsg = epsg) %>%
  write_csv(file.path(csv_dir, paste0(all_name, ".csv")))

all_pts %>% st_drop_geometry() %>% count(event, name = "n_rows")     # expect Biomass_flowering 20, Establishment 120, plus the soil points


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
       caption = paste0("No harvest data yet. Map made ", format(Sys.Date(), "%d %b %Y"))) +     # NEW
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
