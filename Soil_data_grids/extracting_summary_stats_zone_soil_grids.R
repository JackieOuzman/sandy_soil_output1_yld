
## Get a mean value per zone for the soil grids

rm(list=ls())
library(terra)
library(sf)
library(ggplot2)
library(multcomp)
library(dplyr)
library(tidyr)
library(broom)
library(multcompView)
library(lubridate)
library(readxl)
library(readr)

################################################################################
########################            Define the directory              ##########
################################################################################

# site_number <- "1.Walpeup_MRS125"
# site_name <- "Walpeup_MRS125"

 # site_number <-"2.Crystal_Brook_Brians_House"
 # site_name <-  "Crystal_Brook_Brians_House"

# site_number <- "3.Wynarka_Mervs_West"
# site_name <- "Wynarka_Mervs_West"

# site_number <- "4.Wharminda"
# site_name <- "Wharminda"

# site_number <- "5.Walpeup_Gums"
# site_name <- "Walpeup_Gums"

site_number <- "6.Crystal_Brook_Randals"
site_name <- "Crystal_Brook_Randals"

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"


headDir <- paste0(dir, "/work/Output-1/", site_number)


analysis.yr <- "25"

metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")

metadata_file_name <- "names of treatments per site 2025 metadata and other info_v2.xlsx"

crs_used <- 4326 # Name: WGS 84 (World Geodetic System 1984) Type: Geographic coordinate system (latitude/longitude)
projetion_crs <- 7854 #GDA2020 / MGA Zone 54 (EPSG:7854).



#### Bring in the zone data 
zones_path_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>% 
  filter(Site == site_number)  %>% 
  filter(variable == "location of zone shp") %>% 
  pull("file path")

zones <- st_read(
  paste0(headDir,zones_path_details))

### soil grid paths

## 
soil_grid_path_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "soil grids") %>% 
  filter(Site == site_name)  %>% 
   slice(1) %>% 
   pull("Location")

soil_grid_path_details

boundary_grid_path_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>% 
  filter(Site == site_number)  %>% 
  filter(variable == "boundary_grid_5m") %>% 
  pull("file path")

boundary_grid_path_details

boundary_raster <- rast(paste0(headDir, boundary_grid_path_details))


################################################################################
### Walpeup_MRS125 Grids
# maps_0_10 <- rast(paste0(headDir, soil_grid_path_details, "0-10_maps.tif"))
# maps_40_60 <- rast(paste0(headDir, soil_grid_path_details, "40-60_maps.tif"))
# 
# names(maps_0_10)  <- c("pH-CaCl (1:5) 0-10cm",
#                        "eCEC (cmol/kg) 0-10cm",
#                        "EC (dS/m) 0-10cm",
#                        "Clay (%) 0-10cm",
#                        "Sand (%) 0-10cm",
#                        "Silt (%) 0-10cm",
#                        "Colwell P (mg/kg) 0-10cm",
#                        "Nitrate N (mg/kg) 0-10cm")
# 
# names(maps_40_60) <- c("pH-CaCl (1:5) 40-60cm",
#                        "eCEC (cmol/kg) 40-60cm",
#                        "EC (dS/m) 40-60cm",
#                        "Clay (%) 40-60cm",
#                        "Sand (%) 40-60cm",
#                        "Silt (%) 40-60cm")
# 
# pH_w_veris0_10   <- rast(paste0(headDir, soil_grid_path_details, "0-10_pH_w_veris.tif"))
# names(pH_w_veris0_10) <- "pH_w_veris0_10"
# 
# DepthOfSand      <- rast(paste0(headDir, soil_grid_path_details, "DepthOfSand.tif"))
# names(DepthOfSand) <- "DepthOfSand"
# 
# DepthToClay      <- rast(paste0(headDir, soil_grid_path_details, "DepthToClay.tif"))
# names(DepthToClay) <- "DepthToClay"
# 
# DepthToFizz      <- rast(paste0(headDir, soil_grid_path_details, "DepthToFizz.tif"))
# names(DepthToFizz) <- "DepthToFizz"
# 
# DepthToNeutral   <- rast(paste0(headDir, soil_grid_path_details, "DepthToNeutral.tif"))
# names(DepthToNeutral) <- "DepthToNeutral"
# 
# SurfaceRepelence <- rast(paste0(headDir, soil_grid_path_details, "SurfaceRepelence.tif"))
# names(SurfaceRepelence) <- "SurfaceRepelence"
# 
# 
# # project all
# maps_0_10        <- project(maps_0_10,        paste0("EPSG:", projetion_crs))
# maps_40_60       <- project(maps_40_60,        paste0("EPSG:", projetion_crs))
# pH_w_veris0_10   <- project(pH_w_veris0_10,   paste0("EPSG:", projetion_crs))
# DepthOfSand      <- project(DepthOfSand,      paste0("EPSG:", projetion_crs))
# DepthToClay      <- project(DepthToClay,      paste0("EPSG:", projetion_crs))
# DepthToFizz      <- project(DepthToFizz,      paste0("EPSG:", projetion_crs))
# DepthToNeutral   <- project(DepthToNeutral,   paste0("EPSG:", projetion_crs))
# SurfaceRepelence <- project(SurfaceRepelence, paste0("EPSG:", projetion_crs))
# 
# # check resolutions match first
# res(boundary_ratser)
# res(DepthToClay)
# 
# maps_0_10        <- resample(maps_0_10,        boundary_raster, method = "bilinear")
# maps_40_60       <- resample(maps_40_60,       boundary_raster, method = "bilinear")
# pH_w_veris0_10   <- resample(pH_w_veris0_10,   boundary_raster, method = "bilinear")
# DepthOfSand      <- resample(DepthOfSand,      boundary_raster, method = "bilinear")
# DepthToClay      <- resample(DepthToClay,      boundary_raster, method = "bilinear")
# DepthToFizz      <- resample(DepthToFizz,      boundary_raster, method = "bilinear")
# DepthToNeutral   <- resample(DepthToNeutral,   boundary_raster, method = "bilinear")
# SurfaceRepelence <- resample(SurfaceRepelence, boundary_raster, method = "bilinear")
# 
# raster_stack <- c(maps_0_10,
#                   maps_40_60,
#                   pH_w_veris0_10,
#                   DepthOfSand,
#                   DepthToClay,
#                   DepthToFizz,
#                   DepthToNeutral,
#                   SurfaceRepelence)

################################################################################
### Crystal_Brook_Brians_House Grids
# maps_0_10 <- rast(paste0(headDir, soil_grid_path_details, "0-10_maps.tif"))
# maps_40_60 <- rast(paste0(headDir, soil_grid_path_details, "40-60_maps.tif"))
# 
# names(maps_0_10)
# names(maps_40_60)
# 
# names(maps_0_10)  <- c("pH-CaCl (1:5) 0-10cm",
#                        "eCEC (cmol/kg) 0-10cm",
#                        "EC (dS/m) 0-10cm",
#                        "Clay (%) 0-10cm",
#                        "Colwell P (mg/kg) 0-10cm",
#                        "Nitrate N (mg/kg) 0-10cm")
# 
# names(maps_40_60) <- c("pH-CaCl (1:5) 40-60cm",
#                        "eCEC (cmol/kg) 40-60cm",
#                        "EC (dS/m) 40-60cm",
#                        "Clay (%) 40-60cm",
#                        "Nitrate N (mg/kg) 40-60cm")
# 
# 
# DepthToClay <- rast(paste0(headDir, soil_grid_path_details, "DepthToClay.tif"))
# DepthToFizz <- rast(paste0(headDir, soil_grid_path_details, "DepthToFizz.tif"))
# DepthToNeutral <- rast(paste0(headDir, soil_grid_path_details, "DepthToNeutral.tif"))
# Force_map <- rast(paste0(headDir, soil_grid_path_details, "Force_map.tif"))
# SurfaceRepelence<- rast(paste0(headDir, soil_grid_path_details, "SurfaceRepelence.tif"))
# 
# 
# # project each raster individually first
# maps_0_10        <- project(maps_0_10,        paste0("EPSG:", projetion_crs))
# maps_40_60       <- project(maps_40_60,        paste0("EPSG:", projetion_crs))
# DepthToClay      <- project(DepthToClay,      paste0("EPSG:", projetion_crs))
# DepthToFizz      <- project(DepthToFizz,      paste0("EPSG:", projetion_crs))
# DepthToNeutral   <- project(DepthToNeutral,   paste0("EPSG:", projetion_crs))
# Force_map        <- project(Force_map,        paste0("EPSG:", projetion_crs))
# SurfaceRepelence <- project(SurfaceRepelence, paste0("EPSG:", projetion_crs))
# 
# 
# 
# # check resolutions match first
# res(boundary_raster)
# res(DepthToClay)
# 
# # resample all to match maps_0_10 as reference
# DepthToClay    <- resample(DepthToClay,    boundary_raster, method = "bilinear")
# DepthToFizz    <- resample(DepthToFizz,    boundary_raster, method = "bilinear")
# DepthToNeutral <- resample(DepthToNeutral, boundary_raster, method = "bilinear")
# Force_map      <- resample(Force_map,      boundary_raster, method = "bilinear")
# SurfaceRepelence <- resample(SurfaceRepelence, boundary_raster, method = "bilinear")
# maps_0_10       <-  resample(maps_0_10, boundary_raster, method = "bilinear")
# maps_40_60      <- resample(maps_40_60, boundary_raster, method = "bilinear")
# 
# 
# raster_stack <- c(maps_0_10,
#                   maps_40_60,
#                   DepthToClay,
#                   DepthToFizz,
#                   DepthToNeutral,
#                   Force_map,
#                   SurfaceRepelence)
# names(raster_stack)




################################################################################
### Wynarka_Mervs_West Grids
# DepthToB <- rast(paste0(headDir, soil_grid_path_details, "DepthToB.tif"))
# DepthToClay_SR <- rast(paste0(headDir, soil_grid_path_details, "DepthToClay_SR.tif"))
# DepthToFizz <- rast(paste0(headDir, soil_grid_path_details, "DepthToFizz.tif"))
# DepthToGravel <- rast(paste0(headDir, soil_grid_path_details, "DepthToGravel.tif"))
# DepthToSheetLime <- rast(paste0(headDir, soil_grid_path_details, "DepthToSheetLime.tif"))
# pH_cacl_top10cm <- rast(paste0(headDir, soil_grid_path_details, "pH_cacl_top10cm.tif"))
# Surf_Carbonates <- rast(paste0(headDir, soil_grid_path_details, "Surf_Carbonates.tif"))
# Surf_Repellence<- rast(paste0(headDir, soil_grid_path_details, "Surf_Repellence.tif"))
# 
# 
# # project each raster individually first
# DepthToB         <- project(DepthToB,         paste0("EPSG:", projetion_crs))
# names(DepthToB)  <- "DepthToB"
# 
# DepthToClay_SR   <- project(DepthToClay_SR,   paste0("EPSG:", projetion_crs))
# names(DepthToClay_SR) <- "DepthToClay_SR"
# 
# DepthToFizz      <- project(DepthToFizz,      paste0("EPSG:", projetion_crs))
# names(DepthToFizz) <- "DepthToFizz"
# 
# DepthToGravel    <- project(DepthToGravel,    paste0("EPSG:", projetion_crs))
# names(DepthToGravel) <- "DepthToGravel"
# 
# DepthToSheetLime <- project(DepthToSheetLime, paste0("EPSG:", projetion_crs))
# names(DepthToSheetLime) <- "DepthToSheetLime"
# 
# pH_cacl_top10cm  <- project(pH_cacl_top10cm,  paste0("EPSG:", projetion_crs))
# names(pH_cacl_top10cm) <- "pH_cacl_top10cm"
# 
# Surf_Carbonates  <- project(Surf_Carbonates,  paste0("EPSG:", projetion_crs))
# names(Surf_Carbonates) <- "Surf_Carbonates"
# 
# Surf_Repellence  <- project(Surf_Repellence,  paste0("EPSG:", projetion_crs))
# names(Surf_Repellence) <- "Surf_Repellence"
# 
# 
# raster_list <- list(DepthToB, DepthToClay_SR, DepthToFizz, DepthToGravel,
#                     DepthToSheetLime, pH_cacl_top10cm, Surf_Carbonates, Surf_Repellence)
# 
# names(raster_list) <- c("DepthToB", "DepthToClay_SR", "DepthToFizz", "DepthToGravel",
#                         "DepthToSheetLime", "pH_cacl_top10cm", "Surf_Carbonates", "Surf_Repellence")
# 
# extent_df <- do.call(rbind, lapply(names(raster_list), function(n) {
#   e <- ext(raster_list[[n]])
#   data.frame(name = n, xmin = e[1], xmax = e[2], ymin = e[3], ymax = e[4])
# }))
# 
# ggplot(extent_df) +
#   geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
#                 colour = name), fill = NA, linewidth = 1) +
#   labs(title = "Raster extents", x = "X", y = "Y", colour = "Raster") +
#   theme_minimal()
# 
# 
# 
# # check resolutions match first
# # check all resolutions
# res(DepthToB)
# res(DepthToClay_SR)
# res(DepthToFizz)
# res(DepthToGravel)
# res(DepthToSheetLime)
# res(pH_cacl_top10cm)
# res(Surf_Carbonates)
# res(Surf_Repellence)
# res(boundary_raster)
# 
# # resample all to match DepthToB as reference
# DepthToClay_SR   <- resample(DepthToClay_SR,   boundary_raster, method = "bilinear")
# DepthToFizz      <- resample(DepthToFizz,      boundary_raster, method = "bilinear")
# DepthToGravel    <- resample(DepthToGravel,    boundary_raster, method = "bilinear")
# DepthToSheetLime <- resample(DepthToSheetLime, boundary_raster, method = "bilinear")
# pH_cacl_top10cm  <- resample(pH_cacl_top10cm,  boundary_raster, method = "bilinear")
# Surf_Carbonates  <- resample(Surf_Carbonates,  boundary_raster, method = "bilinear")
# Surf_Repellence  <- resample(Surf_Repellence,  boundary_raster, method = "bilinear")
# DepthToB  <-        resample(DepthToB,  boundary_raster, method = "bilinear")
# 
# raster_stack <- c(DepthToB,
#                   DepthToClay_SR,
#                   DepthToFizz,
#                   DepthToGravel,
#                   DepthToSheetLime,
#                   pH_cacl_top10cm,
#                   Surf_Carbonates,
#                   Surf_Repellence)
# 
# 
# names(raster_stack)

################################################################################

# Wharminda Grids

# Clay_40_60cm     <- rast(paste0(headDir, soil_grid_path_details, "Clay_40_60cm.tif"))
# DepthToB         <- rast(paste0(headDir, soil_grid_path_details, "DepthToB.tif"))
# DepthToClay      <- rast(paste0(headDir, soil_grid_path_details, "DepthToClay.tif"))
# DepthToClay_sr   <- rast(paste0(headDir, soil_grid_path_details, "DepthToClay_sr.tif"))
# DepthToFizz      <- rast(paste0(headDir, soil_grid_path_details, "DepthToFizz.tif"))
# pH_cacl_top10cm  <- rast(paste0(headDir, soil_grid_path_details, "pH_cacl_top10cm.tif"))
# pH_water_top10cm <- rast(paste0(headDir, soil_grid_path_details, "pH_water_top10cm.tif"))
# SurfaceRepelence <- rast(paste0(headDir, soil_grid_path_details, "SurfaceRepelence.tif"))
# SurfCarbonates   <- rast(paste0(headDir, soil_grid_path_details, "SurfCarbonates.tif"))
# 
# 
# # project each raster individually first
# Clay_40_60cm     <- project(Clay_40_60cm,     paste0("EPSG:", projetion_crs))
# names(Clay_40_60cm) <- "Clay_40_60cm"
# 
# DepthToB         <- project(DepthToB,         paste0("EPSG:", projetion_crs))
# names(DepthToB)  <- "DepthToB"
# 
# DepthToClay      <- project(DepthToClay,      paste0("EPSG:", projetion_crs))
# names(DepthToClay) <- "DepthToClay"
# 
# DepthToClay_sr   <- project(DepthToClay_sr,   paste0("EPSG:", projetion_crs))
# names(DepthToClay_sr) <- "DepthToClay_sr"
# 
# DepthToFizz      <- project(DepthToFizz,      paste0("EPSG:", projetion_crs))
# names(DepthToFizz) <- "DepthToFizz"
# 
# pH_cacl_top10cm  <- project(pH_cacl_top10cm,  paste0("EPSG:", projetion_crs))
# names(pH_cacl_top10cm) <- "pH_cacl_top10cm"
# 
# pH_water_top10cm <- project(pH_water_top10cm, paste0("EPSG:", projetion_crs))
# names(pH_water_top10cm) <- "pH_water_top10cm"
# 
# SurfaceRepelence <- project(SurfaceRepelence, paste0("EPSG:", projetion_crs))
# names(SurfaceRepelence) <- "SurfaceRepelence"
# 
# SurfCarbonates   <- project(SurfCarbonates,   paste0("EPSG:", projetion_crs))
# names(SurfCarbonates) <- "SurfCarbonates"
# 
# 
# 
# raster_list <- list(Clay_40_60cm, DepthToB, DepthToClay, DepthToClay_sr,
#                     DepthToFizz, pH_cacl_top10cm, pH_water_top10cm,
#                     SurfaceRepelence, SurfCarbonates)

# names(raster_list) <- c("Clay_40_60cm", "DepthToB", "DepthToClay", "DepthToClay_sr",
#                         "DepthToFizz", "pH_cacl_top10cm", "pH_water_top10cm",
#                         "SurfaceRepelence", "SurfCarbonates")
# 
# extent_df <- do.call(rbind, lapply(names(raster_list), function(n) {
#   e <- ext(raster_list[[n]])
#   data.frame(name = n, xmin = e[1], xmax = e[2], ymin = e[3], ymax = e[4])
# }))
# 
# ggplot(extent_df) +
#   geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
#                 colour = name), fill = NA, linewidth = 1) +
#   labs(title = "Raster extents", x = "X", y = "Y", colour = "Raster") +
#   theme_minimal()



# check resolutions match first
# check all resolutions


# resample all to match  reference
# Clay_40_60cm     <- resample(Clay_40_60cm,     boundary_raster, method = "bilinear")
# DepthToB         <- resample(DepthToB,         boundary_raster, method = "bilinear")
# DepthToClay      <- resample(DepthToClay,      boundary_raster, method = "bilinear")
# DepthToClay_sr   <- resample(DepthToClay_sr,   boundary_raster, method = "bilinear")
# pH_cacl_top10cm  <- resample(pH_cacl_top10cm,  boundary_raster, method = "bilinear")
# pH_water_top10cm <- resample(pH_water_top10cm, boundary_raster, method = "bilinear")
# SurfaceRepelence <- resample(SurfaceRepelence, boundary_raster, method = "bilinear")
# SurfCarbonates   <- resample(SurfCarbonates,   boundary_raster, method = "bilinear")
# DepthToFizz     <- resample(DepthToFizz,     boundary_raster, method = "bilinear")
# 
# raster_stack <- c(Clay_40_60cm,
#                   DepthToB,
#                   DepthToClay,
#                   DepthToClay_sr,
#                   DepthToFizz,
#                   pH_cacl_top10cm,
#                   pH_water_top10cm,
#                   SurfaceRepelence,
#                   SurfCarbonates)
# 
# 
# names(raster_stack)

###############################################################################
# Walpeup_Gums

# Clay_40_60cm     <- rast(paste0(headDir, soil_grid_path_details, "Clay_40_60cm.tif"))
# DepthtoB         <- rast(paste0(headDir, soil_grid_path_details, "DepthtoB.tif"))
# DepthToClay      <- rast(paste0(headDir, soil_grid_path_details, "DepthToClay.tif"))
# DepthtoFizz      <- rast(paste0(headDir, soil_grid_path_details, "DepthtoFizz.tif"))
# pH_cacl_top10cm  <- rast(paste0(headDir, soil_grid_path_details, "pH_cacl_top10cm.tif"))
# pH_water_top10cm <- rast(paste0(headDir, soil_grid_path_details, "pH_water_top10cm.tif"))
# SurfaceRepelence <- rast(paste0(headDir, soil_grid_path_details, "SurfaceRepelence.tif"))
# SurfCarbonates   <- rast(paste0(headDir, soil_grid_path_details, "SurfCarbonates.tif"))
# 
# # project each raster individually first
# Clay_40_60cm     <- project(Clay_40_60cm,     paste0("EPSG:", projetion_crs))
# names(Clay_40_60cm) <- "Clay_40_60cm"
# 
# DepthtoB         <- project(DepthtoB,         paste0("EPSG:", projetion_crs))
# names(DepthtoB)  <- "DepthtoB"
# 
# DepthToClay      <- project(DepthToClay,      paste0("EPSG:", projetion_crs))
# names(DepthToClay) <- "DepthToClay"
# 
# DepthtoFizz      <- project(DepthtoFizz,      paste0("EPSG:", projetion_crs))
# names(DepthtoFizz) <- "DepthtoFizz"
# 
# pH_cacl_top10cm  <- project(pH_cacl_top10cm,  paste0("EPSG:", projetion_crs))
# names(pH_cacl_top10cm) <- "pH_cacl_top10cm"
# 
# pH_water_top10cm <- project(pH_water_top10cm, paste0("EPSG:", projetion_crs))
# names(pH_water_top10cm) <- "pH_water_top10cm"
# 
# SurfaceRepelence <- project(SurfaceRepelence, paste0("EPSG:", projetion_crs))
# names(SurfaceRepelence) <- "SurfaceRepelence"
# 
# SurfCarbonates   <- project(SurfCarbonates,   paste0("EPSG:", projetion_crs))
# names(SurfCarbonates) <- "SurfCarbonates"
# 
# raster_list <- list(Clay_40_60cm, DepthtoB, DepthToClay, DepthtoFizz,
#                     pH_cacl_top10cm, pH_water_top10cm,
#                     SurfaceRepelence, SurfCarbonates)
# 
# names(raster_list) <- c("Clay_40_60cm", "DepthtoB", "DepthToClay", "DepthtoFizz",
#                         "pH_cacl_top10cm", "pH_water_top10cm",
#                         "SurfaceRepelence", "SurfCarbonates")

# extent_df <- do.call(rbind, lapply(names(raster_list), function(n) {
#   e <- ext(raster_list[[n]])
#   data.frame(name = n, xmin = e[1], xmax = e[2], ymin = e[3], ymax = e[4])
# }))
# 
# ggplot(extent_df) +
#   geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
#                 colour = name), fill = NA, linewidth = 1) +
#   labs(title = "Raster extents", x = "X", y = "Y", colour = "Raster") +
#   theme_minimal()
# 
# # resample all to match reference
# Clay_40_60cm     <- resample(Clay_40_60cm,     boundary_raster, method = "bilinear")
# DepthtoB         <- resample(DepthtoB,         boundary_raster, method = "bilinear")
# DepthToClay      <- resample(DepthToClay,      boundary_raster, method = "bilinear")
# DepthtoFizz      <- resample(DepthtoFizz,      boundary_raster, method = "bilinear")
# pH_cacl_top10cm  <- resample(pH_cacl_top10cm,  boundary_raster, method = "bilinear")
# pH_water_top10cm <- resample(pH_water_top10cm, boundary_raster, method = "bilinear")
# SurfaceRepelence <- resample(SurfaceRepelence, boundary_raster, method = "bilinear")
# SurfCarbonates   <- resample(SurfCarbonates,   boundary_raster, method = "bilinear")
# 
# raster_stack <- c(Clay_40_60cm,
#                   DepthtoB,
#                   DepthToClay,
#                   DepthtoFizz,
#                   pH_cacl_top10cm,
#                   pH_water_top10cm,
#                   SurfaceRepelence,
#                   SurfCarbonates)
# 
# names(raster_stack)

################################################################################
#Crystal_Brook_Randals

Clay40_60cm      <- rast(paste0(headDir, soil_grid_path_details, "Clay40_60cm.tif"))
DepthToB         <- rast(paste0(headDir, soil_grid_path_details, "DepthToB.tif"))
DepthToClay      <- rast(paste0(headDir, soil_grid_path_details, "DepthToClay.tif"))
DepthtoFizz      <- rast(paste0(headDir, soil_grid_path_details, "DepthtoFizz.tif"))
pH_cacl_top10cm  <- rast(paste0(headDir, soil_grid_path_details, "pH_cacl_top10cm.tif"))
pH_water_top10cm <- rast(paste0(headDir, soil_grid_path_details, "pH_water_top10cm.tif"))
SurfaceRepelence <- rast(paste0(headDir, soil_grid_path_details, "SurfaceRepelence.tif"))

# project each raster individually first
Clay40_60cm      <- project(Clay40_60cm,      paste0("EPSG:", projetion_crs))
names(Clay40_60cm) <- "Clay40_60cm"

DepthToB         <- project(DepthToB,         paste0("EPSG:", projetion_crs))
names(DepthToB)  <- "DepthToB"

DepthToClay      <- project(DepthToClay,      paste0("EPSG:", projetion_crs))
names(DepthToClay) <- "DepthToClay"

DepthtoFizz      <- project(DepthtoFizz,      paste0("EPSG:", projetion_crs))
names(DepthtoFizz) <- "DepthtoFizz"

pH_cacl_top10cm  <- project(pH_cacl_top10cm,  paste0("EPSG:", projetion_crs))
names(pH_cacl_top10cm) <- "pH_cacl_top10cm"

pH_water_top10cm <- project(pH_water_top10cm, paste0("EPSG:", projetion_crs))
names(pH_water_top10cm) <- "pH_water_top10cm"

SurfaceRepelence <- project(SurfaceRepelence, paste0("EPSG:", projetion_crs))
names(SurfaceRepelence) <- "SurfaceRepelence"

raster_list <- list(Clay40_60cm, DepthToB, DepthToClay, DepthtoFizz,
                    pH_cacl_top10cm, pH_water_top10cm, SurfaceRepelence)

names(raster_list) <- c("Clay40_60cm", "DepthToB", "DepthToClay", "DepthtoFizz",
                        "pH_cacl_top10cm", "pH_water_top10cm", "SurfaceRepelence")

extent_df <- do.call(rbind, lapply(names(raster_list), function(n) {
  e <- ext(raster_list[[n]])
  data.frame(name = n, xmin = e[1], xmax = e[2], ymin = e[3], ymax = e[4])
}))

ggplot(extent_df) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                colour = name), fill = NA, linewidth = 1) +
  labs(title = "Raster extents", x = "X", y = "Y", colour = "Raster") +
  theme_minimal()

# resample all to match reference
Clay40_60cm      <- resample(Clay40_60cm,      boundary_raster, method = "bilinear")
DepthToB         <- resample(DepthToB,         boundary_raster, method = "bilinear")
DepthToClay      <- resample(DepthToClay,      boundary_raster, method = "bilinear")
DepthtoFizz      <- resample(DepthtoFizz,      boundary_raster, method = "bilinear")
pH_cacl_top10cm  <- resample(pH_cacl_top10cm,  boundary_raster, method = "bilinear")
pH_water_top10cm <- resample(pH_water_top10cm, boundary_raster, method = "bilinear")
SurfaceRepelence <- resample(SurfaceRepelence, boundary_raster, method = "bilinear")

raster_stack <- c(Clay40_60cm,
                  DepthToB,
                  DepthToClay,
                  DepthtoFizz,
                  pH_cacl_top10cm,
                  pH_water_top10cm,
                  SurfaceRepelence)

names(raster_stack)


################################################################################
## Check zones and raster match epsg
st_crs(raster_stack)$epsg
st_crs(zones)$epsg
st_crs(boundary_raster)$epsg

## Now for each zone what is the zonal 

raster_stack_points <- as.points(raster_stack, values = TRUE, na.rm = TRUE) %>%
  st_as_sf()

raster_stack_points <- st_join(raster_stack_points, zones, join = st_intersects)


## tidy up
names(raster_stack_points)
raster_stack_points <- raster_stack_points %>%
  dplyr::select(
         #Id, 
         #gridcode, 
         cluster,  
         #fcl_mdl,
         #cluster3,  
         
         POLY_AREA, 
         everything()#, 
         #-val
         ) 
## add coordinates
raster_stack_points <- raster_stack_points %>%
  mutate(x = st_coordinates(.)[, 1],
         y = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  as.data.frame()

str(raster_stack_points)


# write.csv(raster_stack_points, 
#           paste0(headDir,"/9.Maps/Soil/Jackie_working/", 
#                  "soil_grids_with_zone_x_y.csv"),
#           row.names = FALSE)
################################################################################

raster_stack_summary <- raster_stack_points %>%
  #filter(!is.na(gridcode)) %>%
  filter(!is.na(cluster)) %>%
  #filter(!is.na(fcl_mdl)) %>%
  #filter(!is.na(cluster3)) %>%
  #dplyr::select(-Id, -POLY_AREA, -x, -y) %>%
  dplyr::select( -POLY_AREA, -x, -y) %>%
  #group_by(gridcode) %>%
  group_by(cluster) %>%
  #group_by(fcl_mdl) %>%
  #group_by(cluster3) %>%
  summarise(across(where(is.numeric), 
                   list(mean   = ~mean(.,   na.rm = TRUE),
                        median = ~median(., na.rm = TRUE),
                        min    = ~min(.,    na.rm = TRUE),
                        max    = ~max(.,    na.rm = TRUE),
                        range  = ~max(.,    na.rm = TRUE) - min(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))


raster_stack_summary_long <- raster_stack_summary %>%
  #pivot_longer(cols = -gridcode,
  pivot_longer(cols = -cluster,
  #pivot_longer(cols = -fcl_mdl,
  #pivot_longer(cols = -cluster3,
               names_to = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)",  # split on last underscore
               values_to = "value")
raster_stack_summary_long


write.csv(raster_stack_summary_long, 
          paste0(headDir,"/9.Maps/Soil/Jackie_working/", 
                 "soil_grids_with_zone_summary_5m_grid.csv"),
          row.names = FALSE)
