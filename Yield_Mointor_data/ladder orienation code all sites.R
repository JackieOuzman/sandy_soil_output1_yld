rm(list=ls())
library(sf)
library(dplyr)
library(readxl)


################################################################################
########################     Define the directory and sites    #################
################################################################################
analysis.type <- "Harvest"

site_numbers <- c(
  "1.Walpeup_MRS125",
  "2.Crystal_Brook_Brians_House",
  "3.Wynarka_Mervs_West",
  "4.Wharminda",
  "5.Walpeup_Gums",
  "6.Crystal_Brook_Randals"
)

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
metadata_path <- paste0(dir, "/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"



# Storage for results
orientation_results <- list()

for (site_number in site_numbers) {
  
  cat("\nProcessing:", site_number, "\n")
  
  headDir <- paste0(dir, "/work/Output-1/", site_number)
  
  ladders_data_file_path <- readxl::read_excel(
    paste0(metadata_path, metadata_file_name),
    sheet = "file location etc") %>%
    filter(Site == site_number) %>%
    filter(variable == "ladder polygons") %>%
    pull("file path")
  
  ladders_qgis <- st_read(paste0(headDir, "/", ladders_data_file_path), quiet = TRUE)
  
  ladders <- ladders_qgis %>%
    mutate(treat_desc = stringr::str_remove_all(treat_desc, "\n") %>% trimws()) %>%
    mutate(centroid = st_centroid(geometry),
           cent_x = st_coordinates(centroid)[, 1],
           cent_y = st_coordinates(centroid)[, 2])
  
  model_ns <- lm(cent_y ~ PointID, data = ladders)
  model_ew <- lm(cent_x ~ PointID, data = ladders)
  
  r2_ns <- summary(model_ns)$r.squared
  r2_ew <- summary(model_ew)$r.squared
  slope_ns <- coef(model_ns)[2]
  slope_ew <- coef(model_ew)[2]
  
  if (r2_ns > r2_ew & r2_ns > 0.7) {
    if (slope_ns > 0) {
      orientation <- "S to N"; start_label <- "S"; end_label <- "N"
    } else {
      orientation <- "N to S"; start_label <- "N"; end_label <- "S"
    }
  } else if (r2_ew > r2_ns & r2_ew > 0.7) {
    if (slope_ew > 0) {
      orientation <- "W to E"; start_label <- "W"; end_label <- "E"
    } else {
      orientation <- "E to W"; start_label <- "E"; end_label <- "W"
    }
  } else {
    orientation <- "diagonal or unclear"; start_label <- "?"; end_label <- "?"
  }
  
  cat("  Orientation:", orientation, "| Start:", start_label, "| End:", end_label, "\n")
  
  # Store result for this site
  orientation_results[[site_number]] <- data.frame(
    site        = site_number,
    orientation = orientation,
    start_label = start_label,
    end_label   = end_label,
    r2_ns       = round(r2_ns, 3),
    r2_ew       = round(r2_ew, 3)
  )
}

# Combine and save
orientation_df <- bind_rows(orientation_results)


write.csv(orientation_df, 
          paste0(metadata_path, "ladder_orientation_all_sites.csv"), 
          row.names = FALSE)





