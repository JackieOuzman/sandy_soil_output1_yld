library(dplyr)
library(purrr)
library(readr)
library(stringr)

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
site_number <- "1.Walpeup_MRS125"
site_name <- "Walpeup_MRS125"
headDir <- paste0(dir, "/work/Output-1/", site_number)
headDir

#################################################################################

path <- paste0(headDir,"/10.Analysis/25/Processing_Jackie/")
# Get all CSV files in the directory
csv_files <- list.files(path = path, 
                        pattern = "\\.csv$", 
                        full.names = TRUE)




# Get just the filenames (no paths)
# Create dataframe with file number and name
# Create dataframe with file number, name, and dates
file_df <- data.frame(
  file_number = 1:length(csv_files),
  file_name = basename(csv_files),
  date_created = file.info(csv_files)$ctime,
  full_path = csv_files,
  date_modified = file.info(csv_files)$mtime,
  file_size = file.info(csv_files)$size
)
file_df



dat_combined_7 <- csv_files[1:7] %>%
  lapply(function(file) {
    read_csv(file, col_types = cols(date_field_observation = col_character())) %>%
      mutate(source_file = basename(file))
  }) %>%
  bind_rows()


# Write to CSV
write.csv(dat_combined_7, 
          paste0(headDir,"/10.Analysis/25/Processing_Jackie/merged/", 
                 "plant_sample_merged_2025.csv"),
          row.names = FALSE)
