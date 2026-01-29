
library(dplyr)
library(purrr)
library(readr)
library(stringr)



path <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/1.Walpeup_MRS125/10.Analysis/25/Harvest/testing_cleaning_trimming_etc/compare/"
# Get all CSV files in the directory
csv_files <- list.files(path = path, 
                        pattern = "\\.csv$", 
                        full.names = TRUE)

csv_files

# Specify columns to keep
cols_to_keep <- c("treat", "mean", "sd", "min", "max", "median", 
                  "Q1", "Q3", "Significance", "treat_desc")

combined_df <- csv_files %>%
  map_dfr(~{
    df <- read_csv(.x, col_select = all_of(cols_to_keep))
    
    filename <- basename(.x)
    first_part <- sub("\\.csv$", "", filename)
    
    df %>% mutate(source_file = first_part)
  })
str(combined_df)


combined_df <- combined_df %>%
  mutate(first_num = str_extract(source_file, "^[0-9]+"))


#1 = "Noclean_no_trim" 
#2= "Thinning"
#3 = "clean_trim_by_strips 
#4 = ladders

combined_df <- combined_df %>%
  mutate(method = case_when(
    first_num == 1 ~ "Noclean_no_trim",
    first_num == 2 ~ "Thinning",
    first_num == 3 ~ "clean_trim_by_strips",
    first_num == 4 ~ "ladders",
    TRUE ~ NA_character_
  ))

ggplot(combined_df, aes(x = treat, y = mean, color = method, group = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Comparison of Mean Values Across Methods",
       x = "Treatment",
       y = "Mean",
       color = "Method") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(combined_df, aes(x = treat, y = mean, fill = method)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Mean Values Across Methods",
       x = "Treatment",
       y = "Mean",
       fill = "Method") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
