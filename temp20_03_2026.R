zone_soil_grid_summary_path_1 <- readxl::read_excel(
  paste0(metadata_path, metadata_file_name),
  sheet = "file location etc") %>% 
  filter(Site == site_number) %>%
  filter(variable == "zone_soil_variable_mean" ) %>% 
  pull("file name") 

zone_soil_grid_summary_path_2 <- paste0(headDir, "/9.Maps/Soil/Jackie_working/")


# Read the file
zone_soil_grid_summary <- read.csv(file.path(zone_soil_grid_summary_path_2, zone_soil_grid_summary_path_1))
head(zone_soil_grid_summary)

zone_soil_grid_summary_tidy <- zone_soil_grid_summary%>%
  filter(stat %in% c("mean", "range")) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  arrange(cluster, stat)
zone_soil_grid_summary_tidy



means  <- zone_soil_grid_summary %>% filter(stat == "mean")  %>% select(-stat)
ranges <- zone_soil_grid_summary %>% filter(stat == "range") %>% select(-stat)

zone_soil_grid_summary_tidy_v2 <- means %>%
  left_join(ranges, by = c("cluster", "variable"), suffix = c("_mean", "_range")) %>%
  mutate(value = paste0(round(value_mean, 2), " (", round(value_range, 2), ")")) %>%
  select(-value_mean, -value_range) %>%
  pivot_wider(names_from = variable, values_from = value)
zone_soil_grid_summary_tidy_v2


#join to get short names
zone_soil_grid_summary_tidy_v2 <- zone_soil_grid_summary_tidy_v2 %>%
  left_join(df_zones %>% select(zone_number, zone_short_name), 
            by = c("cluster" = "zone_number")) %>%
  select(zone_short_name, everything())
zone_soil_grid_summary_tidy_v2


names(zone_soil_grid_summary_tidy_v2)

df_0_10cm <- zone_soil_grid_summary_tidy_v2 %>%
  select(zone_short_name, cluster, ends_with("0-10cm"))

df_40_60cm <- zone_soil_grid_summary_tidy_v2 %>%
  select(zone_short_name, cluster, ends_with("40-60cm"))

df_no_cm <- zone_soil_grid_summary_tidy_v2 %>%
  select(-matches("\\d+-\\d+cm")) #%>% 
#select(-val)


df_0_10cm_long <- df_0_10cm %>%
  select(-zone_short_name) %>%
  pivot_longer(cols = -cluster,
               names_to = "variable",
               values_to = "value") %>%
  pivot_wider(names_from = cluster,
              values_from = value)

df_40_60cm_long <- df_40_60cm %>%
  select(-zone_short_name) %>%
  pivot_longer(cols = -cluster,
               names_to = "variable",
               values_to = "value") %>%
  pivot_wider(names_from = cluster,
              values_from = value)

df_no_cm_long <- df_no_cm %>%
  select(-zone_short_name) %>%
  pivot_longer(cols = -cluster,
               names_to = "variable",
               values_to = "value") %>%
  pivot_wider(names_from = cluster,
              values_from = value)

df_0_10cm_long
df_40_60cm_long
df_no_cm_long