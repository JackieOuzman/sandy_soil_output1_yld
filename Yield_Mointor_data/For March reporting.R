library(terra)
library(sf)
library(ggplot2)
library(multcomp)
library(dplyr)
library(tidyr)
library(broom)
library(multcompView)
library(lubridate)
library(terra)
library(tidyterra)
library(readxl)
library(broom)

library(patchwork)
library(gridExtra)


# site_number <- "1.Walpeup_MRS125"
# site_name <- "Walpeup_MRS125"

# site_number <-"2.Crystal_Brook_Brians_House" 
# site_name <-  "Crystal_Brook_Brians_House"

# site_number <- "3.Wynarka_Mervs_West"
# site_name <- "Wynarka_Mervs_West"

site_number <- "4.Wharminda"
site_name <- "Wharminda"

# site_number <- "5.Walpeup_Gums"
# site_name <- "Walpeup_Gums"

# site_number <- "6.Crystal_Brook_Randals"
# site_name <- "Crystal_Brook_Randals"




dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.type <- "Harvest"



analysis.yr <- "25"


metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"




crs_used <- 7854

Yld_data_av_to_ladder <- read.csv(paste0(
  headDir,
  "/8.Yield_Data/25/Processed/",
  "Yld_data_av_to_ladder.csv"
))

#Just removes any white spaces
Yld_data_av_to_ladder <- Yld_data_av_to_ladder %>%
  mutate(treat_desc = stringr::str_remove_all(treat_desc, "\n") %>% trimws())
unique(Yld_data_av_to_ladder$treat_desc)

str(Yld_data_av_to_ladder)

Yld_data_av_to_ladder <- Yld_data_av_to_ladder %>% 
  dplyr::rename(target.variable = mean_yld,
                zone = mean_zone)



summary_stats <- Yld_data_av_to_ladder %>%
  group_by(treat, treat_desc, zone) %>% 
  summarize(
    mean = mean(target.variable, na.rm = TRUE),
    sd = sd(target.variable, na.rm = TRUE),
    min = min(target.variable, na.rm = TRUE),
    max = max(target.variable, na.rm = TRUE),
    median = median(target.variable, na.rm = TRUE),
    Q1 = quantile(target.variable, 0.25, na.rm = TRUE),
    Q3 = quantile(target.variable, 0.75, na.rm = TRUE),
    n_total = n(),                      # Total number of rows
    n_valid = sum(!is.na(target.variable)),     # Count of non-NA values
    n_na = sum(is.na(target.variable))  
  )
summary_stats
summary_stats <- dplyr::ungroup(summary_stats)
str(summary_stats)

##### subset data for Plot
unique(summary_stats$treat_desc)

summary_stats_subset <- summary_stats %>% 
  dplyr:::filter(treat_desc == "Control" | 
                 treat_desc ==  "Rip + Delve" |
                 treat_desc ==  "Spade + Rip" ) 
#%>%  dplyr:::filter(zone  != "3")

strip_names_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "treatment names") %>% 
  filter(Site == site_number)


strip_names_details <- strip_names_details %>% 
  select("treat" ,
         "Shorthand Name"  ,
         "Treatment Name" ,
         "Order in Paddock" ,
         "Hex" )

zone_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "zone_details") %>% 
  filter(Site == site_number) %>% 
  select(`zone names`, `zone label names`)
zone_details

###############################################################################
#append the metadata file 2
summary_stats_subset <- left_join(summary_stats_subset, strip_names_details)
names(summary_stats_subset)

# Reorder treat based on Order_in_paddock
summary_stats_subset <- summary_stats_subset %>%
  mutate(treat = reorder(treat, `Order in Paddock`),
         `Treatment Name` = reorder(`Treatment Name`, `Order in Paddock`))
str(summary_stats_subset)
zone_details


summary_stats_subset$zone <- as.character(summary_stats_subset$zone)
summary_stats_subset <- left_join(summary_stats_subset, 
                                          zone_details,
                                          join_by(zone == `zone names`)) 
summary_stats_subset <- summary_stats_subset %>% 
  rename(`zone_label` = `zone label names`)

str(summary_stats_subset)


##################################################################################

site.bar.plot <-
  summary_stats_subset %>%
  mutate(treat = reorder(treat, `Order in Paddock`)) %>%
  ggplot(aes(x = treat, y = mean, fill = `Treatment Name`)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black") +
  labs(
    title = "",
    x = NULL,
    y = paste0("Yield (t/ha)"),  # \n creates line break
    fill = NULL
  ) +
  scale_fill_manual(values = setNames(summary_stats_subset$Hex, 
                                      summary_stats_subset$`Treatment Name`)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.caption = element_text(size = 10),
    legend.position = "bottom",
    #axis.text.x = element_text(angle = 45, hjust = 1)
    axis.text.x = element_blank()
  )+
  facet_wrap(.~zone_label)

# Print the plot
site.bar.plot

#### add the summary stats to the plot

names(summary_stats_subset)
summary_stats_subset_mean<-  summary_stats_subset %>% dplyr::select(treat_desc, mean, zone_label)
summary_stats_subset_mean


# Create the wide table
wide_table <- summary_stats_subset_mean %>%
  pivot_wider(
    names_from  = zone_label,
    values_from = mean
  ) %>%
  rename(Treatment = treat_desc) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# # Convert to a grob
# table_grob <- tableGrob(wide_table, rows = NULL)
# 
# # Combine using patchwork's grid layout
# site.bar.plot + gridExtra::grid.arrange(table_grob) +
#   plot_layout(ncol = 1, heights = c(3, 1))
# 
# 
# gridExtra::grid.arrange(
#   site.bar.plot,
#   table_grob,
#   ncol = 1,
#   heights = c(3, 1)
# )


# Shrink table font size
table_grob <- tableGrob(wide_table, rows = NULL,
                        theme = ttheme_default(base_size = 10))

# Build combined object
combined <- gridExtra::arrangeGrob(
  site.bar.plot,
  table_grob,
  ncol = 1,
  heights = c(3, 1)
)

ggsave(paste0(headDir_analysis_folder, 'Yld_plot_reporting_March.png'),
       plot = combined,
       width = 10,
       height = 8,
       dpi = 300)


headDir_analysis_folder <- paste0(headDir, "/10.Analysis/25/Harvest/")

# ggsave(paste0(headDir_analysis_folder,
#               'Yld_plot_reporting_March.png'))
