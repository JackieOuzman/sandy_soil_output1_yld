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

site_number <- "101.Andrew_Thomas_Tanks"
site_name <- "Andrew_Thomas_Tanks"

# site_number <- "3.Wynarka_Mervs_West"
# site_name <- "Wynarka_Mervs_West"



dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.type <- "Harvest"


clean.dat <- "No"
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

### only keep one control
Yld_data_av_to_ladder <- Yld_data_av_to_ladder %>% 
  dplyr::filter(treat_desc != "Control_S") 
Yld_data_av_to_ladder <- Yld_data_av_to_ladder %>% 
  mutate(treat_desc = case_when(
    treat_desc == "Control_N" ~ "Control",
    .default = as.character(treat_desc)
  ))

  


##### Plot

str(Yld_data_av_to_ladder)
unique(Yld_data_av_to_ladder$treat_desc)


# Get control data and treatment names (excluding control)
treatments <- unique(Yld_data_av_to_ladder$treat_desc[Yld_data_av_to_ladder$treat_desc != "Control"])
treatments

# Duplicate control rows for every treatment facet
control_data <- Yld_data_av_to_ladder %>%
  filter(treat_desc == "Control") %>%
  slice(rep(1:n(), length(treatments))) %>%
  mutate(facet = rep(treatments, each = n()/length(treatments)))

# Treatment data with facet column
treatment_data <- Yld_data_av_to_ladder %>%
  filter(treat_desc != "Control") %>%
  mutate(facet = treat_desc)

# Combine
combined <- rbind(control_data, treatment_data)

treatment_order <-  readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "treatment names") %>% 
  filter(Site == site_number) %>% 
  dplyr::arrange(`Order in Paddock`) %>% 
  dplyr::filter(`Treatment Name` != "Control") %>% 
  pull(`Treatment Name`)


combined$facet <- factor(combined$facet, levels = treatment_order)

treatment_colour <-  readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "treatment names") %>% 
  filter(Site == site_number) %>% 
  dplyr::arrange(`Order in Paddock`) %>% 
  dplyr::select(`Treatment Name`, Hex )
treatment_colour

treatment_colour <- setNames(treatment_colour$Hex, treatment_colour$`Treatment Name`)

unique(combined$treat_desc)

ggplot(combined, aes(x = Ladder_PointID*10, y = mean_yld, color = treat_desc)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ facet) +
  scale_color_manual(values = treatment_colour) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Yield down strip", x = "Distance along strip", y = "Mean yield")

headDir_analysis_folder <- paste0(headDir, "/10.Analysis/25/Harvest/")

ggsave(paste0(headDir_analysis_folder,
              'Yld_monitor_strip_distance.png'))
