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
# site_number <- "1.Walpeup_MRS125"
# site_name <- "Walpeup_MRS125"

# site_number <- "3.Wynarka_Mervs_West"
# site_name <- "Wynarka_Mervs_West"

site_number <- "98.Auxillary_Sites/11.Qualeup_Spanners"
site_name <- "Auxillary_Sites_11.Qualeup_Spanners"


dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

analysis.type <- "Harvest"


clean.dat <- "No"
#analysis.yr <- "25"


metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

crs_used <- 7850


Yld_data_av_to_ladder <- read.csv(paste0(
  headDir,
  "/8.Yield_Data/25/Processed/",
  "Yld_data_av_to_ladder.csv"
))
  
#Just removes any white spaces
Yld_data_av_to_ladder <- Yld_data_av_to_ladder %>%
  mutate(treat_desc = stringr::str_remove_all(treat_desc, "\n") %>% trimws())
unique(Yld_data_av_to_ladder$treat_desc)


# Split by year
Yld_data_2020 <- Yld_data_av_to_ladder %>% filter(year == 2020)
Yld_data_2021 <- Yld_data_av_to_ladder %>% filter(year == 2021)
Yld_data_2022 <- Yld_data_av_to_ladder %>% filter(year == 2022)
Yld_data_2023 <- Yld_data_av_to_ladder %>% filter(year == 2023)


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

##################################################################################


plot_yield_strip <- function(dat, yr) {
  dat <- dat
  yr <- unique(dat$year)
  headDir_analysis_folder <- paste0(headDir, "/10.Analysis/Harvest/")
  
    dat <- dat %>%
    mutate(treat_desc = stringr::str_remove_all(treat_desc, "\n") %>% trimws()) %>%
    mutate(treat_desc = case_when(
      treat_desc == "Control (-Tillage -Lime)" ~ "Control",
      .default = as.character(treat_desc)
    ))
  
  treatments <- unique(dat$treat_desc[dat$treat_desc != "Control"])
  
  control_data <- dat %>%
    filter(treat_desc == "Control") %>%
    slice(rep(1:n(), length(treatments))) %>%
    mutate(facet = rep(treatments, each = n() / length(treatments)))
  
  treatment_data <- dat %>%
    filter(treat_desc != "Control") %>%
    mutate(facet = treat_desc)
  
  combined <- rbind(control_data, treatment_data)
  combined$facet <- factor(combined$facet, levels = treatment_order)
  
  ggplot(combined, aes(x = Ladder_PointID * 10, y = mean_yld, color = treat_desc)) +
    geom_line(linewidth = 1) +
    facet_wrap(~ facet) +
    scale_color_manual(values = treatment_colour) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
      title = paste("Yield down strip —", yr),
      x = "Distance along strip",
      y = "Mean yield"
    )
  
  ggsave(paste0(headDir_analysis_folder, "Yld_monitor_strip_distance_", yr, ".png"),
         width = 14, height = 8)
}


# Run for each year
plot_yield_strip(Yld_data_2020, 2020)
plot_yield_strip(Yld_data_2021, 2021)
plot_yield_strip(Yld_data_2022, 2022)
plot_yield_strip(Yld_data_2023, 2023)
