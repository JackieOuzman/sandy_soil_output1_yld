## Stirling Roberton
## Purpose: This script is built to undertake base analsysis of trial strip data

## 1) Run analysis on observed data:                  i) whole of paddock; ii) By Zone

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

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"

headDir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/1.Walpeup_MRS125"

analysis.type <- "Emergence" #Harvest, InSeason, PeakBiomass, PreSeason
analysis.yr <- "25"

## Read in data
boundary <- st_read(paste0(headDir,'/1.Paddock_Boundary/Walpeup_MRS125_Boundary_Masked_4326.shp'))
strips <- st_read(paste0(headDir,"/5.Trial_Plan/FINAL-Trial-Plan/GIS/MRS125_Strips_FINAL_wgs84.shp"))
#data.raw <- st_read(paste0(headDir,'/7.In_Season_data/24/4.Biomass/Biomass_NDVI_Walpeup_2024_merged_data.gpkg'))

zones <- rast(paste0(headDir,'/3.Covariates/6.Clusters_Zones/FINAL/MRS125_NEWZONES_round_wgs84.tif'))
zones <- terra::project(zones,'epsg:4326',method='near')

# zones.2 <- rast(paste0(headDir,'/3.Covariates/6.Clusters_Zones/FINAL/MRS125_Zones_round_wgs84.tif'))
# zones <- zones.2

emergence.dat.raw <- read.csv(paste0(headDir,'/7.In_Season_data/24/1.Emergence/emergence-dat.csv'))
emergence.pts <- st_read(paste0(headDir,'/4.Sampling/2.InSeason/24/Emergence/ACTUAL/MRS125_emergence-spls_adj_wgs84.shp'))
emergence.pts.4326 <- st_transform(emergence.pts,crs=4326)

emergence.dat.all <- inner_join(emergence.dat.raw,emergence.pts.4326,by = "pt_id")
emergence.dat <- st_as_sf(emergence.dat.all[,c("pt_id","total_count","treat_desc.x","geometry")])

data.raw <- emergence.dat

strips <- st_make_valid(strips)
strips <- strips %>%
  mutate(treat_desc = ifelse(treat_desc == "Control (-Tillage -Lime)", "Control", treat_desc))

zone_desc <- c("1" = "Transition", "2" = "Dune","3" = "Swale")  # others keep their ID
zone_labeller <- ggplot2::labeller(
  zone_id = function(z) {
    zc <- as.character(z)
    desc <- zone_desc[zc]
    ifelse(is.na(desc), zc, paste0(zc, " — ", desc))
  }
)

seasons <- tribble(
  ~year, ~crop_type, ~plant_date,   ~harvest_date,
  2024, "Lentils",  "29/05/2024",  NA_character_,
  2025, "Wheat",    "27/04/2025",  NA_character_,
  2026, NA,         NA_character_, NA_character_,
  2027, NA,         NA_character_, NA_character_
) %>%
  mutate(
    year         = as.integer(year),
    plant_date   = dmy(plant_date),
    harvest_date = dmy(harvest_date)
  )

site.info <- list(
  site_id    = "Walpeup_MRS125",
  boundary   = boundary,      # sf
  trial_plan = strips,    # sf
  seasons    = seasons        # tibble
)
class(site.info) <- c("ssii_site", class(site.info))

################################################################################
## Step 1) Define variables
## Define variable in data to analyse
variable <- "total_count"

## Define treatment column name in strips
treat.col.name <- "treat_desc"

## Define name of control
control.name <- "Control"

## Define name of Buffer (if applicable)
buffer.name <- "Buffer"

clean.dat <- "No"

model <- "XGBoost" #"Random Forest

###############################################################################
## Step 2) Clean observation data (if applicable)

## Step 2.1) Clip to variable of interest and crop to trial area
data.crop <- st_crop(data.raw[,variable],strips)
names(data.crop)[1] <- "target.variable"

if(clean.dat=="Yes"){ 
  Q1 <- quantile(data.crop$target.variable, 0.25)
  Q3 <- quantile(data.crop$target.variable, 0.75)
  
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  data.clean <- data.crop %>%
    filter(target.variable >= lower_bound & target.variable <= upper_bound)
  
}else{
  data.clean <- data.crop
}

###############################################################################
## Step 3) Drill treatments and zones
treat.drilled <- st_intersection(data.clean,strips[,treat.col.name])
zones.drilled <- terra::extract(zones,treat.drilled)
names(zones.drilled)[2] <- "zone"

all.dat <- na.omit(cbind(treat.drilled,zones.drilled[-1]))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%% General Stats - Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
## Step 4) Compute summary statistics for whole of field
df <- st_drop_geometry(all.dat)

control_group <- df %>% filter(!!sym(treat.col.name) == control.name) # Assuming Treat_Num 1 is the control group

summary_stats <- df %>%
  group_by(!!sym(treat.col.name)) %>%
  summarize(
    mean = mean(target.variable, na.rm = TRUE),
    sd = sd(target.variable, na.rm = TRUE),
    min = min(target.variable, na.rm = TRUE),
    max = max(target.variable, na.rm = TRUE),
    median = median(target.variable, na.rm = TRUE),
    target.variable = n()
  )

t_test_results <- df %>%
  filter(!!sym(treat.col.name) != control.name) %>% # Exclude the control group
  group_by(!!sym(treat.col.name)) %>%
  do(tidy(t.test(target.variable ~ !!sym(treat.col.name), data = rbind(control_group, .)))) %>%
  ungroup() %>%
  mutate(adj_p_value = p.adjust(p.value, method = "bonferroni"),
         significance = ifelse(adj_p_value <= 0.1, "Significant", "Not Significant"))

print(t_test_results)
print(summary_stats)

# Perform ANOVA
anova <- aov(as.formula(paste("target.variable", "~", treat.col.name)), data = df)
summary(anova)

# Tukey HSD post-hoc test
tukey <- TukeyHSD(anova)
tukey_results <- as.data.frame(tukey[treat.col.name])

# Get the significance letters from Tukey HSD results
letters <- multcompLetters4(anova, tukey)

# Convert to a dataframe
sig.out <- data.frame(
  treat = names(letters[[treat.col.name]]$Letters), 
  Significance = letters[[treat.col.name]]$Letters
)
names(sig.out)[1] <- treat.col.name

summary_stats.2 <- inner_join(summary_stats,sig.out, by  = treat.col.name)
print(summary_stats.2)

write.csv(summary_stats.2,paste0(headDir,'/10.Analysis/',analysis.yr,'/',analysis.type,'/Emergence/summary-stats-whole-pdk.csv'))

################################################################################
## Step 5) Make a ggplot

# Compute summary statistics (median, 25th, and 75th percentiles)
summary_stats <- df %>%
  group_by(!!sym(treat.col.name)) %>%
  summarise(
    median = median(target.variable, na.rm = TRUE),
    Q1 = quantile(target.variable, 0.25, na.rm = TRUE),
    Q3 = quantile(target.variable, 0.75, na.rm = TRUE)
  )

# Create the bar plot
site.bar.plot <- ggplot(summary_stats, aes(x = !!sym(treat.col.name), y = median, fill = !!sym(treat.col.name))) +
  geom_col(alpha = 0.7) +  # Bars for median values
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black") +  # Quartile error bars
  labs(
    title = "Plant Count by Treatment",
    x = "Treatment Description",
    y = "Plant Density (plants/m2)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    axis.title = element_text(size = 18),          # Increase axis label font size
    axis.text = element_text(size = 16),           # Axis text size (already set above, just making explicit)
    plot.title = element_text(size = 22, hjust = 0.5),  # Center and enlarge title
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Print the plot
site.bar.plot

ggsave(paste0(headDir,'/10.Analysis/',analysis.yr,'/',analysis.type,'/Emergence/emergence-plot-whole-pdk.png'),site.bar.plot)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%% By Zone Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

################################################################################
## Step 6) Compute summary statistics by zone

## SKIP THIS FOR WALPEUP MRS125 FOR THE 2025 SEASON - NOT ENOUGH DATA POINTS 

# for(i in 1:length(unique(all.dat$zone))){
#   df <- all.dat %>% 
#     filter(zone == i)
#   
#   control_group <- df %>% filter(!!sym(treat.col.name) == control.name) # Assuming Treat_Num 1 is the control group
#   
#   summary_stats <- df %>%
#     group_by(!!sym(treat.col.name)) %>%
#     summarize(
#       mean = mean(target.variable, na.rm = TRUE),
#       sd = sd(target.variable, na.rm = TRUE),
#       min = min(target.variable, na.rm = TRUE),
#       max = max(target.variable, na.rm = TRUE),
#       median = median(target.variable, na.rm = TRUE)
#     )
#   
#   t_test_results <- df %>%
#     filter(!!sym(treat.col.name) != control.name) %>% # Exclude the control group
#     group_by(!!sym(treat.col.name)) %>%
#     do(tidy(t.test(target.variable ~ !!sym(treat.col.name), data = rbind(control_group, .)))) %>%
#     ungroup() %>%
#     mutate(adj_p_value = p.adjust(p.value, method = "bonferroni"),
#            significance = ifelse(adj_p_value <= 0.1, "Significant", "Not Significant"))
#   
#   # Perform ANOVA
#   anova <- aov(as.formula(paste("target.variable", "~", treat.col.name)), data = df)
#   summary(anova)
#   
#   # Tukey HSD post-hoc test
#   tukey <- TukeyHSD(anova)
#   tukey_results <- as.data.frame(tukey[treat.col.name])
#   
#   # Get the significance letters from Tukey HSD results
#   letters <- multcompLetters4(anova, tukey)
#   
#   # Convert to a dataframe
#   sig.out <- data.frame(
#     treat = names(letters[[treat.col.name]]$Letters), 
#     Significance = letters[[treat.col.name]]$Letters
#   )
#   names(sig.out)[1] <- treat.col.name
#   
#   summary_stats.2 <- st_drop_geometry(inner_join(summary_stats,sig.out, by  = treat.col.name))
#   print(summary_stats.2)
# }



################################################################################
## Step 7) Plot by Zone
df <- all.dat
df$zone <- as.factor(df$zone)
df <- na.omit(df)
nrow(df)
#write.csv(df,paste0(headDir,'/7.In_Season_data/24/1.Emergence/emergence_rStructered.csv'))

summary_stats <- df %>%
  group_by(!!sym(treat.col.name), zone) %>%
  summarise(
    median = median(target.variable, na.rm = TRUE),
    Q1 = quantile(target.variable, 0.25, na.rm = TRUE),
    Q3 = quantile(target.variable, 0.75, na.rm = TRUE),
    .groups = "drop"
  )


zone.bar.plot <- summary_stats %>%
  dplyr::rename(zone_id = zone) %>%
  ggplot(aes(x = !!sym(treat.col.name), y = median, fill = !!sym(treat.col.name))) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black") +
  labs(
    title = "Plant Count by Treatment and Zone",
    x = NULL,
    y = "Plant Density (plants/m2)",
    fill = "Treatment"
  ) +
  facet_wrap(~ zone_id, scales = "free_x", labeller = zone_labeller) +
  theme_minimal() +
  theme(
    text = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 18, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    
    # legend at bottom (matching your NDVI plot)
    legend.position        = "bottom",
    legend.box             = "vertical",
    legend.justification   = "center"
  )+
  guides(
    fill = guide_legend(title.position = "top", title.hjust = 0.5)
  )


zone.bar.plot

ggsave(paste0(headDir,'/10.Analysis/',analysis.yr,'/',analysis.type,'/Emergence/emergence-by-zone.png'),zone.bar.plot)






















# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%% Spatial Modelling - Variable Importance %%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# ## Build a spatial yield model between covariates and crop yield
# dat.df <- st_drop_geometry(df)
# extracted.covs <- terra::extract(covariates,df)
# plot(covariates[[8]])
# 
# train.dat <- cbind(dat.df[,c("target.variable",treat.col.name)],extracted.covs[,-c(1)])
# train.dat[,treat.col.name] <- as.factor(train.dat[,treat.col.name])
# 
# train.dat$repellence <- as.factor(train.dat$repellence)
# 
# #Make other variables binary if required
# 
# ## prepare strips file but remove buffer
# buffer.idx <- which(strips$treat_desc=="Buffer")
# plots.sf <- strips[-buffer.idx,]
# plot(plots.sf)
# 
# ## Prepare prediction data
# covs.sf <- st_as_sf(as.points(covariates))
# covs.treat.sf <- st_join(covs.sf,plots.sf["treat_desc"])
# 
# covs.treat.sf <- covs.treat.sf %>%
#   select("treat_desc", everything())
# 
# covs.treat.df <- na.omit(st_drop_geometry(covs.treat.sf))
# covs.treat.df$treat_desc <- as.factor(covs.treat.df$treat_desc)
# covs.treat.df$repellence <- as.factor(covs.treat.df$repellence)
# 
# zones.sf <- st_as_sf(as.points(zones))
# zones.treat.sf <- st_join(zones.sf,plots.sf["treat_desc"])
# zones.treat.df <- na.omit(st_drop_geometry(zones.treat.sf))
# names(zones.treat.df)[1] <- "zone"
# 
# if(model=="Random Forest"){
#   
#   # covs.treat.sf <- st_join(covs.sf,plots.sf["treat_desc"])
#   # covs.treat.df <- na.omit(st_drop_geometry(covs.treat.sf))
#   # covs.treat.df$treat_desc <- as.factor(covs.treat.df$treat_desc)
#   # covs.treat.df$repellence <- as.factor(covs.treat.df$repellence)
#   # 
#   # covs.treat.coords <- na.omit(covs.treat.sf)
#   ##############################################################################
#   ## Train model - RF
#   tst <- train.dat
#   tst <- train.dat[seq(1, nrow(train.dat), by = 2), ]
#   
#   train.mdls <- fit_randomForest(inputV = tst[,2:ncol(tst)], 
#                                  TV = tst[,1],
#                                  nboots = 50, 
#                                  transform=1)
#   train.mdls$var.imp
#   sum(train.mdls$var.imp$X.incMSE_av)
#   
#   varout <- train.mdls$var.imp
#   
#   varimpplot <- ggplot(varout, aes(x = reorder(variable , X.incMSE_av), y = X.incMSE_av, fill = X.incMSE_av)) +
#     geom_bar(stat = "identity") +
#     coord_flip() +
#     scale_fill_gradient(low = "blue", high = "red") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
#     labs(title = "Variable Importance on Modelled Yield", x = "Features", y = "X.incMSE_av")
#   
#   varimpplot
#   
#   pred.data <- predict_randomForest(models=train.mdls$models,
#                                     newdat=covs.treat.df,
#                                     nboots=50, 
#                                     PI=c(0.1,0.9), 
#                                     transform = 1)
#   
#   pred.dat.out <- covs.treat.df
#   pred.dat.out$target.variable <- pred.data$pred.median
#   pred.dat.out$zone <- zones.treat.df[,1]
#   
# }else{
#   #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#   # Train model - QGB
#   train.dat.xgb <- as.data.frame(lapply(train.dat, function(x) if (is.factor(x) || is.character(x)) as.numeric(as.factor(x)) else x))
#   covs.treat.df.xgb <- as.data.frame(lapply(covs.treat.df, function(x) if (is.factor(x) || is.character(x)) as.numeric(as.factor(x)) else x))
#   
#   train.mdls <- fit_xgBoost(inputV = train.dat.xgb[,2:ncol(train.dat.xgb)], 
#                             TV = train.dat.xgb[,1],
#                             nboots = 50, 
#                             transform=1)
#   
#   
#   varout <- train.mdls[[6]]
#   
#   
#   varimpplot <- ggplot(varout, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
#     geom_bar(stat = "identity") +
#     coord_flip() +
#     scale_fill_gradient(low = "blue", high = "red") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
#     labs(title = "Variable Importance on Modelled Yield", x = "Features", y = "Gain")
#   
#   varimpplot
#   
#   
#   pred.data <- predict_xgBoost(models=train.mdls$models,
#                                newdat=covs.treat.df.xgb,
#                                nboots=50, 
#                                PI=c(0.1,0.9), 
#                                transform = 1)
#   
#   pred.dat.out <- covs.treat.df
#   pred.dat.out$target.variable <- pred.data$pred.median
#   pred.dat.out$zone <- zones.treat.df[,1]
#   
# }
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%%   General Stats - Modelled Data    %%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# planet.em <- rast("//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/1.Walpeup_MRS125/7.In_Season_data/24/2.Satellite_Imagery/Planet/ndvi_emergence_20240528.tif")
# 
# planet.em <- terra::project(planet.em,'EPSG:4326')
# 
# highres.all.em <- rast("//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/1.Walpeup_MRS125/7.In_Season_data/24/2.Satellite_Imagery/50cm/Capture_1_Walpeup/CSIRO_Walpeup_Trial_Capture_50cm_20240526T0400_Coreg.tif")
# names(highres.all.em) <- c("Red","Green","Blue","NIR")
# highres.em <- (highres.all.em$NIR - highres.all.em$Red) / (highres.all.em$NIR + highres.all.em$Red)
# names(highres.em) <- "ndvi"
# #writeRaster(highres.em,"//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/1.Walpeup_MRS125/7.In_Season_data/24/2.Satellite_Imagery/50cm/ndvi_emergence.tif",overwrite=T)
# 
# plot(planet.em)
# plot(highres.em)
# 
# planet.ex <- terra::extract(planet.em,emergence.dat)
# highres.ex <- terra::extract(highres.em,emergence.dat)
# names(planet.ex)[2] <- 'planet.ndvi'
# names(highres.ex)[2] <- 'highres.ndvi'
# 
# em.sat.dat <- cbind(emergence.dat,planet.ex$planet.ndvi,highres.ex$highres.ndvi)
# 
# plot(em.sat.dat$planet.ex.planet.ndvi,em.sat.dat$total_count)
# plot(em.sat.dat$highres.ex.highres.ndvi,em.sat.dat$total_count)
# 
# 
# #Do we:
# #1) Compute stats on raw field observations (96 obs)
# #2) On modelled biomass using only the NDVI
# #2) On modelled biomass using only the NDVI + covs
# 
# ## Get image closest to emergence counts
# ## emergence count date = 01/07/2024
# 
# sat.img.50 <- rast(paste0(headDir,"/7.In_Season_data/24/2.Satellite_Imagery/1.High-res/1.Emergence/CSIRO_CrystalBrook_Trial_Capture_50cm_20240704T2308.tif"))
# names(sat.img.50) <- c("Red","Green","Blue","NIR")
# 
# sat.img.ndvi.50 <- (sat.img.50$NIR - sat.img.50$Red) / (sat.img.50$NIR + sat.img.50$Red)
# plot(sat.img.ndvi.50)
# sat.img.ndvi.50.4326 <- terra::project(sat.img.ndvi.50,'epsg:4326')
# names(sat.img.ndvi.50.4326) <- "ndvi"
# 
# sat.img.planet <- rast(paste0(headDir,"/7.In_Season_data/24/2.Satellite_Imagery/Planet/WalpaupSat3m12Oct_3April2024_Planet_psscene_analytic_8b_sr_udm2/PSScene/20240528_235808_62_2456_3B_AnalyticMS_SR_8b_clip.tif"))
# sat.img.planet.ndvi <- (sat.img.planet$nir - sat.img.planet$red) / (sat.img.planet$nir + sat.img.planet$red)
# names(sat.img.planet.ndvi) <- "ndvi"
# 
# plot(sat.img.planet.ndvi)
# sat.img.planet.ndvi.4326 <- terra::project(sat.img.planet.ndvi,'epsg:4326')
# 
# writeRaster(sat.img.planet.ndvi,paste0(headDir,'/7.In_Season_data/24/2.Satellite_Imagery/Planet/ndvi_emergence_20240528.tif'))
# 
# ## Create basic corellation between sampling points and ndvi
# 
# ndvi.ex <- terra::extract(sat.img.planet.ndvi.4326,all.dat)
# ndvi.df <- as.data.frame(sat.img.planet.ndvi.4326, xy = TRUE, na.rm = TRUE)
# 
# ndvi.biomass.dat <- data.frame(target.variable = all.dat$target.variable,
#                                ndvi = ndvi.ex$ndvi)
# 
# plot(ndvi.biomass.dat)
# 
# ## Combine with all other data
# ndvi.biomass.dat.all <- cbind(train.dat,ndvi.biomass.dat)
# 
# #### Create 2 models - One just with the ndvi, one with ndvi plus covs
# 
# #### 1 - NDVI ONLY 
# train.mdls <- fit_linearModel(inputV = ndvi.biomass.dat.all[,ncol(ndvi.biomass.dat.all)], 
#                               TV = ndvi.biomass.dat.all[,1],
#                               nboots = 50, 
#                               transform=1)
# 
# train.mdls$cal_rmse
# train.mdls$oob_rmse
# 
# #### Predict across whole of site
# sat.dat <- as.data.frame(sat.img.planet.ndvi.4326,xy=T)
# 
# 
# 
# 
# 
# 
# #### 1 - All covs (including ndvi)
# train.mdls <- fit_linearModel(inputV = ndvi.biomass.dat.all[,2:ncol(ndvi.biomass.dat.all)], 
#                               TV = ndvi.biomass.dat.all[,1],
#                               nboots = 50, 
#                               transform=1)
# 
# train.mdls$cal_rmse
# train.mdls$oob_rmse
# 
# 
# ## Create modelled dataset
# 
# l.mdl <- lm(target.variable ~ ndvi, data = ndvi.biomass.dat)
# summary(l.mdl)$r.squared  # Extract R^2 value
# summary(l.mdl)
# 
# 
# model <- lm(target.variable ~ ndvi, data = ndvi.biomass.dat)  # Replace df with your dataframe
# summary(model)$r.squared  # Extract R^2 value
# 
# pred.out <- predict(model,newdata = ndvi.df)  
# pred.out.df <- cbind(ndvi.df[,c("x","y")],pred.out)
# pred.out.sf <- st_as_sf(pred.out.df,coords = c("x","y"),crs=4326)
# names(pred.out.sf)[1] <- 'target.variable'
# 
# emergence.rast <- rast(pred.out.df, type = "xyz")
# crs(emergence.rast) <- "EPSG:4326"  # WGS84
# plot(emergence.rast)
# writeRaster(emergence.rast,paste0(headDir,'/10.Analysis/24/Emergence/emergence_calibration.tif'),overwrite=T)
# 
# 
# 
# 
# 
# 
# 
# ##########################################
# #####################################
# ## Step 3) Drill treatments and zones
# pred.out.sf.crop <- st_crop(pred.out.sf,strips)
# 
# treat.drilled <- st_intersection(pred.out.sf,strips[,treat.col.name])
# zones.drilled <- terra::extract(zones,treat.drilled)
# names(zones.drilled)[2] <- "zone"
# covs.drilled <- terra::extract(covariates,treat.drilled)
# 
# all.dat <- na.omit(cbind(treat.drilled,zones.drilled[-1],covs.drilled[-1]))
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%% General Stats - Observed Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# ################################################################################
# ## Step 4) Compute summary statistics for whole of field
# df <- all.dat
# 
# ## Remove Outside control and buffer
# df <- df %>%
#   filter(!(treat_desc %in% c(buffer.name, "Outside Control")))
# 
# control_group <- df %>% filter(!!sym(treat.col.name) == control.name) # Assuming Treat_Num 1 is the control group
# 
# summary_stats <- df %>%
#   group_by(!!sym(treat.col.name)) %>%
#   summarize(
#     mean = mean(target.variable, na.rm = TRUE),
#     sd = sd(target.variable, na.rm = TRUE),
#     min = min(target.variable, na.rm = TRUE),
#     max = max(target.variable, na.rm = TRUE),
#     median = median(target.variable, na.rm = TRUE),
#     target.variable = n()
#   )
# 
# t_test_results <- df %>%
#   filter(!!sym(treat.col.name) != control.name) %>% # Exclude the control group
#   group_by(!!sym(treat.col.name)) %>%
#   do(tidy(t.test(target.variable ~ !!sym(treat.col.name), data = rbind(control_group, .)))) %>%
#   ungroup() %>%
#   mutate(adj_p_value = p.adjust(p.value, method = "bonferroni"),
#          significance = ifelse(adj_p_value <= 0.1, "Significant", "Not Significant"))
# 
# print(t_test_results)
# print(summary_stats)
# 
# 
# # Perform ANOVA
# anova <- aov(as.formula(paste("target.variable", "~", treat.col.name)), data = df)
# summary(anova)
# 
# # Tukey HSD post-hoc test
# tukey <- TukeyHSD(anova)
# tukey_results <- as.data.frame(tukey[treat.col.name])
# 
# # Get the significance letters from Tukey HSD results
# letters <- multcompLetters4(anova, tukey)
# 
# # Convert to a dataframe
# sig.out <- data.frame(
#   treat = names(letters[[treat.col.name]]$Letters), 
#   Significance = letters[[treat.col.name]]$Letters
# )
# names(sig.out)[1] <- treat.col.name
# 
# summary_stats.2 <- inner_join(summary_stats,sig.out, by  = treat.col.name)
# print(summary_stats.2)
# 
# ################################################################################
# ## Step 5) Make a ggplot
# 
# # Compute summary statistics (median, 25th, and 75th percentiles)
# summary_stats <- df %>%
#   group_by(!!sym(treat.col.name)) %>%
#   summarise(
#     median = median(target.variable, na.rm = TRUE),
#     Q1 = quantile(target.variable, 0.25, na.rm = TRUE),
#     Q3 = quantile(target.variable, 0.75, na.rm = TRUE)
#   )
# 
# # Create the bar plot
# site.bar.plot <- ggplot(summary_stats, aes(x = !!sym(treat.col.name), y = median, fill = !!sym(treat.col.name))) +
#   geom_col(alpha = 0.7) +  # Bars for median values
#   geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2, color = "black") +  # Quartile error bars
#   labs(
#     title = "Target Variable by Treatment",
#     x = "Treatment Description",
#     y = "Target Variable (units)"
#   ) +
#   theme_minimal() +  # Clean theme
#   theme(
#     text = element_text(size = 14),  # Adjust font size
#     legend.position = "none",  # Remove legend if not needed
#     axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for clarity
#   )
# 
# # Print the plot
# site.bar.plot
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# library(ggplot2)
# library(dplyr)
# 
# df <- pred.dat.out
# df$zone <- as.factor(df$zone)
# 
# # Compute summary statistics
# summary_stats_v2 <- df %>%
#   group_by(zone, !!sym(treat.col.name)) %>%
#   summarise(
#     median = median(target.variable, na.rm = TRUE),
#     Q1 = quantile(target.variable, 0.25, na.rm = TRUE),
#     Q3 = quantile(target.variable, 0.75, na.rm = TRUE)
#   ) %>%
#   mutate(IQR_lower = Q1, IQR_upper = Q3)
# 
# # Create bar plot with IQR whiskers
# zone.bar.plot <- ggplot(summary_stats_v2, aes(x = !!sym(treat.col.name), y = median, fill = !!sym(treat.col.name))) +
#   geom_col(alpha = 0.7) +
#   geom_errorbar(aes(ymin = IQR_lower, ymax = IQR_upper), width = 0.2) +
#   labs(
#     title = "Results by Treatment and Zone",
#     x = NULL,  
#     y = "Biomass (kg/ha)",
#     fill = "Treatment"
#   ) +
#   facet_wrap(~ zone, scales = "free_x") +  
#   theme_minimal() +
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_blank(),  
#     axis.ticks.x = element_blank()  
#   )
# 
# zone.bar.plot
# 
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%% Plotting of Modeled Results %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# filtered_data_raw <- all.dat %>%
#   filter(treat_desc == "Control", zone == 2)
# filtered_data_raw
# 
# filtered_data_mdl <- pred.dat.out %>%
#   filter(treat_desc == "Control", zone == 2)
# filtered_data_mdl
# 
# filtered_data_bio <- biomass.all.df %>%
#   filter(treat_desc == "Control", zone == 2)
# filtered_data_bio
# 
# mean(filtered_data_raw$target.variable)
# mean(filtered_data_mdl$target.variable)
# mean(filtered_data_bio$target.variable)
# 
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%% Biomass data investigation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# 
# 
# ##############################################################################
# ##############################################################################
# ##############################################################################
# 
# biomass.map <- rast(paste0(headDir,"/7.In_Season_data/24/4.Biomass/Walpeup_Biomass_map/Walpeup_Biomass_map_NDVI50cm/Walpeup_Biomass_Estimation_50cm_T3.tif"))
# biomass.map.crop <- mask(crop(biomass.map,boundary),boundary)
# writeRaster(biomass.map.crop,paste0(headDir,"/7.In_Season_data/24/4.Biomass/Walpeup_Biomass_map/Walpeup_Biomass_map_NDVI50cm/Walpeup_Biomass_Estimation_50cm_T3_MASK.tif"))
# 
# yield.biomass.map <- (0.2087*biomass.map)+38.544
# 
# biomas.res <- terra::resample(biomass.map,covariates)
# names(biomas.res) <- "target.variable"
# 
# biomass.ex <- terra::extract(biomas.res,covs.treat.sf)
# biomass.all <- cbind(covs.treat.sf,biomass.ex)
# 
# biomass.all.df <- na.omit(st_drop_geometry(biomass.all))
# nrow(biomass.all.df)
# 
# biomass.all.df$zone <- zones.treat.df[,1]
# 
# 
# df <- biomass.all.df
# df$zone <- as.factor(df$zone)
# 
# # Compute summary statistics
# summary_stats_v2 <- df %>%
#   group_by(zone, !!sym(treat.col.name)) %>%
#   summarise(
#     median = median(target.variable, na.rm = TRUE),
#     Q1 = quantile(target.variable, 0.25, na.rm = TRUE),
#     Q3 = quantile(target.variable, 0.75, na.rm = TRUE)
#   ) %>%
#   mutate(IQR_lower = Q1, IQR_upper = Q3)
# 
# # Create bar plot with IQR whiskers
# zone.bar.plot <- ggplot(summary_stats_v2, aes(x = !!sym(treat.col.name), y = median, fill = !!sym(treat.col.name))) +
#   geom_col(alpha = 0.7) +
#   geom_errorbar(aes(ymin = IQR_lower, ymax = IQR_upper), width = 0.2) +
#   labs(
#     title = "Results by Treatment and Zone",
#     x = NULL,  
#     y = "Yield (t/ha)",
#     fill = "Treatment"
#   ) +
#   facet_wrap(~ zone, scales = "free_x") +  
#   theme_minimal() +
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_blank(),  
#     axis.ticks.x = element_blank()  
#   )
# 
# zone.bar.plot
# 
# 
# 
# 
# tst <- biomass.all.df
# tst <- biomass.all.df[seq(1, nrow(train.dat), by = 2), ]
# tst$treat_desc <- as.factor(tst$treat_desc)
# tst$repellence <- as.factor(tst$repellence)
# 
# train.mdls <- fit_randomForest(inputV = tst[,-c(11,12,13)], 
#                                TV = tst[,12],
#                                nboots = 50, 
#                                transform=1)
# train.mdls$var.imp
# sum(train.mdls$var.imp$X.incMSE_av)
# 
# varout <- train.mdls$var.imp
# 
# varimpplot <- ggplot(varout, aes(x = reorder(variable , X.incMSE_av), y = X.incMSE_av, fill = X.incMSE_av)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   scale_fill_gradient(low = "blue", high = "red") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
#   labs(title = "Variable Importance on Modelled Yield", x = "Features", y = "X.incMSE_av")
# 
# varimpplot
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# treats.to.test <- c("Control","Rip + Lime (3t)","Spade + Lime (3t)")
# 
# for(k in 1:length(treats.to.test)){
#   treat.k <- treats.to.test[k]
#   
#   testing.dat.sf <- covs.sf
#   
#   testing.dat.sf$treat_desc <- rep(as.factor(treat.k),nrow(testing.dat.sf))
#   testing.dat.sf$treat_desc <- factor(testing.dat.sf$treat_desc, levels = levels(covs.treat.df$treat_desc))
#   
#   testing.dat.df <- na.omit(st_drop_geometry(testing.dat.sf))
#   testing.dat.df$treat_desc <- as.factor(testing.dat.df$treat_desc)
#   testing.dat.df$repellence <- as.factor(testing.dat.df$repellence)
#   nrow(testing.dat.df)
#   
#   #levels(testing.dat.df$treat_desc) <- levels(covs.treat.df$treat_desc)
#   covs.test.coords <- na.omit(testing.dat.sf)
#   
#   nrow(testing.dat.df)
#   head(testing.dat.df)
#   
#   
#   testing.dat.df <- testing.dat.df %>%
#     select("treat_desc", everything())
#   
#   names(testing.dat.df) <- names(tst[,-c(11,12,13)])
#   
#   pred.test.data <- predict_randomForest(models=train.mdls$models,
#                                          newdat=testing.dat.df,
#                                          nboots=50, 
#                                          PI=c(0.1,0.9), 
#                                          transform = 1)
#   
#   nrow(pred.test.data)
#   pred.data.1 <- cbind(pred.test.data,covs.test.coords$geometry)
#   pred.data.sf.2 <- st_as_sf(pred.data.1)
#   plot(pred.data.sf.2)
#   head(pred.data.sf.2)
#   
#   preds.xyz <- cbind(st_coordinates(pred.data.sf.2),st_drop_geometry(pred.data.sf.2)$pred.median)
#   names(preds.xyz)[3] <- "pred"
#   control.preds.rast <- rast(preds.xyz, type = "xyz")
#   plot(control.preds.rast)
#   names(control.preds.rast) <- treat.k
#   if(k==1){
#     all.out <- control.preds.rast
#   }else{
#     all.out <- c(all.out,control.preds.rast)
#   }
#   
#   print(k)
# }
# 
# plot(all.out)
# 
# 
# for (k in 1:length(treats.to.test)){
#   treat.k <- treats.to.test[k]
#   
#   if(treat.k==control.name){#Do nothing
#   }else{
#     #treat.diff <- all.out$`Rip + Lime (3t)` - all.out$Control
#   }
#   
# }
# 
# rip.lime.diff <- all.out$`Rip + Lime (3t)` - all.out$Control
# plot(rip.lime.diff)
# 
# spade.lime.diff <- all.out$`Spade + Lime (3t)` - all.out$Control
# plot(spade.lime.diff)
# 
# rip.lime.poly <- strips %>%
#   filter(treat_desc == "Rip + Lime (3t)")
# 
# spade.lime.poly <- strips %>%
#   filter(treat_desc == "Spade + Lime (3t)")
# 
# 
# crs(rip.lime.diff) <- "EPSG:4326"
# rip.lime.diff.mask <- mask(crop(rip.lime.diff,rip.lime.poly),rip.lime.poly)
# plot(rip.lime.diff.mask)
# 
# crs(spade.lime.diff) <- "EPSG:4326"
# spade.lime.diff.mask <- mask(crop(spade.lime.diff,spade.lime.poly),spade.lime.poly)
# plot(spade.lime.diff.mask)
# 
# writeRaster(rip.lime.diff.mask,paste0(headDir,'/10.Analysis/24/InSeason/Biomass/modelledBiomass/Treat_Zone_Response_Raster/Rip + Lime (3t).tif'),overwrite=T)
# writeRaster(spade.lime.diff.mask,paste0(headDir,'/10.Analysis/24/InSeason/Biomass/modelledBiomass/Treat_Zone_Response_Raster/Spade + Rip + Lime (3t).tif'),overwrite=T)
# 
# 
# summary(treat.diff)
# 
# 
# masked_raster <- treat.diff
# masked_raster[treat.diff <300] <- 0
# masked_raster[treat.diff >=300] <- 1
# plot(masked_raster)
# 
# (0.2087*300) + 38.544
# 
# 
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%%%%%%%% Spatial responses  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# summary_stats_v2
# nrow(summary_stats_v2)
# nrow(biomass.all.df)
# str(strips)
# 
# biomass.coords <- st_coordinates(na.omit(biomass.all))
# 
# stripsv2 <- strips
# 
# # Convert the raster to vector polygons (each zone becomes a polygon)
# zones_polygons <- as.polygons(zones, dissolve = TRUE) %>% st_as_sf()
# names(zones_polygons)[1] <- "zone"
# 
# # Step 1: Spatial intersection to split polygons by zone
# trial_polygons_zoned <- st_intersection(strips, zones_polygons)
# trial_polygons_zoned$zone <- as.factor(trial_polygons_zoned$zone)
# 
# # Step 3: Ensure the biomass data (`summary_stats_v2`) has Control treatment data per zone
# control_biomass <- summary_stats_v2 %>%
#   filter(treat_desc == "Control") %>%
#   select(zone, control_median_biomass = median)
# 
# # Step 4: Compute yield difference between Control and each treatment in the same zone
# treatment_biomass <- summary_stats_v2 %>%
#   left_join(control_biomass, by = "zone") %>%
#   mutate(yield_difference = median - control_median_biomass) %>%
#   select(treat_desc, zone, yield_difference)
# 
# # Step 5: Merge yield differences back into the newly split polygons
# trial_polygons_zoned <- trial_polygons_zoned %>%
#   left_join(treatment_biomass, by = c("treat_desc", "zone"))
# 
# plot(trial_polygons_zoned)
# 
# st_write(trial_polygons_zoned,paste0(headDir,'//10.Analysis/24/InSeason/Biomass/modelledBiomass/Treat_Zone_Response_Polygon/all-treats.gpkg'))
# 
# for(j in 1:length(unique(trial_polygons_zoned$treat_desc))){
#   treat.j <- unique(trial_polygons_zoned$treat_desc)[j]
#   
#   treat.j.idx <- which(trial_polygons_zoned$treat_desc==treat.j)
#   
#   treat.j.poly <- trial_polygons_zoned[treat.j.idx,]
#   st_write(treat.j.poly,paste0(headDir,'/10.Analysis/24/InSeason/Biomass/modelledBiomass/Treat_Zone_Response_Polygon/',treat.j,'.gpkg'))
#   
# }

