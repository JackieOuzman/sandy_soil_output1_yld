#useful code for strip plot

orientation_df <- read.csv(paste0(metadata_path, "ladder_orientation_all_sites.csv"))
# Pull labels for current site
start_label <- orientation_df$start_label[orientation_df$site == site_number]
end_label   <- orientation_df$end_label[orientation_df$site == site_number]



y_max <- max(combined$mean_yld, na.rm = TRUE)
x_max <- max(combined$Ladder_PointID * 10, na.rm = TRUE)

ggplot(combined, aes(x = Ladder_PointID * 10, y = mean_yld, color = treat_desc)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ facet) +
  scale_color_manual(values = treatment_colour) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Yield down strip", 
       # subtitle = "Control is grey",
       caption = "Control is reference line in grey *NB at MRS125 Control (-Tillage -Lime)",
       x = "Distance along strip (m)", 
       y = "Mean yield (t/ha)") +
  annotate("text",
           x = 0,
           y = y_max,
           label = start_label,
           fontface = "bold",
           size = 4,
           hjust = 0,
           vjust = 1) 