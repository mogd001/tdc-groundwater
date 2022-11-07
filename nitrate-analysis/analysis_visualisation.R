library(tdcR)
library(tidyverse)
library(sf)
library(lubridate)
library(ggmap)
library(patchwork)
library(trend)
library(Kendall)
library(xts)
library(zoo)
library(glue)
library(ggforce)

##### Nitrate Analysis #####
#
# Matt Ogden, September/October 2022
#
# Waimea Plains Nitrate Analysis - Visualisaton
#
##################
# source("data_wrangling.R")
source("other.R") # for mid_date function

aquifer_levels <- c("UCA", "LCA", "AGUA", "HU")
aquifer_colors <- c("green", "blue", "orange", "magenta")
aquifer_shape <- c(2, 6, 1, 0)

aquifer_data <- read_csv("data/20220915_waimea_nitrate_sites.csv") %>%
  mutate(aquifer = factor(aquifer, levels = aquifer_levels))

study_area <- st_read("data/aquifer_data.gpkg", layer = "study_area")
aquifers <- st_read("data/aquifer_data.gpkg", layer = "aquifers") %>%
  mutate(aquifer = factor(aquifer, levels = aquifer_levels))

nitrate_data <- readRDS("data/nitrate.rds") %>%
  filter(!is.na(easting) & !is.na(northing)) %>%
  mutate(
    year = year(sample_taken_on),
    month = month(sample_taken_on),
    sample_taken_on = as.Date(sample_taken_on)
  ) %>%
  rename(
    value = test_result_numeric,
    unit = test_units,
    sample_date = sample_taken_on
  ) %>%
  left_join(aquifer_data, by = "bore_id") %>%
  st_as_sf(coords = c("easting", "northing"), crs = 2193)

# Save for visualisation in  GIS
st_write(nitrate_data, "data/nitrate_data.gpkg", "nitrate_observations", append = FALSE)

# Filter just Waimea Plains Data
waimea_nitrate_data <- nitrate_data %>%
  filter(zone_id == 19 & !is.na(aquifer)) %>% # filter to waimea plains and with aquifer
  st_transform(crs = 4326) %>%
  mutate(
    lon = st_coordinates(.)[, "X"],
    lat = st_coordinates(.)[, "Y"]
  ) %>%
  st_transform(crs = 2193) %>%
  mutate(
    easting = st_coordinates(.)[, "X"],
    northing = st_coordinates(.)[, "Y"]
  )

st_write(waimea_nitrate_data, "data/waimea_nitrate_data.gpkg", "waimea_nitrate_observations", append = FALSE)

# Map waimea observation points
study_area_wgs84 <- st_transform(study_area, crs = 4326)
centre <- st_coordinates(st_centroid(study_area_wgs84))
bbox <- st_bbox(study_area_wgs84)

basemap <- get_map(location = c(lon = centre[, "X"], lat = centre[, "Y"]), maptype = "terrain-background", zoom = 11, alpha = 0.3) # Richmond, NZ (alternative for getting location)

nitrate_map <- ggmap(basemap, darken = c(0.6, "white")) +
  coord_cartesian() +
  geom_sf(study_area_wgs84, mapping = aes(), fill = NA, color = "black", inherit.aes = FALSE) +
  geom_jitter(waimea_nitrate_data, mapping = aes(lon, lat, color = aquifer, shape = aquifer), size = 2) +
  scale_color_manual(values = aquifer_colors) +
  scale_shape_manual(values = aquifer_shape) + 
  labs(color = "Aquifer") +
  coord_sf(
    xlim = c(bbox["xmin"] - 0.02, bbox["xmax"] + 0.02),
    ylim = c(bbox["ymin"] - 0.02, bbox["ymax"] + 0.02)
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = c(0.06, 0.9)
  ) + 
  guides(shape = "none")

nitrate_map

ggsave("nitrate_map.png", plot = nitrate_map, width = 7, height = 7)

# Inspect data
unique(waimea_nitrate_data$test_name)
unique(sort(waimea_nitrate_data$year))
unique(waimea_nitrate_data$site)
unique(waimea_nitrate_data$aquifer)

# Some "sites" are combinations of multiple sites, so we merge for analysis and use site_for_analysis as our grouping variable
site_merge_for_analysis <- tribble(
  ~site, ~analysis_site,
  "GW 471", "GW 37 - Gardner"
)

waimea_nitrate_data <- waimea_nitrate_data %>%
  mutate(
    site_for_analysis = if_else(site %in% unique(site_merge_for_analysis$site), filter(site_merge_for_analysis, site == site)$analysis_site, site),
    year_quarter = paste0(year(sample_date), "Q", quarter(sample_date))
  )

analysis_data <- waimea_nitrate_data %>%
  group_by(site_for_analysis, year_quarter) %>% # resample data to quarterly medians
  summarise(value = median(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    q_date_start = as.Date(as.yearqtr(year_quarter, format = "%YQ%q")),
    q_date_end = as.Date(as.yearqtr(year_quarter, format = "%YQ%q"), frac = 1),
    date = mid_date(q_date_start, q_date_end) # midpoint
  )

trend_data <- analysis_data %>%
  group_by(site_for_analysis) %>%
  filter(n() > 4) %>%
  summarise(
    n_records = n(),
    mann_kendall_tau = MannKendall(xts(value, date))$tau,
    mann_kendall_sl = MannKendall(xts(value, date))$sl # get mann-kendall values
  ) %>%
  select(site_for_analysis, n_records, mann_kendall_tau, mann_kendall_sl) %>%
  st_set_geometry(NULL) %>%
  mutate(
    trend_direction = ifelse(mann_kendall_tau < 0.1, "Decreasing",
      ifelse(mann_kendall_tau > 0.1, "Increasing", "None")
    ),
    trend_confidence = ifelse(mann_kendall_sl < 0.01, "Very strong",
      ifelse(mann_kendall_sl < 0.1, "Strong", "Indeterminate")
    )
  )

trend_data <- select(waimea_nitrate_data, site_for_analysis) %>%
  st_set_geometry(NULL) %>%
  unique() %>%
  left_join(trend_data, by = "site_for_analysis")

write_csv(trend_data, "outputs/trend_data.csv")
st_write(trend_data, "outputs/trend_data.gpkg", "trend_calculations", append = FALSE)

# Create trend plots for each site
plot_trend_by_site <- function(site, waimea_nitrate_data, analysis_data) {
  meas_data <- filter(waimea_nitrate_data, site_for_analysis == !!site)
  analysis_data_sample <- filter(analysis_data, site_for_analysis == !!site)

  # Plot Nitrate results by Site and Aquifer for 10 most sampled sites
  plot_site <- ggplot() +
    geom_point(data = meas_data, aes(x = sample_date, y = value, color = "Actual measurement"), alpha = 0.5, size = 1, shape = 1) +
    geom_point(data = analysis_data_sample, aes(x = date, y = value, color = "Quarterly resample"), size = 3, shape = 4) +
    geom_smooth(data = analysis_data_sample, aes(x = date, y = value), method = "lm", size = 0.5, color = "black") +
    geom_hline(yintercept = 11.3, linetype = "dashed", color = "red", size = 1) +
    annotate("text", x = ymd("19800101"), y = 11.3, label = expression(DWSNZ ~ MAV ~ 11.3 ~ (g / m^{
      3
    })), vjust = -0.25, hjust = -0.1, angle = 0, color = "red", size = 3) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    scale_y_continuous(limits = c(0, 30), expand = c(NA, 1)) +
    scale_color_manual(values = c("Actual measurement" = "blue", "Quarterly resample" = "red")) +
    labs(x = "", y = expression(NO[3] - N ~ (g / m^{
      3
    }))) +
    theme_bw() +
    guides(color = guide_legend(title = "Legend", override.aes = list(alpha = c(0.5, 1), size = c(1, 3), shape = c(1, 4)))) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    facet_grid(~site_for_analysis)

  ggsave(glue("outputs/plots/{site}_trend_plot.png"), plot = plot_site, width = 10, height = 7)
}

sites <- unique(trend_data$site_for_analysis)
lapply(sites, plot_trend_by_site, waimea_nitrate_data, analysis_data)

# Paginated plot for all sites, by aquifer
summary_plot_data <- waimea_nitrate_data %>% 
  filter(!is.na(aquifer) & !is.na(site_for_analysis)) %>% 
  arrange(as.numeric(bore_no))
summary_plot_data$bore_no <- factor(as.numeric(summary_plot_data$bore_no), ordered = TRUE, labels = glue("WWD~{head(unique(summary_plot_data$bore_no), -1)}"))

# Plotting shapes for each aquifer
aquifer_shapes <- c(6, 0, 1, 2)
names(aquifer_shapes) <- unique(summary_plot_data$aquifer)

for (aquifer in unique(summary_plot_data$aquifer)) {
  
  aquifer_summary_plot_data <- summary_plot_data %>% 
    filter(aquifer == !!aquifer)
  
  s <- aquifer_shapes[aquifer]
  
  gg <- ggplot(aquifer_summary_plot_data) +
    geom_point(aes(sample_date, value), size = 1.5, shape = s, alpha = 0.7) +
    geom_hline(yintercept = 11.3, linetype = "dashed", color = "red", size = 0.5) +
    annotate("text", x = ymd("19700101"), y = 11.3, label = expression(DWSNZ ~ MAV ~ 11.3 ~ (g / m^{
      3
    })), vjust = -0.25, hjust = -0.1, angle = 0, color = "red", size = 2) +
    scale_x_date(limits = c(ymd("19700101"), ymd("20250101")), expand = c(0,0), date_labels = "%Y", date_breaks = "10 years") +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "Sample date", y = expression(NO[3] - N ~ (g / m^{3})), title = glue("Aquifer {aquifer}")) + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    facet_wrap_paginate(~bore_no, ncol = 2, nrow = 5, page = 1)
  
  n <- n_pages(gg)
  
  pdf(glue("{aquifer}_Nitrate_Sampling_Summary.pdf"), paper= 'A4', w= 210/20, 297/20)
  for(i in 1:n){
    print(gg + facet_wrap_paginate(~bore_no, labeller = label_parsed, ncol = 2, nrow = 5, page = i))
  }
  dev.off()
}