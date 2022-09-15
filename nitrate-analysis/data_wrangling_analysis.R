library(tdcR)
library(tidyverse)
library(sf)
library(lubridate)
library(ggmap)
library(patchwork)

##### Nitrate Analysis #####
#
# Matt Ogden, September 2022
#
# Waimea Plains Nitrate Analysis.
#
##################

aquifer_levels <- c("UCA", "LCA", "AGUA", "HU")
aquifer_colors <- c("green", "blue", "orange", "magenta")

aquifer_data <- read_csv("data/20220915_waimea_nitrate_sites.csv") %>% 
  mutate(aquifer = factor(aquifer, levels = aquifer_levels))

study_area <- st_read("data/aquifer_data.gpkg", layer = "study_area")
aquifers <- st_read("data/aquifer_data.gpkg", layer = "aquifers") %>% 
  mutate(aquifer = factor(aquifer, levels = aquifer_levels))

bore_data <- get_bore_data_envmon()
lab_data <- get_lab_data_envmon()

# Pull only variables of interest
bore_data <- bore_data %>%
  select(bore_id, bore_no, easting, northing, zone_id, bore_depth, site_id, primary_use) # take location from the bore, "aquifer" also avaiable

nitrate_data <- lab_data %>%
  filter(test_name_group == "Nitrates") %>%
  select(test_name, test_result_numeric, test_units, sample_taken_on, site_id, site) %>%
  left_join(bore_data, by = "site_id") 

rm(lab_data, bore_data)

saveRDS(nitrate_data, "data/nitrate.rds")

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

# Save for visualisation in  GIS program
st_write(nitrate_data, "data/nitrate_data.gpkg", "nitrate_observations", append = FALSE)

# Analysis
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

# map points
study_area_wgs84 <- st_transform(study_area, crs = 4326)
centre <- st_coordinates(st_centroid(study_area_wgs84))
bbox <- st_bbox(study_area_wgs84)

basemap <- get_map(location = c(lon = centre[, "X"], lat = centre[, "Y"]), maptype = "terrain-background", zoom = 11, alpha = 0.3) # Richmond, NZ (alternative for getting location)

nitrate_map <- ggmap(basemap, darken = c(0.6, "white")) +
  coord_cartesian() + 
  geom_sf(study_area_wgs84, mapping = aes(), fill = NA, color = "black", inherit.aes = FALSE) +
  geom_jitter(waimea_nitrate_data, mapping = aes(lon, lat, color = aquifer), size = 2) +
  scale_color_manual(values = aquifer_colors) + 
  labs(color = "Aquifer") + 
  coord_sf(
    xlim = c(bbox["xmin"] - 0.02, bbox["xmax"] + 0.02), 
    ylim = c(bbox["ymin"] - 0.02, bbox["ymax"] + 0.02)) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.background = element_blank(),
    legend.key=element_blank(),
    legend.position = c(0.06, 0.9)
  )
nitrate_map

ggsave("nitrate_map.png", plot = nitrate_map)

# inspect data
unique(waimea_nitrate_data$test_name)
unique(waimea_nitrate_data$year)
unique(waimea_nitrate_data$site)
unique(waimea_nitrate_data$aquifer)
View(waimea_nitrate_data)


# Plot all Nitrate results by test method (violin plot)
p1 <- ggplot(waimea_nitrate_data, aes(x = test_name, y = value, fill = test_name)) +
  geom_violin(alpha = 0.5) +
  geom_hline(yintercept = 11.3, linetype = "dotted", col = "red") +
  annotate("text", x = "Nitrate-N", y = 11.3, label = expression(DWSNZ ~ MAV ~ 11.3 ~ (g / m^{
    3
  })), vjust = -0.25, hjust = -0.5, angle = 0, color = "red", size = 3) +
  scale_y_continuous(expand = c(NA, 1)) +
  labs(x = "", y = expression(NO[3] - N ~ (g / m^{
    3
  })), title = "Nitrate Results by Test Method", fill = "Test name") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p1

ggsave("p1.png", plot = p1)

# Plot all Nitrate results by aquifer (violin plot)
p2 <- ggplot(waimea_nitrate_data, aes(x = aquifer, y = value, fill = aquifer)) +
  geom_violin(alpha = 0.5) +
  geom_hline(yintercept = 11.3, linetype = "dashed", col = "red", size = 1) +
  annotate("text", x = "UCA", y = 11.3, label = expression(DWSNZ ~ MAV ~ 11.3 ~ (g / m^{
    3
  })), vjust = -0.25, hjust = -0.5, angle = 0, color = "red", size = 3) +
  scale_y_continuous(expand = c(NA, 1)) +
  scale_fill_manual(values = aquifer_colors) + 
  labs(x = "", y = expression(NO[3] - N ~ (g / m^{
    3
  })), title = "Nitrate Results by Aquifer", fill = "Aquifer") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p2

ggsave("p2.png", plot = p2)

sites <- waimea_nitrate_data %>%
  group_by(site) %>%
  summarise(n_measurments = n()) %>%
  arrange(desc(n_measurments)) %>%
  head(10) %>% 
  pull(site)
# manual 
sites <- c("GW 37 - Gardner", "GW 471")

waimea_nitrate_data_subset <- waimea_nitrate_data %>% filter(site %in% sites)

# Plot Nitrate results by Site and Aquifer for 10 most sampled sites
p3 <- ggplot(waimea_nitrate_data_subset, aes(x = sample_date, y = value, color = aquifer)) +
  geom_hline(yintercept = 11.3, linetype = "dashed", col = "red", size = 1) +
  geom_point(alpha = 0.5, shape = 4) + 
  geom_smooth(method = "lm", show.legend= FALSE) + 
  #geom_smooth(se = FALSE, linetype = "dotted", show.legend = FALSE) + 
  annotate("text", x = ymd("19800101"), y = 11.3, label = expression(DWSNZ ~ MAV ~ 11.3 ~ (g / m^{
    3
  })), vjust = -0.25, hjust = -0.0, angle = 0, color = "red", size = 3) +
  
  scale_color_manual(values = aquifer_colors) + 
  #scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 30), expand = c(NA, 1)) +
  labs(x = "", y = expression(NO[3] - N ~ (g / m^{
    3
  })), color = "Aquifer") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  facet_wrap(~site, ncol = 1)

p3
ggsave("p3.png", plot = p3, width = 8)

# TODO - trend analysis

mdl <- lm(value ~ sample_date + site, data=waimea_nitrate_data_subset)
summary(mdl)