library(tdcR)
library(tidyverse)
library(sf)
library(lubridate)

##### Nitrate Analysis #####
#
# Matt Ogden, September 2022
#
# Waimea Plains Nitrate Analysis.
#
##################

bore_data <- get_bore_data_envmon()
lab_data <- get_lab_data_envmon()

# Pull only variables of interest
bore_data <- bore_data %>%
  select(bore_id, bore_no, easting, northing, zone_id, bore_depth, site_id, primary_use, aquifer) # take location from the bore

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
    month = month(sample_taken_on)
  ) %>%
  rename(
    value = test_result_numeric,
    unit = test_units,
    sample_date = sample_taken_on
  ) %>%
  st_as_sf(coords = c("easting", "northing"), crs = 2193)

# Save for visualisation in  GIS program
st_write(nitrate_data, "data/nitrate_data.gpkg", "nitrate_observations", append = FALSE)

# Analysis

waimea_nitrate_data <- nitrate_data %>%
  filter(zone_id == 19) # filter to waimea plains

unique(waimea_nitrate_data$name)
unique(waimea_nitrate_data$year)
unique(waimea_nitrate_data$site)

p1 <- ggplot(waimea_nitrate_data, aes(x = name, y = value)) +
  geom_boxplot() + 
  labs(x = "Test method", y = "Nitrate (g/m3)") + 
  theme_bw()

wnd_summary <- waimea_nitrate_data %>% 
  group_by(site) %>% 
  summarise(n_measurments = n()) %>% 
  arrange(desc(n_measurments)) %>% 
  head(10)

p2 <- ggplot(wnd_summary, aes(x = reorder(site, -n_measurments), y = n_measurments, color = site, fill = site)) +
  geom_col(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p3 <- waimea_nitrate_data %>%  
  filter(site %in% wnd_summary$site) %>% 
  ggplot(aes(year, value, color = site)) + 
  geom_line()
