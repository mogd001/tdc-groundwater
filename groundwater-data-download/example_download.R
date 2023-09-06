library(tdcR)
library(lubridate)
library(tidyverse)
library(zoo)
library(sf)

sites <- get_sites(collection = "GWcheckDiff", latlong = FALSE, synonyms  = TRUE) %>% 
  mutate(x = easting, y = northing) %>% 
  st_as_sf(coords = c("x", "y"), crs = 2193) %>% 
  st_write("outputs/gw_sites.gpkg", "gw_sites")

# measurements <- get_measurements(site = mcke)
mcke <- "GW 1128 - McKenzie"
mcke_data <- get_data_site_measurement(site = mcke, measurement = "Groundwater Level", method = "Average", interval = "1 day", from = "Data Start", to = "Data End") %>%
  transmute(
    date = as.Date(datetime, tz = "Etc/GMT-12"),
    gw_level = value
  )

write_csv(mcke_data, "outputs/GW 1128 - McKenzie Groundwater Level.csv")

ferg <- "GW 1129 - Ferguson"
ferg_data <- get_data_site_measurement(site = ferg, measurement = "Groundwater Level", method = "Average", interval = "1 day", from = "Data Start", to = "Data End") %>%
  transmute(
    date = as.Date(datetime, tz = "Etc/GMT-12"),
    gw_level = value
  )

dates <- seq(
  from = min(mcke_data$date),
  to = max(mcke_data$date),
  by = "day"
)

mcke_data <- tibble(date = dates) %>%
  left_join(mcke_data) %>%
  drop_na() %>%
  mutate(site = mcke)

ferg_data <- tibble(date = dates) %>%
  left_join(ferg_data) %>%
  drop_na() %>%
  mutate(site = ferg)

combined_data <- bind_rows(mcke_data, ferg_data) %>%
  group_by(site) %>%
  mutate(
    plot_period = factor(if_else(date < as.Date("2016-06-03"), 1,
      if_else(date < as.Date("2018-01-01"), 2, 3)
    )),
    rolling_mean = rollmean(gw_level, k = 100, align = "center", fill = NA)
  ) %>%
  ungroup()

p1 <- ggplot(combined_data, aes(x = date, gw_level, shape = site, color = plot_period)) +
  geom_line() +
  geom_line(aes(y = rolling_mean), color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw()

wide_combined_data <- combined_data %>%
  select(-rolling_mean) %>%
  pivot_wider(names_from = site, values_from = gw_level) %>%
  rename(c(f = !!ferg, m = !!mcke)) %>%
  mutate(
    gw_diff = f - m,
    rolling_mean = rollmean(gw_diff, k = 100, align = "center", fill = NA)
  )

p2 <- ggplot(wide_combined_data, aes(x = date, gw_diff, color = plot_period)) +
  geom_line() +
  geom_line(aes(y = rolling_mean), color = "black") +
  labs(x = "Date", y = "Groundwater Level Difference (m)", title = "Ferguson GWL - McKenzie GWL") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw()
