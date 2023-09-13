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

library(readxl)

source("trend-analysis/functions.R")

##### SOE GWQ Trend Analysis #####
#
# Matt Ogden, September 2023
#
# State of the Environment Groundwater Quality Trend Analysis
#
##################

trend_var <- "nitrate_n_[g/m3]"

# Some "sites" are combinations of multiple sites, so we merge for analysis and use site_for_analysis as our grouping variable
site_merge_for_analysis <- tribble(
  ~site, ~analysis_site,
  "GW 471", "GW 37 - Gardner"
)

# Load data
gwq_quarterly_soe_data <- read_excel("data/2023-08 GWQ Quarterly SOE_GNS_Sites AllRec.xlsx", sheet = "data") %>% 
  mutate(datetime = ymd_hms(glue("{format(date, format = '%Y/%m/%d')} {format(time, format = '%H:%M:%S')}"), tz = "NZ")) %>% # combine date and time 
  rename(site = site_name) %>% 
  st_as_sf(coords = c("easting", "northing"), crs = 2193)

# Prepare data for trend analysis
analysis_data <- gwq_quarterly_soe_data %>% 
  select("site", "catchment", "datetime", trend_var) %>% 
  rename(value = trend_var) %>% 
  mutate(
    site_for_analysis = if_else(site %in% unique(site_merge_for_analysis$site), filter(site_merge_for_analysis, site == site)$analysis_site, site),
    year_quarter = paste0(year(datetime), "Q", quarter(datetime)),
    trend_var = trend_var,
    value = as.numeric(value)
    ) %>%
  group_by(site_for_analysis, year_quarter) %>% 
  summarise(value = median(value, na.rm = TRUE)) %>% # resample data to quarterly medians
  ungroup() %>%
  mutate( # add the midpoint date of the quarters
    q_date_start = as.Date(as.yearqtr(year_quarter, format = "%YQ%q")),
    q_date_end = as.Date(as.yearqtr(year_quarter, format = "%YQ%q"), frac = 1),
    date = mid_date(q_date_start, q_date_end) # midpoint
  )

trend_analysis <- analysis_data %>%
  group_by(site_for_analysis) %>%
  filter(n() > 4) %>%
  summarise(
    n_records = n(),
    mann_kendall_tau = MannKendall(xts(value, date))$tau,
    mann_kendall_sl = MannKendall(xts(value, date))$sl # get mann-kendall values
  ) %>%
  select(site_for_analysis, n_records, mann_kendall_tau, mann_kendall_sl) %>%
  mutate(
    trend_direction = ifelse(mann_kendall_tau < 0.1, "Decreasing",
                             ifelse(mann_kendall_tau > 0.1, "Increasing", "None")
    ),
    trend_confidence = ifelse(mann_kendall_sl < 0.01, "Very strong",
                              ifelse(mann_kendall_sl < 0.1, "Strong", "Indeterminate")
    )
  )

# save for visualisation in  GIS
st_write(trend_analysis, "trend-analysis/outputs/gwq_gns_trend_results.gpkg", "gwq_gns_observations", append = FALSE)

# produce plots for each site by catchment