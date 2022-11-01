library(tdcR)
library(tidyverse)
library(sf)
library(lubridate)

##### Nitrate Analysis #####
#
# Matt Ogden, September/October 2022
#
# Waimea Plains Nitrate Analysis - Data Wrangling, compiling "nitrate_data" for subsequent analysis and visualisation.
#
##################

bore_data <- tdcR::get_bore_data_envmon()
lab_data <- tdcR::get_lab_data_envmon()

# Select only variables of interest
bore_data <- bore_data %>%
  select(bore_id, bore_no, easting, northing, zone_id, bore_depth, site_id, primary_use) # take location from the bore, "aquifer" also avaiable

nitrate_data <- lab_data %>%
  filter((test_name_group == "Nutrients" & (test_name %in% c("Nitrate-N", "Total Nitrogen", "Nitrate-N + Nitrite-N"))) | test_name == "Nitrate-N") %>%
  group_by(sample_taken_on, site_id) %>% 
  slice(which.max(test_result_numeric)) %>% # Need to check if this is a valid step.
  ungroup() %>% 
  select(test_name, test_result_numeric, test_units, sample_taken_on, site_id, site) %>%
  left_join(bore_data, by = "site_id") 

rm(lab_data, bore_data)

saveRDS(nitrate_data, "data/nitrate.rds")