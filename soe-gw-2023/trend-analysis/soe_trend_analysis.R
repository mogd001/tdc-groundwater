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

trend_var <- "nitrate_n_[g/m3]" # or e_coli_[mpn/100ml]
trend_var_name <- gsub("\\[[^\\]]*\\]", "", trend_var, perl=TRUE) %>% str_sub(start = 1, end = -2) # drop units from trend_var

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
    value = as.numeric(value)
  )

analysis_data_qtly <- analysis_data %>% # use median value per quarter
  group_by(site_for_analysis, year_quarter) %>%
  summarise(value = median(value, na.rm = TRUE)) %>% # resample data to quarterly medians
  ungroup() %>%
  mutate( # add the midpoint date of the quarters
    q_date_start = as.Date(as.yearqtr(year_quarter, format = "%YQ%q")),
    q_date_end = as.Date(as.yearqtr(year_quarter, format = "%YQ%q"), frac = 1),
    date = mid_date(q_date_start, q_date_end) # midpoint
  ) %>%
  mutate(variable = trend_var)

# Perform trend analysis, minimum of 4 samples are required
trend_analysis <- analysis_data_qtly %>%
  group_by(site_for_analysis) %>%
  filter(n() > 4) %>%
  summarise(
    variable = trend_var,
    n_records = n(),
    mann_kendall_tau = MannKendall(xts(value, date))$tau,
    mann_kendall_sl = MannKendall(xts(value, date))$sl # get mann-kendall values
  ) %>%
  select(site_for_analysis, variable, n_records, mann_kendall_tau, mann_kendall_sl) %>%
  mutate(
    trend_direction = if_else(mann_kendall_tau < 0.1, "Decreasing",
      ifelse(mann_kendall_tau > 0.1, "Increasing", "None")
    ),
    trend_confidence = if_else(mann_kendall_sl < 0.01, "Very strong",
      ifelse(mann_kendall_sl < 0.1, "Strong", "Indeterminate")
    )
  )

# Save for visualisation in GIS
st_write(trend_analysis, "trend-analysis/outputs/gwq_gns_trend_results.gpkg", "gwq_gns_observations", append = FALSE)
write_csv(trend_analysis, "trend-analysis/outputs/gwq_gns_trend_results.csv")

# Produce plots for each site
plot_trend_by_site <- function(site, analysis_data, analysis_data_qtly) {
  site_data <- filter(analysis_data, site_for_analysis == !!site)
  site_data_qtly <- filter(analysis_data_qtly, site_for_analysis == !!site)

  # Plot Nitrate results by Site and Aquifer for 10 most sampled sites
  plot_site <- ggplot() +
    geom_point(data = site_data, aes(x = as.Date(datetime), y = value, color = "Actual measurement"), alpha = 0.5, size = 1, shape = 1) +
    geom_point(data = site_data_qtly, aes(x = date, y = value, color = "Quarterly resample"), size = 3, shape = 4) +
    geom_smooth(data = site_data_qtly, aes(x = date, y = value), method = "lm", size = 0.5, color = "black") +
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

  ggsave(glue("trend-analysis/outputs/plots/{site}_trend_plot.png"), plot = plot_site, width = 10, height = 7)
}

sites <- unique(trend_analysis$site_for_analysis)
lapply(sites, plot_trend_by_site, analysis_data, analysis_data_qtly)


# To be completed...

# Paginated plot for all sites, by catchment
summary_plot_data <- analysis_data %>% 
  filter(!is.na(catchment) & !is.na(site_for_analysis)) #%>% 
  #arrange(as.numeric(bore_no)) # if required to arrange the plots by bore_no

for (catchment in unique(summary_plot_data$catchment)) {
  
  catchment_summary_plot_data <- summary_plot_data %>% 
    filter(catchment == !!catchment) %>% 
    mutate(sample_date = date(datetime))

  gg <- ggplot(catchment_summary_plot_data) +
    geom_point(aes(sample_date, value), size = 1.5, alpha = 0.7) +
    geom_hline(yintercept = 11.3, linetype = "dashed", color = "red", size = 0.5) +
    annotate("text", x = ymd("19700101"), y = 11.3, label = expression(DWSNZ ~ MAV ~ 11.3 ~ (g / m^{3})), vjust = -0.25, hjust = -0.1, angle = 0, color = "red", size = 2) +
    scale_x_date(limits = c(ymd("19700101"), ymd("20250101")), expand = c(0,0), date_labels = "%Y", date_breaks = "10 years") +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "Sample date", y = expression(NO[3] - N ~ (g / m^{3})), title = glue("Catchment {catchment}")) + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0), axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    facet_wrap_paginate(~site_for_analysis, ncol = 2, nrow = 5, page = 1)
  
  n <- n_pages(gg)
  
  pdf(glue("trend-analysis/outputs/{catchment}_{trend_var_name}__Summary.pdf"), paper= 'A4', w= 210/20, 297/20)
  for(i in 1:n){
    print(gg + facet_wrap_paginate(~site_for_analysis, labeller = label_parsed, ncol = 2, nrow = 5, page = i))
  }
  dev.off()
}