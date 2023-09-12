library(Microsoft365R)
library(tidyverse)
library(readxl)
library(glue)

site <- get_sharepoint_site(site_name = "Environmental Monitoring") # Environmental Monitoring

site$get_drive()$list_items("")

fname_gwq_soe_gnssites <- "2023-08 GWQ Quarterly SOE_GNS_Sites AllRec.xlsx"
fname_gwq_soe_allsites <- "2023-08 GWQ AllSites 2010-2023.xlsx"

site$get_drive()$download_file(glue("Reports and Analysis/Data Requests/{fname_gwq_soe_gnssites}"), glue("data/{fname_gwq_soe_gnssites}"), overwrite = TRUE)
site$get_drive()$download_file(glue("Reports and Analysis/Data Requests/{fname_gwq_soe_allsites}"), glue("data/{fname_gwq_soe_allsites}"), overwrite = TRUE)

# Use read_excel() to load the file
data <- read_excel(sharepoint_file_url, col_names = TRUE) # Set col_names to TRUE if you want to use the first row as column names
